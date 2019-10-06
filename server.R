library(shiny)
library(ggplot2)
library(deSolve)


'%=%' <- function(x, y) {
  x <- as.character(substitute(x)[-1])
  if (length(y) < length(x))
    y <- rep(y, ceiling(length(x) / length(y)))
  if (length(y) > length(x))
    y <- y[1:length(x)]
  mapply(assign, x, y, MoreArgs = list(envir = parent.frame()))
  invisible()
}

h <- function(v) v > 0

shinyServer(function(input, output) {

  # Rendering for diff_eqs
  output$diff_eqs <- renderPlot({
    dt_x <- input$dt_x
    dt_y <- input$dt_y
    dt_z <- input$dt_z
    dt_u <- input$dt_u
    dt_w <- input$dt_w
    dt_p <- input$dt_p
    Gro <- input$Gro
    EMC <- input$EMC
    D <- input$D
    UB <- input$UB
    SINA <- input$SINA
    C <- input$C
    n <- input$n
    m1 <- input$m1
    m2 <- input$m2
    m3 <- input$m3
    m4 <- input$m4
    m5 <- input$m5
    m6 <- input$m6
    kx <- input$kx
    ky <- input$ky
    kz <- input$kz
    ku <- input$ku
    kw <- input$kw
    kp <- input$kp
    a3 <- input$a3
    b3 <- input$b3
    c3 <- input$c3
    a4 <- input$a4
    b4 <- input$b4
    c4 <- input$c4
    a5 <- input$a5
    b5 <- input$b5
    c5 <- input$c5
    a6 <- input$a6
    b6 <- input$b6
    c6 <- input$c6
    d1 <- input$d1
    d3 <- input$d3
    d5 <- input$d5
    y0 <- input$y0
    x0 <- input$x0
    z0 <- input$z0
    u0 <- input$u0
    w0 <- input$w0
    p0 <- input$p0
    sigma1 <- function (v) {
      h(v) * d1 * v ^ n / (1 + v ^ n)
    }
    sigma3 <- function (v) {
      h(v) * d3 * v ^ n / (1 + v ^ n)
    }
    sigma5 <- function (v) {
      h(v) * d5 * v ^ n / (1 + v ^ n)
    }
    s3 <- function (v) {
      h(v) * a3 * exp((v - b3) / c3) / (1 + exp((v - b3) / c3))
    }
    s4 <- function (v) {
      h(v) * a4 * exp((v - b4) / c4) / (1 + exp((v - b4) / c4))
    }
    s5 <- function (v) {
      h(v) * a5 * exp((v - b5) / c5) / (1 + exp((v - b5) / c5))
    }
    s6 <- function (v) {
      h(v) * a6 * exp((v - b6) / c6) / (1 + exp((v - b6) / c6))
    }


    # Helper for x'
    x_helper <- function(t, lagPoint, currentPoint = lagPoint) {
      c(x, y, z, u, w, p) %=% lagPoint
      c(x_curr, y_curr, z_curr, u_curr, w_curr, p_curr) %=% currentPoint
      kx * (sigma1(D * x) + sigma3(z) + sigma5(w)) / ((1 + Gro * y) * (1 + EMC * x)) - m1 * x * (1 + p_curr * UB * SINA)
    }

    # Modelling for x'
    x_model <- function(t, dt, startValue, point, args, ...) {
      if (t < 0) { startValue }
      else if (t < dt || dt < 0.01) {
        lagPoint <- x_helper(t, point)
      } else {
        lagPoint <- lagvalue(t - dt)
        x_helper(t, lagPoint, point)
      }
    }

    # Helper for y'
    y_helper <- function(t, lagPoint, currentPoint = lagPoint) {
      c(x, y, z, u, w, p) %=% lagPoint
      c(x_curr, y_curr, z_curr, u_curr, w_curr, p_curr) %=% currentPoint
      ky * C / (1 + u) - m2 * y_curr
    }

    # Modelling for y'
    y_model <- function(t, dt, startValue, point, args, ...) {
      if (t < 0) { startValue }
      else if (t < dt || dt < 0.01) {
        lagPoint <- y_helper(t, point)
      } else {
        lagPoint <- lagvalue(t - dt)
        y_helper(t, lagPoint, point)
      }
    }

    # Helper for z'
    z_helper <- function(t, lagPoint, currentPoint = lagPoint) {
      c(x, y, z, u, w, p) %=% lagPoint
      c(x_curr, y_curr, z_curr, u_curr, w_curr, p_curr) %=% currentPoint
      kz * s3(D * x) - m3 * z_curr
    }

    # Modelling for z'
    z_model <- function(t, dt, startValue, point, args, ...) {
      if (t < 0) { startValue }
      else if (t < dt || dt < 0.01) {
        lagPoint <- z_helper(t, point)
      } else {
        lagPoint <- lagvalue(t - dt)
        z_helper(t, lagPoint, point)
      }
    }

    # Helper for u'
    u_helper <- function(t, lagPoint, currentPoint = lagPoint) {
      c(x, y, z, u, w, p) %=% lagPoint
      c(x_curr, y_curr, z_curr, u_curr, w_curr, p_curr) %=% currentPoint
      ku * s4(D * x) - m4 * u_curr
    }

    # Modelling for u'
    u_model <- function(t, dt, startValue, point, args, ...) {
      if (t < 0) { startValue }
      else if (t < dt || dt < 0.01) {
        lagPoint <- u_helper(t, point)
      } else {
        lagPoint <- lagvalue(t - dt)
        u_helper(t, lagPoint, point)
      }
    }

    # Helper for w'
    w_helper <- function(t, lagPoint, currentPoint = lagPoint) {
      c(x, y, z, u, w, p) %=% lagPoint
      c(x_curr, y_curr, z_curr, u_curr, w_curr, p_curr) %=% currentPoint
      kw * s5(D * x) - m5 * w_curr
    }

    # Modelling for w'
    w_model <- function(t, dt, startValue, point, args, ...) {
      if (t < 0) { startValue }
      else if (t < dt || dt < 0.01) {
        lagPoint <- w_helper(t, point)
      } else {
        lagPoint <- lagvalue(t - dt)
        w_helper(t, lagPoint, point)
      }
    }

    # Helper for p'
    p_helper <- function(t, lagPoint, currentPoint = lagPoint) {
      c(x, y, z, u, w, p) %=% lagPoint
      c(x_curr, y_curr, z_curr, u_curr, w_curr, p_curr) %=% currentPoint
      h(t - 12) * (kp * s6(D * x) * (t - 12) ^ 2 / (1 + (t - 12) ^ 2) - m6 * p_curr)
    }

    # Modelling for p'
    p_model <- function(t, dt, startValue, point, args, ...) {
      if (t < 0) { startValue }
      else if (t < dt || dt < 0.01) {
        lagPoint <- p_helper(t, point)
      } else {
        lagPoint <- lagvalue(t - dt)
        p_helper(t, lagPoint, point)
      }
    }

    # Modelling stuff
    model <- function(t, p, parms, ...) {
      list(c(x_model(t, dt_x, x0, p), y_model(t, dt_y, y0, p), z_model(t, dt_z, z0, p), u_model(t, dt_u, u0, p), w_model(t, dt_w, w0, p), p_model(t, dt_p, p0, p)))
    }
    t <- seq(0, 16, by = 0.1)
    start <- c(x0 * kx, y0 * ky, z0 * kz, u0 * ku, w0 * kw, p0 * kp)
    traj <- as.data.frame(dede(start, t, func = model))

    plot_x <- traj[, 2]
    plot_y <- traj[, 3]
    plot_z <- traj[, 4]
    plot_u <- traj[, 5]
    plot_w <- traj[, 6]
    plot_p <- traj[, 7]

    df <- data.frame(t, plot_x, plot_y, plot_z, plot_u, plot_w, plot_p)

    ggplot(df, aes(x = t , y = Value, color = Concentration)) +
           geom_line(aes(y = plot_x, col = "AS-C: x(t)")) +
           geom_line(aes(y = plot_y, col = "Hairy: y(t)")) +
           geom_line(aes(y = plot_z, col = "Senseless: z(t)")) +
           geom_line(aes(y = plot_u, col = "Scratch: u(t)")) +
           geom_line(aes(y = plot_w, col = "Charlatan: w(t)")) +
           geom_line(aes(y = plot_p, col = "Phyllopod: p(t)"))
  })

  # Rendering for aux_panel
  output$aux_panel <- renderPlot({
    dt_x <- input$dt_x
    dt_y <- input$dt_y
    dt_z <- input$dt_z
    dt_u <- input$dt_u
    dt_w <- input$dt_w
    dt_p <- input$dt_p
    Gro <- input$Gro
    EMC <- input$EMC
    D <- input$D
    UB <- input$UB
    SINA <- input$SINA
    C <- input$C
    n <- input$n
    m1 <- input$m1
    m2 <- input$m2
    m3 <- input$m3
    m4 <- input$m4
    m5 <- input$m5
    m6 <- input$m6
    kx <- input$kx
    ky <- input$ky
    kz <- input$kz
    ku <- input$ku
    kw <- input$kw
    kp <- input$kp
    a3 <- input$a3
    b3 <- input$b3
    c3 <- input$c3
    a4 <- input$a4
    b4 <- input$b4
    c4 <- input$c4
    a5 <- input$a5
    b5 <- input$b5
    c5 <- input$c5
    a6 <- input$a6
    b6 <- input$b6
    c6 <- input$c6
    d1 <- input$d1
    d3 <- input$d3
    d5 <- input$d5
    y0 <- input$y0
    x0 <- input$x0
    z0 <- input$z0
    u0 <- input$u0
    w0 <- input$w0
    p0 <- input$p0
    sigma1 <- function (v) {
      h(v) * d1 * v ^ n / (1 + v ^ n)
    }
    sigma3 <- function (v) {
      h(v) * d3 * v ^ n / (1 + v ^ n)
    }
    sigma5 <- function (v) {
      h(v) * d5 * v ^ n / (1 + v ^ n)
    }
    s3 <- function (v) {
      h(v) * a3 * exp((v - b3) / c3) / (1 + exp((v - b3) / c3))
    }
    s4 <- function (v) {
      h(v) * a4 * exp((v - b4) / c4) / (1 + exp((v - b4) / c4))
    }
    s5 <- function (v) {
      h(v) * a5 * exp((v - b5) / c5) / (1 + exp((v - b5) / c5))
    }
    s6 <- function (v) {
      h(v) * a6 * exp((v - b6) / c6) / (1 + exp((v - b6) / c6))
    }

    v <- seq(0.0, 10.0, by = 0.01)

    plot_sigma1 <- sigma1(v)
    plot_sigma3 <- sigma3(v)
    plot_sigma5 <- sigma5(v)
    plot_s3 <- s3(v)
    plot_s4 <- s4(v)
    plot_s5 <- s5(v)
    plot_s6 <- s6(v)

    df <- data.frame(v, plot_sigma1, plot_sigma3, plot_sigma5, plot_s3, plot_s4, plot_s5, plot_s6)

    ggplot(df, aes(x = v , y = Value, color = Function)) +
           geom_line(aes(y = plot_sigma1, col = "sigma1(v)")) +
           geom_line(aes(y = plot_sigma3, col = "sigma3(v)")) +
           geom_line(aes(y = plot_sigma5, col = "sigma5(v)")) +
           geom_line(aes(y = plot_s3, col = "s3(v)")) +
           geom_line(aes(y = plot_s4, col = "s4(v)")) +
           geom_line(aes(y = plot_s5, col = "s5(v)")) +
           geom_line(aes(y = plot_s6, col = "s6(v)"))
  })

  # Rendering for formulas_panel
  output$formulas_panel <- renderUI({
    dt_x <- input$dt_x
    dt_y <- input$dt_y
    dt_z <- input$dt_z
    dt_u <- input$dt_u
    dt_w <- input$dt_w
    dt_p <- input$dt_p
    Gro <- input$Gro
    EMC <- input$EMC
    D <- input$D
    UB <- input$UB
    SINA <- input$SINA
    C <- input$C
    n <- input$n
    m1 <- input$m1
    m2 <- input$m2
    m3 <- input$m3
    m4 <- input$m4
    m5 <- input$m5
    m6 <- input$m6
    kx <- input$kx
    ky <- input$ky
    kz <- input$kz
    ku <- input$ku
    kw <- input$kw
    kp <- input$kp
    a3 <- input$a3
    b3 <- input$b3
    c3 <- input$c3
    a4 <- input$a4
    b4 <- input$b4
    c4 <- input$c4
    a5 <- input$a5
    b5 <- input$b5
    c5 <- input$c5
    a6 <- input$a6
    b6 <- input$b6
    c6 <- input$c6
    d1 <- input$d1
    d3 <- input$d3
    d5 <- input$d5
    y0 <- input$y0
    x0 <- input$x0
    z0 <- input$z0
    u0 <- input$u0
    w0 <- input$w0
    p0 <- input$p0
    text <- paste0(
      "--------------------------------------------------------------------------------",
      "<br>",
      "Differential equations:",
      "<br>",
      "x' = kx * (sigma1(D * x) + sigma3(z) + sigma5(w)) / ((1 + Gro * y) * (1 + EMC * x)) - m1 * x * (1 + p_curr * UB * SINA)",
      "<br>",
      "y' = ky * C / (1 + u) - m2 * y_curr",
      "<br>",
      "z' = kz * s3(D * x) - m3 * z_curr",
      "<br>",
      "u' = ku * s4(D * x) - m4 * u_curr",
      "<br>",
      "w' = kw * s5(D * x) - m5 * w_curr",
      "<br>",
      "p' = h(t - 12) * (kp * s6(D * x) * (t - 12) ^ 2 / (1 + (t - 12) ^ 2) - m6 * p_curr)",
      "<br>",
      "--------------------------------------------------------------------------------",
      "<br>",
      "Aux functions:",
      "<br>",
      "sigma1 ( v )  = h(v) * d1 * v ^ n / (1 + v ^ n)",
      "<br>",
      "sigma3 ( v )  = h(v) * d3 * v ^ n / (1 + v ^ n)",
      "<br>",
      "sigma5 ( v )  = h(v) * d5 * v ^ n / (1 + v ^ n)",
      "<br>",
      "s3 ( v )  = h(v) * a3 * exp((v - b3) / c3) / (1 + exp((v - b3) / c3))",
      "<br>",
      "s4 ( v )  = h(v) * a4 * exp((v - b4) / c4) / (1 + exp((v - b4) / c4))",
      "<br>",
      "s5 ( v )  = h(v) * a5 * exp((v - b5) / c5) / (1 + exp((v - b5) / c5))",
      "<br>",
      "s6 ( v )  = h(v) * a6 * exp((v - b6) / c6) / (1 + exp((v - b6) / c6))",
      "<br>",
      "--------------------------------------------------------------------------------",
      "<br>",
      "Parameters:",
      "<br>",
      paste0("dt_x = ", dt_x),
      "<br>",
      paste0("dt_y = ", dt_y),
      "<br>",
      paste0("dt_z = ", dt_z),
      "<br>",
      paste0("dt_u = ", dt_u),
      "<br>",
      paste0("dt_w = ", dt_w),
      "<br>",
      paste0("dt_p = ", dt_p),
      "<br>",
      paste0("Gro = ", Gro),
      "<br>",
      paste0("EMC = ", EMC),
      "<br>",
      paste0("D = ", D),
      "<br>",
      paste0("UB = ", UB),
      "<br>",
      paste0("SINA = ", SINA),
      "<br>",
      paste0("C = ", C),
      "<br>",
      paste0("n = ", n),
      "<br>",
      paste0("m1 = ", m1),
      "<br>",
      paste0("m2 = ", m2),
      "<br>",
      paste0("m3 = ", m3),
      "<br>",
      paste0("m4 = ", m4),
      "<br>",
      paste0("m5 = ", m5),
      "<br>",
      paste0("m6 = ", m6),
      "<br>",
      paste0("kx = ", kx),
      "<br>",
      paste0("ky = ", ky),
      "<br>",
      paste0("kz = ", kz),
      "<br>",
      paste0("ku = ", ku),
      "<br>",
      paste0("kw = ", kw),
      "<br>",
      paste0("kp = ", kp),
      "<br>",
      paste0("a3 = ", a3),
      "<br>",
      paste0("b3 = ", b3),
      "<br>",
      paste0("c3 = ", c3),
      "<br>",
      paste0("a4 = ", a4),
      "<br>",
      paste0("b4 = ", b4),
      "<br>",
      paste0("c4 = ", c4),
      "<br>",
      paste0("a5 = ", a5),
      "<br>",
      paste0("b5 = ", b5),
      "<br>",
      paste0("c5 = ", c5),
      "<br>",
      paste0("a6 = ", a6),
      "<br>",
      paste0("b6 = ", b6),
      "<br>",
      paste0("c6 = ", c6),
      "<br>",
      paste0("d1 = ", d1),
      "<br>",
      paste0("d3 = ", d3),
      "<br>",
      paste0("d5 = ", d5),
      "<br>",
      paste0("y0 = ", y0),
      "<br>",
      paste0("x0 = ", x0),
      "<br>",
      paste0("z0 = ", z0),
      "<br>",
      paste0("u0 = ", u0),
      "<br>",
      paste0("w0 = ", w0),
      "<br>",
      paste0("p0 = ", p0),
      "<br>",
      "--------------------------------------------------------------------------------"
    )
    HTML(text)
  })
})

