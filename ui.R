library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("AS-C with Delays v1.2"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(

      # Slider for dt_x
      sliderInput("dt_x",
                  "dt_x",
                  min = 0.0,
                  max = 2.0,
                  value = 0,
                  step = 0.01)
      ,

      # Slider for dt_y
      sliderInput("dt_y",
                  "dt_y",
                  min = 0.0,
                  max = 2.0,
                  value = 0,
                  step = 0.01)
      ,

      # Slider for dt_z
      sliderInput("dt_z",
                  "dt_z",
                  min = 0.0,
                  max = 2.0,
                  value = 0,
                  step = 0.01)
      ,

      # Slider for dt_u
      sliderInput("dt_u",
                  "dt_u",
                  min = 0.0,
                  max = 2.0,
                  value = 0,
                  step = 0.01)
      ,

      # Slider for dt_w
      sliderInput("dt_w",
                  "dt_w",
                  min = 0.0,
                  max = 2.0,
                  value = 0,
                  step = 0.01)
      ,

      # Slider for dt_p
      sliderInput("dt_p",
                  "dt_p",
                  min = 0.0,
                  max = 2.0,
                  value = 0,
                  step = 0.01)
      ,

      # Slider for Gro
      sliderInput("Gro",
                  "Gro",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for EMC
      sliderInput("EMC",
                  "EMC",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for D
      sliderInput("D",
                  "D",
                  min = 0.0,
                  max = 15.0,
                  value = 2.05,
                  step = 0.01)
      ,

      # Slider for UB
      sliderInput("UB",
                  "UB",
                  min = 0.0,
                  max = 15.0,
                  value = 1.17,
                  step = 0.01)
      ,

      # Slider for SINA
      sliderInput("SINA",
                  "SINA",
                  min = 0.0,
                  max = 15.0,
                  value = 5.6,
                  step = 0.01)
      ,

      # Slider for C
      sliderInput("C",
                  "C",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for n
      sliderInput("n",
                  "n",
                  min = 0.0,
                  max = 15.0,
                  value = 2,
                  step = 0.01)
      ,

      # Slider for m1
      sliderInput("m1",
                  "m1",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for m2
      sliderInput("m2",
                  "m2",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for m3
      sliderInput("m3",
                  "m3",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for m4
      sliderInput("m4",
                  "m4",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for m5
      sliderInput("m5",
                  "m5",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for m6
      sliderInput("m6",
                  "m6",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for m7
      sliderInput("m7",
                  "m7",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for kx
      sliderInput("kx",
                  "kx",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for ky
      sliderInput("ky",
                  "ky",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for kz
      sliderInput("kz",
                  "kz",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for ku
      sliderInput("ku",
                  "ku",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for kw
      sliderInput("kw",
                  "kw",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for kp
      sliderInput("kp",
                  "kp",
                  min = 0.0,
                  max = 15.0,
                  value = 1,
                  step = 0.01)
      ,

      # Slider for a3
      sliderInput("a3",
                  "a3",
                  min = 0.0,
                  max = 15.0,
                  value = 3.61,
                  step = 0.01)
      ,

      # Slider for b3
      sliderInput("b3",
                  "b3",
                  min = 0.0,
                  max = 15.0,
                  value = 4.96,
                  step = 0.01)
      ,

      # Slider for c3
      sliderInput("c3",
                  "c3",
                  min = 0.0,
                  max = 15.0,
                  value = 1.35,
                  step = 0.01)
      ,

      # Slider for a4
      sliderInput("a4",
                  "a4",
                  min = 0.0,
                  max = 15.0,
                  value = 4.43,
                  step = 0.01)
      ,

      # Slider for b4
      sliderInput("b4",
                  "b4",
                  min = 0.0,
                  max = 15.0,
                  value = 6.09,
                  step = 0.01)
      ,

      # Slider for c4
      sliderInput("c4",
                  "c4",
                  min = 0.0,
                  max = 15.0,
                  value = 1.66,
                  step = 0.01)
      ,

      # Slider for a5
      sliderInput("a5",
                  "a5",
                  min = 0.0,
                  max = 15.0,
                  value = 8.09,
                  step = 0.01)
      ,

      # Slider for b5
      sliderInput("b5",
                  "b5",
                  min = 0.0,
                  max = 15.0,
                  value = 11.13,
                  step = 0.01)
      ,

      # Slider for c5
      sliderInput("c5",
                  "c5",
                  min = 0.0,
                  max = 15.0,
                  value = 3.03,
                  step = 0.01)
      ,

      # Slider for a6
      sliderInput("a6",
                  "a6",
                  min = 0.0,
                  max = 15.0,
                  value = 2.67,
                  step = 0.01)
      ,

      # Slider for b6
      sliderInput("b6",
                  "b6",
                  min = 0.0,
                  max = 15.0,
                  value = 3.67,
                  step = 0.01)
      ,

      # Slider for c6
      sliderInput("c6",
                  "c6",
                  min = 0.0,
                  max = 15.0,
                  value = 1.00,
                  step = 0.01)
      ,

      # Slider for d1
      sliderInput("d1",
                  "d1",
                  min = 0.0,
                  max = 15.0,
                  value = 7.46,
                  step = 0.01)
      ,

      # Slider for d3
      sliderInput("d3",
                  "d3",
                  min = 0.0,
                  max = 15.0,
                  value = 2.77,
                  step = 0.01)
      ,

      # Slider for d5
      sliderInput("d5",
                  "d5",
                  min = 0.0,
                  max = 15.0,
                  value = 1.24,
                  step = 0.01)
      ,

      # Slider for y0
      sliderInput("y0",
                  "y0",
                  min = 0.0,
                  max = 15.0,
                  value = 3,
                  step = 0.01)
      ,

      # Slider for x0
      sliderInput("x0",
                  "x0",
                  min = 0.0,
                  max = 15.0,
                  value = 3 / 20,
                  step = 0.01)
      ,

      # Slider for z0
      sliderInput("z0",
                  "z0",
                  min = 0.0,
                  max = 15.0,
                  value = 3 / 20,
                  step = 0.01)
      ,

      # Slider for u0
      sliderInput("u0",
                  "u0",
                  min = 0.0,
                  max = 15.0,
                  value = 0,
                  step = 0.01)
      ,

      # Slider for w0
      sliderInput("w0",
                  "w0",
                  min = 0.0,
                  max = 15.0,
                  value = 0,
                  step = 0.01)
      ,

      # Slider for p0
      sliderInput("p0",
                  "p0",
                  min = 0.0,
                  max = 15.0,
                  value = 0,
                  step = 0.01)

    ),

    mainPanel(
      plotOutput("diff_eqs"),
      plotOutput("aux_panel"),
      htmlOutput("formulas_panel")
    )
  )
))

