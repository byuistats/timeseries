# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("ggokabeito")) install.packages("ggokabeito")
if (!require("ggplot2")) install.packages("ggplot2")

library(shiny)
library(tidyverse)
library(dplyr)
library(ggokabeito)
library(fpp3)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)

### UI  ####
ui <- fluidPage(
  #### Code and tags for LATEX and Dynamic UI ####
  tags$head(
    tags$link(rel="stylesheet",
              href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css",
              integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
              crossorigin="anonymous"),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
    HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
  ),
  useShinyjs(),
  withMathJax(),
  tags$head(
    tags$style(HTML(".nav-pills {display: flex; justify-content: center;}\n.nav-pills > li > a {padding: 8px 120px; font-size: 16px; }"))
  ),
  #### Header ####
  titlePanel("Fitted Models"),
  fluidRow(
    column(5, offset = 2, sliderInput("n_points", "Number of Points", min = 0, max = 2000, value = 500, step = 50)),
    column(5, sliderInput("sigma", "Standard Deviation", min = 0, max = 10, value = 1, step = 0.1))
    ),
  fluidRow(
    column(12, h4(""))
  ),
  tabsetPanel(
    id = "main",
    type = "pills",
    #### AR1 Tab ####
    tabPanel(
      title = "AR(1)",
      fluidRow(
        column(3, offset = 1, numericInput("AR1_alpha_1", "$ \\alpha_1 $", min = -500, max = 500, value = 0.7, step = 0.02)), ######### WHY IS THIS 500?????
      ),
      fluidRow(
        column(12,  div(style = "display: flex; justify-content: center; align-items: center;", uiOutput("formula1"))),
        column(4, offset = 5, actionButton("go1", "Simulate!")),
        column(12, conditionalPanel(
          condition = "input.go1",
          div(id = "outputs1",
              fluidRow(
                column(12, h4("Series")),
                column(12, plotOutput("plot1")),
                column(12, h4("ACF of Simulated AR(1) Process")),
                column(12, plotOutput("acf_plot1", height = "250px")),
                column(12, h4("Partial ACF of Simulated AR(1) Process")),
                column(12, plotOutput("pacf_plot1", height = "250px"))
              )
          ))
        ),
        column(12, conditionalPanel(
          condition = "input.n_points == 0",
          div(id = "error1",
              fluidRow(
                column(12, h4("Invalid Number of Points"))
              )
          )
        ))
      )
    ),
    #### AR2 tab ####
    tabPanel(
      title = "AR(2)",
      fluidRow(
        column(3, offset = 1, numericInput("AR2_alpha_1", "$ \\alpha_1 $", min = -500, max = 500, value = 0.5, step = 0.02)),
        column(3, numericInput("AR2_alpha_2", "$ \\alpha_2 $", min = -500, max = 500, value = 0.5, step = 0.02))
      ),
      fluidRow(
        column(12,  div(style = "display: flex; justify-content: center; align-items: center;", uiOutput("formula2"))),
        column(4, offset = 5, actionButton("go2", "Simulate!")),
        column(12, conditionalPanel(
          condition = "input.go2",
          div(id = "outputs2",
              fluidRow(
                column(12, h4("Series")),
                column(12, plotOutput("plot2")),
                column(12, h4("ACF of Simulated AR(2) Process")),
                column(12, plotOutput("acf_plot2", height = "250px")),
                column(12, h4("Partial ACF of Simulated AR(2) Process")),
                column(12, plotOutput("pacf_plot2", height = "250px"))
              )
          ))
        ),
        column(12, conditionalPanel(
          condition = "input.n_points == 0",
          div(id = "error2",
              fluidRow(
                column(12, h4("Invalid Number of Points"))
              )
          )
        ))
      )
    ),
    #### AR3 tab ####
    tabPanel(
      title = "AR(3)",
      fluidRow(
        column(3, offset = 1, numericInput("AR3_alpha_1", "$ \\alpha_1 $", min = -500, max = 500, value = 0.25, step = 0.02)),
        column(3, numericInput("AR3_alpha_2", "$ \\alpha_2 $", min = -500, max = 500, value = 0.2, step = 0.02)),
        column(3, numericInput("AR3_alpha_3", "$ \\alpha_3 $", min = -500, max = 500, value = 0.15, step = 0.02))
      ),
      fluidRow(
        column(12,  div(style = "display: flex; justify-content: center; align-items: center;", uiOutput("formula3"))),
        column(4, offset = 5, actionButton("go3", "Simulate!")),
        column(12, conditionalPanel(
          condition = "input.go3",
          div(id = "outputs3",
              fluidRow(
                column(12, h4("Series")),
                column(12, plotOutput("plot3")),
                column(12, h4("ACF of Simulated AR(3) Process")),
                column(12, plotOutput("acf_plot3", height = "250px")),
                column(12, h4("Partial ACF of Simulated AR(3) Process")),
                column(12, plotOutput("pacf_plot3", height = "250px"))
              )
          ))
        ),
        column(12, conditionalPanel(
          condition = "input.n_points == 0",
          div(id = "error3",
              fluidRow(
                column(12, h4("Invalid Number of Points"))
              )
          )
        ))
      )
    )
  )
)

### Server ####
server <- function(input, output, session) {

  #### Hide/Show logic for UI ####
  # Observe for button clicks in each tab
  observeEvent(input$go1, {
    show("outputs1")
    hide("error1")
  })

  observeEvent(input$go2, {
    show("outputs2")
    hide("error2")
  })

  observeEvent(input$go3, {
    show("outputs3")
    hide("error3")
  })

  # Observe for invalid n_points
  observe({
    if (input$n_points == 0) {
      show("error1")
      hide("button1")
      hide("outputs1")
      show("error2")
      hide("button2")
      hide("outputs2")
      show("error3")
      hide("button3")
      hide("outputs3")
    } else {
      show("button1")
      hide("error1")
      show("button2")
      hide("error2")
      show("button3")
      hide("error3")
    }
  })

  # # old code from before i made each alpha from every tab a unique id, tried for a while to get shiny to let me overwrite them from tab to tab but didnt succeed. maybe need to use the isolate() but haven't used it before and don't fully understand the implications
  # observe({ #needed this before I moved all the inputs into the indivdual tabs, this is code for updated inputs in the header.... but its still needed? because 'redefining' the input for alpha1/2 doesn't seem to overwrite the previously defined default value...
  #   selected_tab <- input$main
  #
  #   if (selected_tab == "AR (1)") {# Assuming "tab1" is the ID of the first tab
  #     a1 <- 0.7
  #     a2 <- 0
  #     a3 <- 0
  #   } else if (selected_tab == "AR (2)") {
  #     a1 <- 0.5
  #     a2 <- 0.5
  #     a3 <- 0
  #   } else {
  #     a1 <- 0.25
  #     a2 <- 0.2
  #     a3 <- 0.15
  #   }
  #
  #   updateNumericInput(session, "alpha_1", value = a1)
  #   updateNumericInput(session, "alpha_2", value = a2)
  #   updateNumericInput(session, "alpha_3", value = a3)
  # })

  #### AR1 sim ####
  sim_data1 <- eventReactive(input$go1, {
    # AR(1) process
    n_days <- input$n_points
    x <- w <- rnorm(n_days, sd = input$sigma)
    start_date <- my(paste(1, floor(year(now())-n_days/365)))
    date_seq <- seq(start_date,
                    start_date + days(n_days - 1),
                    by = "1 days")

    for (t in 2:n_days) {
      x[t] <- input$AR1_alpha_1 * x[t-1] + w[t]
    }

    sim_ts <- data.frame(dates = date_seq, x=x) |>
      as_tsibble(index = dates)

    # ACF plot data
    acf_plot <- sim_ts |> ACF(x) |> autoplot()

    pacf_plot <- sim_ts |> PACF(x) |> autoplot()

    list(sim_data = sim_ts, acf_plot = acf_plot, pacf_plot = pacf_plot)
  })
  #### AR2 sim ####
  sim_data2 <- eventReactive(input$go2, {
    # AR(2) process
    n_days <- input$n_points
    x <- w <- rnorm(n_days, sd = input$sigma)
    start_date <- my(paste(1, floor(year(now())-n_days/365)))
    date_seq <- seq(start_date,
                    start_date + days(n_days - 1),
                    by = "1 days")

    x[2] <- input$AR2_alpha_1 * x[1] + w[2]
    for (t in 3:n_days) {
      x[t] <- input$AR2_alpha_1 * x[t-1] + input$AR2_alpha_2 * x[t-2] + w[t]
    }

    sim_ts <- data.frame(dates = date_seq, x=x) |>
      as_tsibble(index = dates)

    # ACF plot data
    acf_plot <- sim_ts |> ACF(x) |> autoplot()

    pacf_plot <- sim_ts |> PACF(x) |> autoplot()

    list(sim_data = sim_ts, acf_plot = acf_plot, pacf_plot = pacf_plot)
  })

  #### AR3 sim ####
  sim_data3 <- eventReactive(input$go3, {
    # AR(3) process
    n_days <- input$n_points
    x <- w <- rnorm(n_days, sd = input$sigma)
    start_date <- my(paste(1, floor(year(now())-n_days/365)))
    date_seq <- seq(start_date,
                    start_date + days(n_days - 1),
                    by = "1 days")

    x[2] <- input$AR3_alpha_1 * x[1] + w[2]
    x[3] <- input$AR3_alpha_1 * x[2] + input$AR3_alpha_2 * x[1] + w[3]
    for (t in 4:n_days) {
      x[t] <- input$AR3_alpha_1 * x[t-1] + input$AR3_alpha_2 * x[t-2] + input$AR3_alpha_3 * x[t-3] + w[t]
    }

    sim_ts <- data.frame(dates = date_seq, x=x) |>
      as_tsibble(index = dates)

    # ACF plot data
    acf_plot <- sim_ts |> ACF(x) |> autoplot()

    pacf_plot <- sim_ts |> PACF(x) |> autoplot()

    list(sim_data = sim_ts, acf_plot = acf_plot, pacf_plot = pacf_plot)
  })

  #### AR1 Plots ####
  output$plot1 <- renderPlot({
    df <- sim_data1()$sim_data


    ggplot(df, aes(x = dates, y = x)) +
      geom_line() +
      labs(title = NULL,
           x = "x",
           y = "y")
  })

  output$acf_plot1 <- renderPlot({
    sim_data1()$acf_plot
  })

  output$pacf_plot1 <- renderPlot({
    sim_data1()$pacf_plot
  })


  #### AR2 Plots ####
  output$plot2 <- renderPlot({
    df <- sim_data2()$sim_data


    ggplot(df, aes(x = dates, y = x)) +
      geom_line() +
      labs(title = NULL,
           x = "x",
           y = "y")
  })

  output$acf_plot2 <- renderPlot({
    sim_data2()$acf_plot
  })

  output$pacf_plot2 <- renderPlot({
    sim_data2()$pacf_plot
  })
  #### AR3 Plots ####
  output$plot3 <- renderPlot({
    df <- sim_data3()$sim_data


    ggplot(df, aes(x = dates, y = x)) +
      geom_line() +
      labs(title = NULL,
           x = "x",
           y = "y")
  })

  output$acf_plot3 <- renderPlot({
    sim_data3()$acf_plot
  })

  output$pacf_plot3 <- renderPlot({
    sim_data3()$pacf_plot
  })
  #### Dynamic formulas ####
  output$formula1 <- renderUI({
    a1 <- input$AR1_alpha_1
    tagList(
      paste0("$x_t = ",a1, "x_{t-1} + w_t$"),
      tags$script('renderMathInElement(document.getElementById("formula1"), {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })

  output$formula2 <- renderUI({
    a1 <- input$AR2_alpha_1
    a2 <- input$AR2_alpha_2
    tagList(
      paste0("$x_t = ",a1, "x_{t-1} + ",a2,"x_{t-2} + w_t$"),
      tags$script('renderMathInElement(document.getElementById("formula2"), {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })

  output$formula3 <- renderUI({
    a1 <- input$AR3_alpha_1
    a2 <- input$AR3_alpha_2
    a3 <- input$AR3_alpha_3
    tagList(
      paste0("$x_t = ",a1, "x_{t-1} + ",a2,"x_{t-2} + ",a3," x_{t-3} + w_t$"),
      tags$script('renderMathInElement(document.getElementById("formula3"), {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })
}





shinyApp(ui, server)
