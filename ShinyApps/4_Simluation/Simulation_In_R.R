# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("ggokabeito")) install.packages("ggokabeito")
if (!require("ggplot2")) install.packages("ggplot2")

library(shiny)
library(dplyr)
library(ggplot2)
library(ggokabeito)
library(fpp3)

ui <- fluidPage(
  titlePanel("IDK what this one is supposed to be called"),
  # A fluidRow() defines a row (ik pretty complex)
  # Each row is then split into columns, the first value passed to these defines how much relative space they are alloted for that row as a fraction of 12.
  # 2 columns with an allotment of 6 will be centered into two equal columns on one row.
  # If the columns of a row add to more than 12 it will simply start to overflow into a new row
  # so 3 columns in 6 in a single fluid row will have form a 2x2 grid, with the bottom right section being empty
  fluidRow(
    column(6, sliderInput("sigma", "Standard Deviation", min = 0, max = 10, value = 1)),
    column(6, sliderInput("n_points", "Number of Points", min = 10, max = 500, value = 100, step = 10)),
  ),
  # the optional offset parameter allows you to adjust to position where the columns start
  fluidRow(
    column(4,  offset = 5, actionButton("go", "Simulate!"))
  ),
  # Here is an example of using the overflow feature to get multiple 'rows' without needing to define tons of fluidRow()'s
  fluidRow(
    column(12, h4("Series")),
    column(12, plotOutput("plot")),
    column(12, h4("ACF")),
    column(12, plotOutput("acf_plot"))
  )
)

server <- function(input, output, session) {
  sim_data <- eventReactive(input$go, {
    set.seed(1)


    wd <- tibble(
      seq = seq_len(input$n_points),
      values = rnorm(input$n_points, sd = input$sigma)
    )

    # ACF plot data
    acf_plot <- tsibble(
      seq = seq_len(nrow(wd)),
      values = wd$values,
      index = seq
    ) |> ACF(values) |> autoplot()

    list(sim_data = wd, acf_plot = acf_plot)
  })

  output$plot <- renderPlot({
    df <- sim_data()$sim_data


    ggplot(df, aes(x = seq, y = values)) +
      geom_line() +
      labs(title = NULL,
           x = "x",
           y = "y")
  })

  output$acf_plot <- renderPlot({
    sim_data()$acf_plot
  })
}


shinyApp(ui, server)
