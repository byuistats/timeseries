# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("ggokabeito")) install.packages("ggokabeito")
if (!require("ggplot2")) install.packages("ggplot2")

library(shiny)
library(dplyr)
library(ggplot2)
library(ggokabeito)
library(fpp3) 
library(tsibble)

ui <- fluidPage(
  titlePanel("Bivariate Normal Distribution Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("sigma", "Standard Deviation", min = 0, max = 10, value = 1),
      sliderInput("n_points", "Number of Points", min = 10, max = 500, value = 100, step = 10), 
      actionButton("go", "Simulate!")
    ),
    
    mainPanel(
      plotOutput("plot"),
      plotOutput("additional_plot")
    )
  )
)


server <- function(input, output, session) {
  sim_data <- eventReactive(input$go, {
    set.seed(1)
    
    wd <- tibble(
      seq = seq_len(input$n_points),
      w = rnorm(input$n_points, sd = input$sigma),
      values = cumsum(w)
    )
    
    wd_tsibble <- wd %>%
      as_tsibble(index = seq)
    
    acf_object <- ACF(wd_tsibble)
    
    print(acf_object)
    
    list(sim_data = wd, acf_object = acf_object)
  })
  
  output$plot <- renderPlot({
    df <- sim_data()$sim_data
    
    ggplot(df, aes(x = seq, y = values)) +
      geom_line() +
      labs(title = "Bivariate Normal Distribution Simulator",
           x = "x",
           y = "y")
  })
  
  output$acf_plot <- renderPlot({
    # Get the ACF object from sim_data
    acf_object <- sim_data()$acf_object
    
    # Create ACF plot
    acf_plot <- ggplot(data = acf_object, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity") +
      labs(title = "Autocorrelation Function Plot",
           x = "Lag",
           y = "Autocorrelation Coefficient")
    
    # Print ACF plot object
    print(acf_plot)
    
    # Return ACF plot
    acf_plot
  })
  
}



shinyApp(ui, server)
