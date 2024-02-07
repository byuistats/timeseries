# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("ggokabeito")) install.packages("ggokabeito")
if (!require("ggplot2")) install.packages("ggplot2")

library(shiny)
library(dplyr)
library(ggplot2)
library(ggokabeito)

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
    
    acf_plot <- wd %>%
      as_tsibble(index = seq) %>%
      ACF(values) %>%
      autoplot()
    
    list(sim_data = wd, acf_plot = acf_plot)
  })
  
  output$plot <- renderPlot({
    df <- sim_data()$sim_data
    
    ggplot(df, aes(x = seq, y = values)) +
      geom_line() +
      labs(title = "Bivariate Normal Distribution Simulator (cumulative sum)",
           x = "x",
           y = "y")
  })
  
  output$acf_plot <- renderPlot({
    sim_data()$acf_plot +
      ggtitle("Auto-correlation Function Plot")  # Adding title to the ACF plot
  })
}






shinyApp(ui, server)
