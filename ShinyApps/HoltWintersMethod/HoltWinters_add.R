# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("MASS")) install.packages("MASS")

# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tidyverse)
library(rio)

# functions

holt_winters_forecast_plot <- function(data, alpha = 0.2, beta = 0.2, gamma = 0.2, p = 12, a1 = NULL, b1 = NULL, s1 = NULL) {
  # Assuming 'data' is a tsibble with a column 'total_incidents'
  data <- data %>%
    mutate(month = month(CrimeDate))

  at <- numeric(nrow(data))
  bt <- numeric(nrow(data))
  st <- numeric(nrow(data))

  at[1] <- ifelse(!is.null(a1), a1, data$total_incidents[1])
  bt[1] <- ifelse(!is.null(b1), b1, 0)
  st[1:p] <- ifelse(!is.null(s1), s1, data$total_incidents[1:p])

  for (t in (p + 1):nrow(data)) {
    at[t] <- alpha * (data$total_incidents[t] - st[t - p]) + (1 - alpha) * (at[t - 1] + bt[t - 1])
    bt[t] <- beta * (at[t] - at[t - 1]) + (1 - beta) * bt[t - 1]
    st[t] <- gamma * (data$total_incidents[t] - at[t]) + (1 - gamma) * st[t - p]
  }

  data <- data %>%
    mutate(estimated_level = at, estimated_slope = bt, estimated_seasonal = st)

  # Plot 1: Just 'at'
  plot_at <- ggplot(data, aes(x = CrimeDate, y = at)) +
    geom_line(color = "blue", size = 1) +
    labs(
      x = "Date",
      y = "Level (at)",
      title = "a_t"
    ) +
    theme_minimal()+
    theme(legend.position = "none")

  # Plot 2: Just 'bt'
  plot_bt <- ggplot(data, aes(x = CrimeDate, y = bt)) +
    geom_line(color = "red", size = 1) +
    labs(
      x = "Date",
      y = "Slope (bt)",
      title = "b_t"
    ) +
    theme_minimal()+
    theme(legend.position = "none")

  # Plot 3: Just 'st'
  plot_st <- ggplot(data, aes(x = CrimeDate, y = st)) +
    geom_line(color = "green", size = 1) +
    labs(
      x = "Date",
      y = "Seasonal (st)",
      title = "s_t"
    ) +
    theme_minimal()+
    theme(legend.position = "none")

  # Plot 4: Original trendline with 'at + bt + st'
  plot_combined <- ggplot(data, aes(x = CrimeDate)) +
    geom_line(aes(y = total_incidents, color = "Base"),linetype=3, size = 1) +
    geom_line(aes(y = estimated_level + estimated_slope + estimated_seasonal, color = "Components", alpha=0.5), size = 1) +
    labs(
      x = "Date",
      y = "Total Incidents",
      title = "Original & 'a_t + b_t + s_t'",
      color = "Series"
    ) +
    theme_minimal() +
    theme(legend.position = "top")+
    scale_color_manual(values = c("black", "red"))

  return(list(plot_at, plot_bt, plot_st, plot_combined, data))
}



# Define the UI
ui <- fluidPage(
    titlePanel("Exploration: Holt-Winters Additive Model"),
    fluidRow(
      column(4, sliderInput("a", "Alpha", min = 0, max = 1, value = 0.2, step=0.1)),
      column(4, sliderInput("b", "Beta", min = 0, max = 1, value = 0.2, step=0.1)),
      column(4, sliderInput("g", "Gamma", min = 0, max = 1, value = 0.2, step=0.1)),
      column(4, dateRangeInput("dateRange", "Select date range:",
                     start = "2011-01-01",  # Default start date
                     end = "2016-11-12",         # Default end date
                     min = "2011-01-01",       # Earliest date selectable
                     max = "2016-11-12"))         # Latest date selectable
    ),
    fluidRow(
      column(4, offset = 5,actionButton("go", "Run!")),
    ),
    fluidRow(
      #column(12, uiOutput("formula0")),
      column(12, plotOutput("plot_fin")),
      column(12, h4("Components:")),
      column(12, plotOutput("plot_at")),
      column(12, plotOutput("plot_bt")),
      column(12, plotOutput("plot_st")),
      #column(12, uiOutput("formula1")),
      column(12, h3("Table: Title???"))
    )
)




# Define the server logic
server <- function(input, output, session) {
  #import data
  df <- rio::import("https://byuistats.github.io/timeseries/data/baltimore_crime.csv", header=TRUE, stringsAsFactors=FALSE)



  #wrangle data
  crime_data <- df %>%
    group_by(CrimeDate) %>%
    summarise(
      total_incidents = sum(Total.Incidents)
    )
  crime_data <- summary_df %>%
    mutate(CrimeDate = as.Date(CrimeDate, format = "%m/%d/%Y")) %>%
    arrange(CrimeDate)
  crime_data <- as_tsibble(crime_data, index = CrimeDate)

  #Reactive data input
  sim_data <- eventReactive(input$go,{
    data <- filter(crime_tsibble, CrimeDate >= as.Date(input$dateRange[1], format = "%Y/%m/%d") & CrimeDate <= as.Date(input$dateRange[2], format = "%Y/%m/%d"))
    plots <- holt_winters_forecast_plot(data, alpha = input$a, beta = input$b, gamma = input$g)
    return(
      plots
    )
  })

  output$plot_fin<-renderPlot({
    sim_data()[[4]]
  })
  output$plot_at<-renderPlot({
    sim_data()[[1]]
    })
  output$plot_bt<-renderPlot({
    sim_data()[[2]]
  })
  output$plot_st<-renderPlot({
    sim_data()[[3]]
  })


}


# Run the application
shinyApp(ui, server)

