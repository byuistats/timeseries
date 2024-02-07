# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("MASS")) install.packages("MASS")

# remove bt from combined graph
# drop down for dataset selection

# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tidyverse)
library(tsibble)
library(rio)
library(shinyjs)
library(shinyWidgets)

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
    geom_line(color = "#E69F00", size = 1) +
    labs(
      x = "Date",
      y = "Level (at)",
      title = NULL
    ) +
    theme_minimal()+
    theme(legend.position = "none")

  # Plot 2: Just 'bt'
  plot_bt <- ggplot(data, aes(x = CrimeDate, y = bt)) +
    geom_line(color = "#D55E00", size = 1) +
    labs(
      x = "Date",
      y = "Slope (bt)",
      title = NULL
    ) +
    theme_minimal()+
    theme(legend.position = "none")

  # Plot 3: Just 'st'
  plot_st <- ggplot(data, aes(x = CrimeDate, y = st)) +
    geom_line(color = "#009E73", size = 1) +
    labs(
      x = "Date",
      y = "Seasonal (st)",
      title = NULL
    ) +
    theme_minimal()+
    theme(legend.position = "none")

  # Plot 4: Original trendline with 'at + bt + st'
  plot_combined <- ggplot(data, aes(x = CrimeDate)) +
    geom_line(aes(y = total_incidents, color = "Base"),linetype=3, size = 1) +
    geom_line(aes(y = estimated_level + estimated_seasonal, color = "Components", alpha=0.5), size = 1) +
    labs(
      x = "Date",
      y = "Total Incidents",
      title = NULL,
      color = "Series"
    ) +
    theme_minimal() +
    theme(legend.position = "top")+
    scale_color_manual(values = c("black", "#56B4E9"))+
    guides(alpha = FALSE)

  return(list(plot_at, plot_bt, plot_st, plot_combined, data))
}



# Define the UI
ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
    titlePanel("Exploration: Holt-Winters Additive Model"),
    fluidRow(
      column(3, dateRangeInput("dateRange", "Select date range:",
                               start = "2011-01-01",  # Default start date
                               end = "2016-11-12",         # Default end date
                               min = "2011-01-01",       # Earliest date selectable
                               max = "2016-11-12")),
      column(3, selectInput("dataset", label = "Choose a Dataset:",
                            choices = c("Basic", "Enrollment", "Crime"))),
      column(3, selectInput("series", label = "Choose a Series:",
                            choices = c("Monthly Total", "Daily Average for Month"))),
      column(3, materialSwitch(inputId = "toggle", value = FALSE, label = "Advanced Inputs")),
    ),
    fluidRow(
      column(4, sliderInput("a", "$$\\alpha$$", min = 0, max = 1, value = 0.2, step=0.1)),
      column(4, sliderInput("b", "Beta", min = 0, max = 1, value = 0.2, step=0.1)),
      column(4, sliderInput("g", "Gamma", min = 0, max = 1, value = 0.2, step=0.1))
    ),

  div(id = "AdvInputs",
    fluidRow(
      column(2, numericInput("s1", "s1",0)),
      column(2, numericInput("s2", "s2",0)),
      column(2, numericInput("s3", "s3",0)),
      column(2, numericInput("s4", "s4",0)),
      column(2, numericInput("s5", "s5",0)),
      column(2, numericInput("s6", "s6",0))
      ),
    fluidRow(
      column(2, numericInput("s7", "s7",0)),
      column(2, numericInput("s8", "s8",0)),
      column(2, numericInput("s9", "s9",0)),
      column(2, numericInput("s10", "s10",0)),
      column(2, numericInput("s11", "s11",0)),
      column(2, numericInput("s12", "s12",0))
      ),
    ),
    fluidRow(
      column(4, offset = 5,actionButton("go", "Run!")),
    ),
  div(id = "outputs",
    fluidRow(
      #column(12, uiOutput("formula0")),
      column(12, h4("Original & 'a_t + b_t + s_t'")),
      column(12, plotOutput("plot_fin")),
      column(12, h4("Components:")),
      column(12, h4("'a_t'")),
      column(12, plotOutput("plot_at")),
      column(12, h4("'b_t'")),
      column(12, plotOutput("plot_bt")),
      column(12, h4("'s_t'")),
      column(12, plotOutput("plot_st")),
      #column(12, uiOutput("formula1")),
      column(12, h3("Table: Title???"))
    )
  )
)




# Define the server logic
server <- function(input, output, session) {
  observe({
    if(input$toggle) {
      show("AdvInputs")
    } else {
      hide("AdvInputs")
    }
  })

  observe({
    if(input$go) {
      show("outputs")
    } else {
      hide("outputs")
    }
  })

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

