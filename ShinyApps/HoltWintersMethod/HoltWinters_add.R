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
library(fable)
library(feasts)
library(tsibbledata)
library(fable.prophet)
library(lubridate)
library(shinyjs)
library(shinyWidgets)

# functions

################## Holt-Winters ####################

holt_winters_additive_forecast <- function(data, value_var, alpha = 0.2, beta = 0.2, gamma = 0.2, p = 12, a1 = NULL, b1 = NULL, s1 = NULL) {
  # Assuming 'data' is a tsibble with a column 'value'
  at <- numeric(nrow(data))
  bt <- numeric(nrow(data))
  st <- numeric(nrow(data))

  at[1] <- ifelse(!is.null(a1), a1, data[[value_var]][1])
  bt[1] <- ifelse(!is.null(b1), b1, (1 / p) * mean( data[[value_var]][(p+1):(2*p)] - data[[value_var]][1:p] ))
  st[1:p] <- ifelse(!is.null(s1), s1, 0)

  # First cycle
  for (t in 2:p) {
    at[t] <- alpha * (data[[value_var]][t] - st[t - 0 * p ]) + (1 - alpha) * (at[t - 1] + bt[t - 1])
    bt[t] <- beta * (at[t] - at[t - 1]) + (1 - beta) * bt[t - 1]
  }

  for (t in (p + 1):nrow(data)) {
    at[t] <- alpha * (data[[value_var]][t] - st[t - p]) + (1 - alpha) * (at[t - 1] + bt[t - 1])
    bt[t] <- beta * (at[t] - at[t - 1]) + (1 - beta) * bt[t - 1]
    st[t] <- gamma * (data[[value_var]][t] - at[t]) + (1 - gamma) * st[t - p]
  }

  data <- data %>%
    mutate(estimated_level = at, estimated_slope = bt, estimated_seasonal = st)

  data %>% return()
}



expand_holt_winters_df <- function(df, date_var, value_var, p = 3, predict_periods = 10) {
  # Note that p must be < nrow(df)
  # Note that predict_periods must be < nrow(df)

  # Create new variables
  df$date <- df[[date_var]]
  df$x_t <- df[[value_var]]

  df2 <- df |>
    # select(date, x_t) |>  # This deletes all variables in original file
    mutate(
      a_t = as.numeric(NA),
      b_t = as.numeric(NA),
      s_t = as.numeric(NA),
      xhat_t = as.numeric(NA)
    )

  header <- df2 |>
    head(p + 1) |>
    mutate(
      date = date - max(date) + min(date),
      x_t = NA
    ) |>
    head(p)

  footer <- df2 |>
    tail(predict_periods + 1) |>
    mutate(
      date = date - min(date) + max(date),
      x_t = NA
    ) |>
    tail(predict_periods)

  df_final <- df_final <- as.data.frame(header)|>
    bind_rows(as.data.frame(df2)) |>
    bind_rows(as.data.frame(footer))
  df_tsibble <- df_final |> as_tsibble(index = date)

  return(df_tsibble)
}

hw_additive_slope_additive_seasonal <- function(df, date_var, value_var, p = 12, predict_periods = 18, alpha = 0.2, beta = 0.2, gamma = 0.2, s_initial = rep(0,p)) {

  # Get expanded data frame
  df <- df |> expand_holt_winters_df(date_var, value_var, p, predict_periods)

  # Fill in prior belief about s_t
  for (t in 1:p) {
    df$s_t[t] <- s_initial[t]
  }

  # Fill in first row of values
  offset <- p # number of header rows to skip
  df$a_t[1 + offset] <- df$x_t[1 + offset]
  df$b_t[1 + offset] <- (1 / p) * mean(df$x_t[(p + 1 + offset):(2 * p + offset)] - df$x_t[(1 + offset):(p + offset)])
  df$s_t[1 + offset] <- (1 - gamma) * df$s_t[1]

  # Fill in remaining rows of body of df with values
  for (t in (2 + offset):(nrow(df) - predict_periods) ) {
    df$a_t[t] = alpha * (df$x_t[t] - df$s_t[t-p]) + (1 - alpha) * (df$a_t[t-1] + df$b_t[t-1])
    df$b_t[t] = beta * (df$a_t[t] - df$a_t[t-1]) + (1 - beta) * df$b_t[t-1]
    df$s_t[t] = gamma * (df$x_t[t] - df$a_t[t]) + (1 - gamma) * df$s_t[t-p]
  }

  df <- df |>
    mutate(k = ifelse(row_number() >= nrow(df) - predict_periods, row_number() - (nrow(df) - predict_periods), NA))

  # Fill in forecasted values
  offset <- nrow(df) - predict_periods
  for (t in offset:nrow(df)) {
    df$s_t[t] = df$s_t[t - p]
    df$xhat_t[t] = df$a_t[offset] + df$k[t] * df$b_t[offset] + df$s_t[t - p]
  }

  # Delete temporary variable k
  df <- df |> select(-k)

  return(df)
}

################## UI ####################

# Define the UI
ui <- fluidPage(
  useShinyjs(),
    titlePanel("Exploration: Holt-Winters Additive Model"),
    fluidRow(
      column(4, dateRangeInput("dateRange", "Select date range:",
                     start = "2011-01-01",  # Default start date
                     end = "2016-11-12",         # Default end date
                     min = "2011-01-01",       # Earliest date selectable
                     max = "2016-11-12")),
      column(4, selectInput("dataset_in", label = "Choose a Dataset",
                  choices = c("Enrollment", "Crimes"))),
      column(4, materialSwitch(inputId = "toggle", value = FALSE, label = "Advanced Inputs"))
    ),
    fluidRow(
      column(4, sliderInput("a", "Alpha", min = 0, max = 1, value = 0.2, step=0.1)),
      column(4, sliderInput("b", "Beta", min = 0, max = 1, value = 0.2, step=0.1)),
      column(4, sliderInput("g", "Gamma", min = 0, max = 1, value = 0.2, step=0.1))
    ),
  div(id = "DynInputs",
    fluidRow(
      column(4, numericInput("p", "Periods per Season", min = 1, max =10, value = 3, step=1)),
      column(4, numericInput("periods", "Periods to Predict", min = 1, max =10, value = 3, step=1))
    ),
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
      #column(12, h4("Components:")),
      #column(12, h4("'a_t'")),
      column(12, plotOutput("plot_at")),
      #column(12, h4("'b_t'")),
      column(12, plotOutput("plot_bt")),
      #column(12, h4("'s_t'")),
      column(12, plotOutput("plot_st")),
      #column(12, uiOutput("formula1")),
      #column(12, h3("Table: Title???")),
      column(12, tableOutput("table"))
    )
  )
)



################## Server ####################

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


  # output$p_input <- renderUI({
  #   req(sim_data()) # Wait for the data to be loaded
  #   if (input$dataset_in == "Enrollment"){
  #     data <- enrollment_ts #filter(enrollment_ts, dates >= as.Date(input$dateRange[1], format = "%Y/%m/%d") & dates <= as.Date(input$dateRange[2], format = "%Y/%m/%d"))
  #   } else {
  #     data <- filter(crime_data, dates >= as.Date(input$dateRange[1], format = "%Y/%m/%d") & dates <= as.Date(input$dateRange[2], format = "%Y/%m/%d"))
  #   }
  #   numericInput("p", "Periods in a season", min = 1, max = nrow(data()), value = 3, step=1)
  # })

  #### import data ####
  crime_data <- rio::import("https://byuistats.github.io/timeseries/data/baltimore_crime.csv", header=TRUE, stringsAsFactors=FALSE)

  #wrangle data
  crime_data <- crime_data %>%
    group_by(CrimeDate) %>%
    summarise(
      value = sum(Total.Incidents)
    )
  crime_data <- crime_data %>%
    mutate(dates = as.Date(CrimeDate, format = "%m/%d/%Y")) %>%
    arrange(dates)
  crime_data <- as_tsibble(crime_data, index = dates)

  enrollment_ts <- rio::import("https://byuistats.github.io/timeseries/data/byui_enrollment.csv") |>
    mutate(dates = yearmonth( ym( paste(year, term * 4 - 3) ) ), value = enrollment ) |>
    dplyr::select(semester, dates, value) |>
    as_tsibble(index = dates)

  extra_terms <- enrollment_ts |>
    tail(6) |>
    mutate(dates = yearmonth(ym(dates) + years(2))) |>
    mutate(
      semester = paste0(
        left(semester, 2),
        as.integer(right(semester, 2)) + 2
      )
    ) |>
    mutate(value = NA, enroll_thousand = NA)

  #### app ####

  #Reactive data input
  sim_data <- eventReactive(input$go,{
    if (input$dataset_in == "Enrollment"){
      data <- enrollment_ts #filter(enrollment_ts, dates >= as.Date(input$dateRange[1], format = "%Y/%m/%d") & dates <= as.Date(input$dateRange[2], format = "%Y/%m/%d"))
    } else {
      data <- filter(crime_data, dates >= as.Date(input$dateRange[1], format = "%Y/%m/%d") & dates <= as.Date(input$dateRange[2], format = "%Y/%m/%d"))
    }

    forecast_dat <- holt_winters_additive_forecast(data, "value", alpha = input$a, input$b, gamma = input$g, p = 3, a1 = NULL, b1 = NULL, s1 = NULL)
    preddat <- hw_additive_slope_additive_seasonal(forecast, "dates", "value", p = 3, predict_periods = 4, alpha = 0.2, beta = 0.2, gamma = 0.2, s_initial = rep(0,p))
    return(
      list(
        data <- forecast_dat
      )
    )
  })

output$plot_fin<-renderPlot({
  data <- sim_data()[[1]]
  ggplot(data, aes(x = dates)) +
    geom_line(aes(y = value, color = "Base"),linetype=3, size = 1) +
    geom_line(aes(y = estimated_level + estimated_seasonal, color = "Components", alpha=0.5), size = 1) +
    labs(
      x = "Date",
      y = "Value",
      title = NULL,
      color = "Series"
    ) +
    theme_minimal() +
    theme(legend.position = "top")+
    scale_color_manual(values = c("black", "#56B4E9"))+
    guides(alpha = FALSE)
})
output$plot_at<-renderPlot({
  data <- sim_data()[[1]]
  ggplot(data, aes(x = dates, y = estimated_level)) +
    geom_line(color = "#009E73", size = 1) +
    labs(
      x = "Date",
      y = "Seasonal (st)",
      title = NULL
    ) +
    theme_minimal()+
    theme(legend.position = "none")
  })
output$plot_bt<-renderPlot({
  data <- sim_data()[[1]]
  ggplot(data, aes(x = dates, y = estimated_slope)) +
    geom_line(color = "#D55E00", size = 1) +
    labs(
      x = "Date",
      y = "Seasonal (st)",
      title = NULL
    ) +
    theme_minimal()+
    theme(legend.position = "none")
})
output$plot_st<-renderPlot({
  data <- sim_data()[[1]]
  ggplot(data, aes(x = dates, y = estimated_seasonal)) +
    geom_line(color = "#E69F00", size = 1) +
    labs(
      x = "Date",
      y = "Seasonal (st)",
      title = NULL
    ) +
    theme_minimal()+
    theme(legend.position = "none")
})


  output$table<- renderTable({
    sim_data()[[1]]
  })

  output$plot_enroll<-renderPlot({
    enrollment_ts |>
      bind_rows(extra_terms) |>
      autoplot(.vars = value) +
      labs(
        x = "Time",
        y = "Enrollment",
        title = paste0("BYU-Idaho On-Campus Enrollment Counts")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  })

  output$predplot<-renderPlot({
    ggplot(preddat, aes(x = date)) +
      geom_line(aes(y = x_t, color = "Base"), size = 1) +
      geom_line(aes(y = xhat_t, color = "Components", alpha=0.5),linetype=3, size = 1) +
      labs(
        x = "Date",
        y = "Value",
        title = NULL,
        color = "Series"
      ) +
      theme_minimal() +
      theme(legend.position = "top")+
      scale_color_manual(values = c("black", "#56B4E9"))+
      guides(alpha = FALSE)
  })

}


# Run the application
shinyApp(ui, server)

