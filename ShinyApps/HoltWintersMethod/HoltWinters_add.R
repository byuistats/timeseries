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
library(zoo)
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



expand_holt_winters_df <- function(df, date_var, value_var, p = 12, predict_periods = 18, round_to = "day") {
  # Ensure date is in Date format
  df[[date_var]] <- as.Date(df[[date_var]])

  # Check if all dates have the same day component
  same_day <- all(day(df[[date_var]]) == day(df[[date_var]][1]))

  start_date <- min(df[[date_var]])
  end_date <- max(df[[date_var]])

  if(round_to == "month" | round_to == "Month") {
    # Calculate the difference in months between the first two rows
    date_diff_months <- as.integer(round(as.numeric(difftime(df[[date_var]][2], df[[date_var]][1], units = "days")) / 30))

    # Create sequences for header and footer with distinct months
    header_dates <- seq.Date(from = start_date %m-% months(date_diff_months * p), by = paste0(date_diff_months, " months"), length.out = p)
    footer_dates <- seq.Date(from = end_date %m+% months(date_diff_months), by = paste0(date_diff_months, " months"), length.out = predict_periods)
  } else {
    # Calculate the difference in days between the first two rows
    date_diff <- as.numeric(df[[date_var]][2] - df[[date_var]][1])
    # Create the header by generating a sequence of dates before the first date
    header_dates <- seq(from = start_date - date_diff*p, by = date_diff, length.out = p)

    # Create the footer by generating a sequence of dates after the last date
    footer_dates <- seq(from = end_date + date_diff, by = date_diff, length.out = predict_periods)

  }
  # Combine header, original df, and footer
  header_df <- data.frame(date = header_dates, x_t = rep(NA, p), section="H")
  footer_df <- data.frame(date = footer_dates, x_t = rep(NA, predict_periods), section="F")


  # Ensure original df has the necessary columns
  df$x_t <- df[[value_var]]
  df$date <- df[[date_var]]
  df <- df |>
    # select(date, x_t) |>  # This deletes all variables in original file
    mutate(
      a_t = as.numeric(NA),
      b_t = as.numeric(NA),
      s_t = as.numeric(NA),
      xhat_t = as.numeric(NA),
      section = "B"
    )
  debug <- as.character(df[[date_var]][1])

  # Combine all parts
  df_final <- bind_rows(header_df, as.data.frame(df), footer_df)
  #df_final$date <- format(df_final$date, output_date_format)
  # Convert to tsibble if necessary
  df_tsibble <- as_tsibble(df_final, index = date)

  return(df_tsibble)
}

holt_winters_forecast <- function(df, date_var, value_var, p = 12, predict_periods = 18, alpha = 0.2, beta = 0.2, gamma = 0.2, s_initial = rep(0, p), round_to, slope_type = "add", season_type = "add") {
  # Get expanded data frame with section column
  df <- df |> expand_holt_winters_df(date_var, value_var, p, predict_periods, round_to)

  ### Header
  # Initialize the seasonal component for the header section
  df$s_t[1:p] <- s_initial


  ### Initial row of body
  # Calculate initial level and trend values based on the first period of actual data
  actual_start <- p + 1 # The start of actual data in the expanded dataframe
  df$a_t[actual_start] <- df$x_t[actual_start]

  roll_mean <- df %>%
    filter(section == "B") %>%
    mutate(roll_mean = rollmean(x_t, 7, fill = NA, align = "center")) %>%
    select(roll_mean)
  roll_mean <- roll_mean[4,1][[1]]

  #df$b_t[actual_start] <- mean(diff(df$x_t[actual_start:(actual_start + p - 1)])) / p #chat gpt code doesn't calc correctly
  #df$b_t[actual_start] <- mean(df$x_t[(actual_start + p):(3 * p)] - df$x_t[(actual_start):(p + p)]) / p # simplified Bro Johnson code
  df$b_t[actual_start] <- (df$x_t[actual_start] - roll_mean) / df$x_t[actual_start] # period 4 of 7 period rolling average (Bro Moncayo)
  df$s_t[actual_start] <- df$s_t[1]



  ### Fill Body
  for (t in (actual_start + 1):(nrow(df) - predict_periods)) {
    prior_seasonal_index <- ifelse(t <= actual_start + p, t - actual_start, t - p)
    df$a_t[t] <- alpha * (df$x_t[t] - df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t - 1] + df$b_t[t - 1])
    df$b_t[t] <- beta * (df$a_t[t] - df$a_t[t - 1]) + (1 - beta) * df$b_t[t - 1]
    df$s_t[t] <- gamma * (df$x_t[t] - df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index]
  }

  ### Footer
  # Prepare for forecasting
  last_actual_t <- nrow(df) - predict_periods
  forecast_start <- last_actual_t

  # Forecast future values
  for (t in forecast_start:nrow(df)) {
    forecast_index <- t - last_actual_t
    df$s_t[t] <- df$s_t[t - p] # Carry forward the seasonal component
    if (slope_type == "add" & season_type == "add"){
      df$xhat_t[t] <- (df$a_t[last_actual_t] + forecast_index * df$b_t[last_actual_t]) + df$s_t[t - p] # add slope & add season
    } else if (slope_type == "add" & season_type == "mult"){
      df$xhat_t[t] <- (df$a_t[last-actual_t] + forecast_index * df$b_t[last-actual_t]) * df$s_t[t - p] # add slope & mult season
    } else if (slope_type == "mult" & season_type == "add"){
      df$xhat_t[t] <- (df$a_t[last-actual_t] + df$b_t[last-actual_t]^(forecast_index)) + df$s_t[t - p] # mult slope & add season
    } else {
      df$xhat_t[t] <- (df$a_t[last-actual_t] + df$b_t[last-actual_t]^(forecast_index)) * df$s_t[t - p] # mult slope & mult season
    }
  }


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
      column(4, numericInput("p", "Periods per Season", min = 1, max =1, value = 1, step=1)),
      column(4, numericInput("periods", "Periods to Predict", min = 1, max=1 , value = 1, step=1)),
      column(4, selectInput("round_to", label = "Round off dates to:",
                            choices = c("day", "month")))
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
    column(12, tableOutput("preview")),
  ),
  div(id = "Predict",
    fluidRow(
      column(4, offset = 5,actionButton("go", "Predict!")),
    ),
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
      column(12, uiOutput("formula1")),
      #column(12, h3("Table: Title???")),
      column(12, tableOutput("table")),
      column(12, plotOutput("predplot")),
      column(12, dataTableOutput("dattable"))
     # column(12, textOutput("text"))
    )
  )
)



################## Server ####################

# Define the server logic
server <- function(input, output, session) {
  #### import data ####
  crime_data <- rio::import("https://byuistats.github.io/timeseries/data/baltimore_crime.csv", header=TRUE, stringsAsFactors=FALSE)

  #wrangle data
  crime_data <- crime_data %>%
    group_by(CrimeDate) %>%
    summarise(
      value = sum(Total.Incidents)
    )
  crime_data <- crime_data %>%
    mutate(dates = base::as.Date(CrimeDate, format = "%m/%d/%Y")) %>%
    arrange(dates)

  crime_summary <- as.data.frame(crime_data)
  crime_summary <- crime_summary %>%
    mutate(dates = floor_date(dates, "month")) %>%
    group_by(dates) %>%
    summarise(
      value = sum(value)
    ) %>%
    mutate(average_value = value / days_in_month(dates)) %>%
    as_tsibble(index = dates)
  #is coming out Y D M needs to be Y M D

  enrollment_ts <- rio::import("https://byuistats.github.io/timeseries/data/byui_enrollment.csv") |>
    mutate(dates = yearmonth( ym( paste(year, term * 4 - 3) ) ), value = enrollment ) |>
    dplyr::select(semester, dates, value) |>
    as_tsibble(index = dates)

  # extra_terms <- enrollment_ts |>
  #   tail(6) |>
  #   mutate(dates = yearmonth(ym(dates) + years(2))) |>
  #   mutate(
  #     semester = paste0(
  #       left(semester, 2),
  #       as.integer(right(semester, 2)) + 2
  #     )
  #   ) |>
  #   mutate(value = NA, enroll_thousand = NA)

  #### observe updates ####
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

  observe({
    # Determine the length of the selected dataset
    datasetLength <- if (input$dataset_in == "Enrollment") {
      nrow(enrollment_ts)
    } else if (input$dataset_in == "Crimes") {
      nrow(crime_data)
    } else {
      1 # Default or fallback value
    }

    # Update the max parameter of the 'p' and 'periods' inputs
    updateNumericInput(session, "p", max = datasetLength)
    updateNumericInput(session, "periods", max = datasetLength)
  })


  #### event reactives ####

  #Reactive data input
  sel_data <- reactive({
    if (input$dataset_in == "Enrollment"){
      data <- enrollment_ts #filter(enrollment_ts, dates >= as.Date(input$dateRange[1], format = "%Y/%m/%d") & dates <= as.Date(input$dateRange[2], format = "%Y/%m/%d"))
    } else {
      data <- filter(crime_data, dates >= as.Date(input$dateRange[1], format = "%Y/%m/%d") & dates <= as.Date(input$dateRange[2], format = "%Y/%m/%d"))
    }
    data
  })

  sim_data <- eventReactive(input$go,{
    if (input$dataset_in == "Enrollment"){
      data <- enrollment_ts #filter(enrollment_ts, dates >= as.Date(input$dateRange[1], format = "%Y/%m/%d") & dates <= as.Date(input$dateRange[2], format = "%Y/%m/%d"))
    } else {
      data <- filter(crime_summary, dates >= as.Date(input$dateRange[1], format = "%Y/%m/%d") & dates <= as.Date(input$dateRange[2], format = "%Y/%m/%d"))
    }

    forecast_dat <- holt_winters_forecast(data, "dates","value", alpha = input$a, input$b, gamma = input$g, p = input$p, predict_periods = input$periods,s_initial = rep(0,input$p), round_to = input$round_to)
    #preddat <- hw_additive_slope_additive_seasonal(data, "dates", "value", p = input$p, predict_periods = input$periods, alpha = input$a, beta = input$b, gamma = input$g, s_initial = rep(0,input$p), round_to = input$round_to)
    #expanddat <- expand_holt_winters_df(data, "dates", "value", p = input$p, predict_periods = input$periods, round_to = input$round_to)
    return(
      list(
        data <- forecast_dat
        #preddata <- preddat
        #expand <- expanddat
      )
    )
  })

  output$plot_fin<-renderPlot({
    data <- sim_data()[[1]]
    ggplot(data, aes(x = dates)) +
      geom_line(aes(y = value, color = "Base"),linetype=3, size = 1) +
      geom_line(aes(y = a_t + s_t, color = "Components", alpha=0.7), size = 1) +
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
    ggplot(data, aes(x = dates, y = a_t)) +
      geom_line(color = "#009E73", size = 1) +
      labs(
        x = "Date",
        y = "level (at)",
        title = NULL
      ) +
      theme_minimal()+
      theme(legend.position = "none")
    })
  output$plot_bt<-renderPlot({
    data <- sim_data()[[1]]
    ggplot(data, aes(x = dates, y = b_t)) +
      geom_line(color = "#D55E00", size = 1) +
      labs(
        x = "Date",
        y = "Slope (bt)",
        title = NULL
      ) +
      theme_minimal()+
      theme(legend.position = "none")
  })
  output$plot_st<-renderPlot({
    data <- sim_data()[[1]]
    ggplot(data, aes(x = dates, y = s_t)) +
      geom_line(color = "#E69F00", size = 1) +
      labs(
        x = "Date",
        y = "Seasonal (st)",
        title = NULL
      ) +
      theme_minimal()+
      theme(legend.position = "none")
  })


  # output$table<- renderTable({
  #   sim_data()[[1]]
  # })
  output$dattable<- renderDataTable({
    sim_data()[[1]]
  })

  output$preview<- renderTable({
    data <- sel_data()
    data$dates <- as.character(data$dates)
    data[1:5,]
  })

  # output$text<- renderText({
  #   sim_data()[[1]]
  # })

  # output$plot_enroll<-renderPlot({
  #   enrollment_ts |>
  #     bind_rows(extra_terms) |>
  #     autoplot(.vars = value) +
  #     labs(
  #       x = "Time",
  #       y = "Enrollment",
  #       title = paste0("BYU-Idaho On-Campus Enrollment Counts")
  #     ) +
  #     theme(
  #       plot.title = element_text(hjust = 0.5)
  #     )
  # })

  output$predplot<-renderPlot({
    data <- sim_data()[[1]]
    ggplot(data, aes(x = date)) +
      geom_line(aes(y = x_t, color = "Base"), size = 1) +
      geom_line(aes(y = xhat_t, color = "Components"),linetype=3, size = 1) +
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

