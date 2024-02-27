# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("MASS")) install.packages("MASS")

# remove bt from combined graph
# drop down for dataset selection

# Load required libraries
library(shiny)
library(zoo)
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
    dplyr::select(roll_mean)
  roll_mean <- roll_mean[4,1][[1]]

  #df$b_t[actual_start] <- mean(df$x_t[(actual_start + p):(3 * p)] - df$x_t[(actual_start):(p + p)]) / p # simplified Bro Johnson code
  df$b_t[actual_start] <- (df$x_t[actual_start] - roll_mean) / df$x_t[actual_start] # period 4 of 7 period rolling average (Bro Moncayo)
  df$s_t[actual_start] <- df$s_t[1]


  for (t in (1 + actual_start):(nrow(df) - predict_periods) ) {
    prior_seasonal_index <- ifelse(t <= actual_start + p, t - actual_start, t - p)
    if (slope_type == "Additive" & season_type == "Additive"){
      df$a_t[t] <- alpha * (df$x_t[t] - df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t - 1] + df$b_t[t - 1])
      df$b_t[t] <- beta * (df$a_t[t] - df$a_t[t - 1]) + (1 - beta) * df$b_t[t - 1]
      df$s_t[t] <- gamma * (df$x_t[t] - df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index]# add slope & add season
    } else { #(slope_type == "Additive" & season_type == "Multiplicative")
      df$a_t[t] = alpha * (df$x_t[t] / df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t-1] + df$b_t[t-1])
      df$b_t[t] = beta * (df$a_t[t] - df$a_t[t-1]) + (1 - beta) * df$b_t[t-1]
      df$s_t[t] = gamma * (df$x_t[t] / df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index] # add slope & mult season
    } # else if (slope_type == "Multiplicative" & season_type == "Additive"){
    #   df$a_t[t] = alpha * (df$x_t[t] + df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t-1] + df$b_t[t-1])
    #   df$b_t[t] = beta * (df$a_t[t] / df$a_t[t-1]) + (1 - beta) * df$b_t[t-1]
    #   df$s_t[t] = gamma * (df$x_t[t] + df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index] # mult slope & add season
    # } else {
    #   df$a_t[t] = alpha * (df$x_t[t] / df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t-1] + df$b_t[t-1])
    #   df$b_t[t] = beta * (df$a_t[t] / df$a_t[t-1]) + (1 - beta) * df$b_t[t-1]
    #   df$s_t[t] = gamma * (df$x_t[t] / df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index] # mult slope & mult season
    # }
  }

  ### Footer
  # Prepare for forecasting
  last_actual_t <- nrow(df) - predict_periods
  forecast_start <- last_actual_t

  # Forecast future values
  for (t in forecast_start:nrow(df)) {
    forecast_index <- t - last_actual_t
    df$s_t[t] <- df$s_t[t - p] # Carry forward the seasonal component
    if (slope_type == "Additive" & season_type == "Additive"){
      df$xhat_t[t] <- (df$a_t[last_actual_t] + forecast_index * df$b_t[last_actual_t]) + df$s_t[t - p] # add slope & add season
    } else { #(slope_type == "Additive" & season_type == "Multiplicative")
      df$xhat_t[t] <- (df$a_t[last_actual_t] + forecast_index * df$b_t[last_actual_t]) * df$s_t[t - p] # add slope & mult season
    }
    # } else if (slope_type == "Multiplicative" & season_type == "Additive"){
    #   df$xhat_t[t] <- (df$a_t[last_actual_t] + df$b_t[last_actual_t]^(forecast_index)) + df$s_t[t - p] # mult slope & add season
    # } else {
    #   df$xhat_t[t] <- (df$a_t[last_actual_t] + df$b_t[last_actual_t]^(forecast_index)) * df$s_t[t - p] # mult slope & mult season
    # }
  }


  return(df)
}

################## UI ####################

# Define the UI
ui <- fluidPage(
  useShinyjs(),
    titlePanel("Exploration: Holt-Winters Additive Model"),
    fluidRow(
      column(3, selectInput("dataset_in", label = "Choose a Dataset",
                            choices = c("Enrollment", "Baltimore Crimes", "Apple Revenue"))),
      column(3, dateRangeInput("dateRange", "Select date range:",
                     start = "2000-01-01",  # Default start date
                     end = "2024-01-01",         # Default end date
                     min = "2000-01-01",       # Earliest date selectable
                     max = "2024-01-01")),
      column(2, materialSwitch(inputId = "toggle_advIn", value = FALSE, label = "Advanced Inputs")),
      column(2, materialSwitch(inputId = "toggle_advOut", value = FALSE, label = "Advanced Outputs")),
      column(2, materialSwitch(inputId = "toggle_modelVals", value = FALSE, label = "Use Model Values"))
    ),
    fluidRow(
      column(4, numericInput("a", "Alpha", min = 0, max = 1, value = 0.2)),
      column(4, numericInput("b", "Beta", min = 0, max = 1, value = 0.2)),
      column(4, numericInput("g", "Gamma", min = 0, max = 1, value = 0.2))
    ),
  div(id = "DynInputs",
    fluidRow(
      column(3, numericInput("p", "Periods per Season", min = 1, max =1, value = 1, step=1)),
      column(3, numericInput("periods", "Periods to Predict", min = 1, max=1 , value = 1, step=1)),
      #column(3, selectInput("slope", "Type of Slope", choices = c("Additive", "Multiplicative"))),
      column(3, selectInput("season", "Type of Seasonality", choices = c("Additive", "Multiplicative"))),
      column(3, selectInput("value", "Value", selected = NULL, choices = c()))
      #column(4, selectInput("round_to", label = "Round off dates to:", choices = c("day", "month")))
    ),
  ),
  div(id = "AdvInputs",
    fluidRow(
      column(3, textInput("S_list", "Initial S values, seperated by commas", placeholder = "1,0.9,1,1.2"))
      ),
    ),
  fluidRow(
    column(6, tableOutput("preview_data")),
    column(6, verbatimTextOutput("preview"))
  ),
  div(id = "Predict",
    fluidRow(
      column(4, offset = 5, actionButton("explore", "Model Series")),
      column(4, offset = 5, hidden(div(id = "secondButton", actionButton("go", "Predict Series")))), # Hide this initially
      column(12, textOutput("debug")),
    ),
  ),
  div(id = "AdvOutputs",
    fluidRow(
      column(12, h4("Components:")),
      column(12, h4("'a_t'")),
      column(12, plotOutput("plot_at")),
      column(12, h4("'b_t'")),
      column(12, plotOutput("plot_bt")),
      column(12, h4("'s_t'")),
      column(12, plotOutput("plot_st"))
    )
  ),
  div(id = "outputs",
    fluidRow(
      column(12, h4("Original & 'Components'")),
      column(12, plotOutput("plot_fin")),
      column(12, h4("Table: Title???")),
      column(12, plotOutput("predplot")),
      column(12, dataTableOutput("dattable")),
      column(12, textOutput("initial_list"))
    )
  )
)



################## Server ####################

# Define the server logic
server <- function(input, output, session) {
  #### observe updates ####
  observeEvent(input$explore, {
    # Use shinyjs to show the second button when the first button is clicked
    shinyjs::show("secondButton")
  })

  # observeEvent(input$dataset_in, {
  #   updateMaterialSwitch(session,"toggle_advIn", FALSE)
  #   updateMaterialSwitch(session,"toggle_advOut", FALSE)
  #   updateMaterialSwitch(session,"toggle_modelVals", FALSE)
  # })

  observe({
    if(input$toggle_advIn) {
      show("AdvInputs")
    } else {
      hide("AdvInputs")
    }
  })

  observe({
    if(input$toggle_advOut) {
      show("AdvOutputs")
    } else {
      hide("AdvOutputs")
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
    datasetLength <- nrow(sel_data())
    datasetColumns <- colnames(sel_data())

    # Update the max parameter of the 'p' and 'periods' inputs
    updateNumericInput(session, "p", max = datasetLength)
    updateNumericInput(session, "periods", max = datasetLength*10)
    updateSelectInput(session, "value", choices = datasetColumns)
    updateDateRangeInput(session, "dateRange",   start = data_dates()[[1]], end = data_dates()[[2]], min = data_dates()[[1]], max = data_dates()[[2]])
  })

  observe({
    datasetLength <- nrow(sel_data())
    if (input$toggle_modelVals == TRUE) {
      model_p <- exp_data()[[1]][[1]][["fit"]][["spec"]][["period"]]
      updateNumericInput(session, "p", max = datasetLength, value = model_p)
    } else {
      updateNumericInput(session, "p", max = datasetLength)
    }
  })

  observe({
    if (input$toggle_modelVals == TRUE) {
      model <- exp_data()
      mod_a <- model[[1]][[1]][["fit"]][["par"]][["estimate"]][[1]]
      mod_b <- model[[1]][[1]][["fit"]][["par"]][["estimate"]][[2]]
      mod_g <- model[[1]][[1]][["fit"]][["par"]][["estimate"]][[3]]
      updateNumericInput(session, "a", value = mod_a)
      updateNumericInput(session, "b", value = mod_b)
      updateNumericInput(session, "g", value = mod_g)
    } else {
      updateNumericInput(session, "a", value = input$a)
      updateNumericInput(session, "b", value = input$b)
      updateNumericInput(session, "g", value = input$g)
    }
  })

  #### import data ####
  crime_data <- rio::import("https://byuistats.github.io/timeseries/data/baltimore_crime.csv", header=TRUE, stringsAsFactors=FALSE)

  crime_data <- crime_data %>%
    group_by(CrimeDate) %>%
    summarise(
      value = sum(Total.Incidents)
    )

  crime_data <- crime_data %>%
    mutate(dates = base::as.Date(CrimeDate, format = "%m/%d/%Y")) %>%
    filter(dates >= base::as.Date("2011-01-01", format = "%Y-%m-%d") & dates <= base::as.Date("2016-10-31", format = "%Y-%m-%d")) %>%
    arrange(dates)

  #crime_summary <- as.data.frame(crime_data)
  crime_summary <- crime_data %>%
    mutate(dates = yearmonth(floor_date(dates, "month"))) %>%
    group_by(dates) %>%
    summarise(
      value = sum(value)
    ) %>%
    mutate(average_value = value / days_in_month(dates)) %>%
    as_tsibble(index = dates)

  enrollment_ts <- rio::import("https://byuistats.github.io/timeseries/data/byui_enrollment.csv") |>
    mutate(dates = yearmonth( ym( paste(year, term * 4 - 3) ) ), value = enrollment ) |>
    dplyr::select(semester, dates, value) |>
    as_tsibble(index = dates)

  apple_ts <- rio::import("https://byuistats.github.io/timeseries/data/apple_revenue.csv") |>
    mutate(
      dates = mdy(date),
      year = lubridate::year(dates),
      quarter = lubridate::quarter(dates),
      value = revenue_billions
    ) |>
    dplyr::select(dates, year, quarter, value)  |>
    arrange(dates) |>
    mutate(dates = tsibble::yearquarter(dates)) |>
    as_tsibble(index = dates) |>
    dplyr::select(dates, year, quarter, value)

  #### event reactives ####
  # list of defined s's
  S_initial_list <- reactive({
    values <- input$S_list
    p <- input$p
    season_type <- input$season
    model <- exp_data()[[1]][[1]][["fit"]]
    if (input$toggle_modelVals){
      initialStates <- model[["states"]][[model[["spec"]][["period"]]+3]][1:model[["spec"]][["period"]]]
      S_initial_list <- rev(initialStates)
    } else {
      tryCatch({
        values <- as.numeric(strsplit(values, ",")[[1]])
        if (length(values)==p){
          S_initial_list <- values
        } else {
          if (season_type == "Additive"){
            S_initial_list <-rep(0, p)
          } else{
            S_initial_list <-rep(1, p)
          }
        }
      }, error = function(error) {
        if (season_type == "Additive"){
          S_initial_list <-rep(0, p)
        } else{
          S_initial_list <-rep(1, p)
        }
      })
    }
    S_initial_list
  })

  output$initial_list <- renderText({
    paste(S_initial_list())
  })

  #Reactive data input
  sel_data <- reactive({
    if (input$dataset_in == "Enrollment"){
      data <- enrollment_ts
    } else if (input$dataset_in == "Apple Revenue"){
      data <- apple_ts
    } else {
      data <- crime_summary
    }
    data
  })

  data_dates <- reactive({
    data <- sel_data()
    oldest_date <- min(data$dates, na.rm = TRUE)
    most_recent_date <- max(data$dates, na.rm = TRUE)
    return(
      list(
        oldest_date,
        most_recent_date
      )
    )

  })

  fil_data <- reactive({
    data <- sel_data()

    # Ensure the 'dates' column is of type Date
    #data <- data %>% mutate(dates = as.Date(dates))

    # Convert input dates from Shiny input to Date objects
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])

    # Filter the data based on the date range
    data <- data %>% dplyr::filter(dates >= start_date & dates <= end_date)
  })



  sim_data <- eventReactive(input$go,{
    data <- fil_data()
    forecast_dat <- holt_winters_forecast(data, "dates",input$value, alpha = input$a, input$b, gamma = input$g, p = input$p, predict_periods = input$periods,s_initial = S_initial_list(), round_to = "month", slope_type = "Additive", season_type = input$season)
    return(
      list(
        data <- forecast_dat
      )
    )
  })

  output$plot_fin<-renderPlot({
    data <- sim_data()[[1]]
    value_col <- sym(input$value)
    if (input$season == "Additive"){
      ggplot(data, aes(x = dates)) +
        geom_line(aes(y = !!value_col, color = "Base"), size = 1) +
        geom_line(aes(y = a_t + s_t, color = "Components", alpha=0.7),linetype=3, size = 1) +
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
    } else {
      ggplot(data, aes(x = dates)) +
        geom_line(aes(y = !!value_col, color = "Base"),linetype=3, size = 1) +
        geom_line(aes(y = a_t * s_t, color = "Components", alpha=0.7), size = 1) +
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
    }
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


  output$dattable <- renderDataTable({
    sim_data()[[1]]
  })

  output$debug <- renderText({
    paste0(" ")
  })


  output$preview_data <- renderTable({
    data <- fil_data()
    data$dates <- as.character(data$dates)
    data[1:5,]
    data <- fil_data()
    selected_val <- input$value

    # Rename the dynamically selected column to a fixed name, e.g., "target"
    data <- data %>%
      mutate(target = data[[selected_val]])
    data$dates <- as.character(data$dates)
    data[1:5,]
  })

  exp_data <- eventReactive(input$explore,{
    data <- fil_data()
    selected_val <- input$value

    #Rename the dynamically selected column to a fixed name, e.g., "target"
    data <- data %>%
      mutate(target = data[[selected_val]]) %>%
      as_tsibble(index = dates)

    # Now, you can use a fixed formula since the target column name is known
    season_type <- if (input$season == "Additive") "A" else "M"
    formula <- paste0("target ~ trend('A') + error('A') + season('", season_type, "')")

    data_hw <- data |>
      model(ExploreModel = ETS(!!as.formula(formula), opt_crit = "amse", nmse = 1))
    model <- report(data_hw)
  })

  output$preview <- renderPrint({
    req(exp_data())
    exp_data()
  })


  output$predplot<-renderPlot({
    data <- sim_data()[[1]]
    data <- filter(data, section != "H")
    if (input$season == "Additive"){
      ggplot(data, aes(x = date)) +
        geom_line(aes(y = x_t, color = "Base"), size = 1) +
        geom_line(aes(y = a_t + s_t, color = "Components", alpha=0.7), size = 1) +
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
    } else {
      ggplot(data, aes(x = date)) +
        geom_line(aes(y = x_t, color = "Base"), size = 1) +
        geom_line(aes(y = a_t * s_t, color = "Components", alpha=0.7), size = 1) +
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
    }
  })

}


# Run the application
shinyApp(ui, server)

