# Load packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  # Interactive plots
  plotly, # Interactive visualizations, loaded before tidyverse so it overwrite dplyr::select(). Note: High conflict Potential

  # Core packages
  MASS, # MVNorm, loaded before tidyverse so it doesn't overwrite dplyr::select()
  tidyverse, # This will also load the dependencies; dplyr, readr, stringr, tibble, tidyr, purrr, forcats, gglot2, & lubridate

  # Data manipulation
  tsibble, # Tidyverse Temporal data
  tsibbledata, # Sample Tsibble datasets

  # Statistical modeling (GLS - Chpt 6-7)
  nlme, # loaded before feasts to avoid ACF() conflict
  tidymodels, # for GLS, This will also load the dependencies; broom, rsample, dials, tune, infer, workflows, modeldata, workflowsets, parsnip, yardstick, & recipies. Note: High conflict Potential
  multilevelmod, # for GLS
  broom.mixed, # for GLS

  # TS modeling and forecasting
  fable,# Forecasting Models for Tidy Time Series, Note: High conflict Potential
  feasts, # collection of features, decomposition methods, statistical summaries and graphics for tsibble data, Loaded after nlme to avoid ACF() conflict
  fable.prophet, # Converts prophet (forecasting) package for fable workflow

  # Data exploration & visualization
  patchwork, # Multiple plot outputs
  ggthemes, # Plot styling
  see,  # okabeito color scheme
  ggokabeito,  # colorblind palette

  # Reporting & output
  kableExtra, # Create nice-looking tables from data.frames
  rio, # Easy import/export of data between R and other software
  gt, # Grammar of Tables for advanced table creation
  quarto, # For generating reports in LaTeX format

  # Additional packages
  tidyquant # Quantitative analysis tools using tidyverse principles, This will also load the dependencies; PerformanceAnalytics, xts, & zoo. Important Masks: ‘package:base’: as.Date, as.Date.numeric. Note: High conflict Potential
)
################# WARNING: DO NOT USE mosaic. IT MESSES UP THE DECOMPOSITION. #need to test loading this in the front, might be ok

# Palette ----
palette("okabeito")

okabeito_colors_list <- c(
  `orange` = "#E69F00",
  `light blue` = "#56B4E9",
  `green` = "#009E73",
  `yellow` = "#F0E442",
  `blue` = "#0072B2",
  `red` = "#D55E00",
  `purple` = "#CC79A7",
  `grey` = "#999999",
  `black` = "#000000",
  `sky blue` = "#56B4E9",
  `bluish green` = "#009E73",
  `vermillion` = "#D55E00",
  `reddish purple` = "#CC79A7",
  `dark yellow` = "#F5C710",
  `amber` = "#F5C710"
)

# Definition
emdash <- "—"
endash <- "–"

# Rounding ----
## Round Dataframe ----
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  return(df)
}


# THIS IS NOT WORKING RIGHT>>>>
autoround_df <- function(df) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = 6 + floor(-log(abs(min(df[,nums])), base = 10)))
  # return(df)

  return(df |> round_df(1))
}

# Used to create a table with ellipses in the middle

row_of_vdots <- function(df) {
  temp_df <- df |>
    # mutate(across(everything(), as.character)) |>
    head(1)

  for (j in 1:ncol(temp_df)) {
    if (names(temp_df[j]) == "sign") {
      temp_df[1,j] = " "
    } else {
      temp_df[1,j] = "⋮"
    }
  } # for

  return(temp_df)
}

concat_partial_table <- function(df, nrow_head, nrow_tail, decimals = 3) {
  temp_df <- convert_df_to_char(df, decimals)

  out_df <- head(temp_df, nrow_head) |>
    bind_rows(row_of_vdots(temp_df)) |>
    bind_rows(tail(temp_df, nrow_tail))

  return(out_df)
}

display_partial_table <- function(df, nrow_head, nrow_tail, decimals = 3, min_col_width = "0in") {
  concat_partial_table(df, nrow_head, nrow_tail, decimals) |>
    display_table(min_col_width)
}

display_table <- function(df, min_col_width = "0in") {
  df |>
    knitr::kable(format = "html", align='ccccccccccccccccc', escape = FALSE, width = NA, row.names = FALSE) |>
    kable_styling(full_width = FALSE, "striped") |>
    column_spec(1:ncol(df), width_min = min_col_width)
}

display_arima_models <- function(models_ts) {
  # Identify "best" models
  extrema <- models_ts |>
    reframe(
      sigma2 = which(min(sigma2)==sigma2),
      log_lik = which(max(log_lik)==log_lik),
      AIC = which(min(AIC)==AIC),
      AICc = which(min(AICc)==AICc),
      BIC = which(min(BIC)==BIC)
    )
  # Format the table
  models_ts |>
    select(-ar_roots, -ma_roots) |>
    rename(Model = ".model") |>
    autoround_df() |>
    format_cells(rows = unique(extrema$sigma2), cols = 2, "bold") |>
    format_cells(rows = unique(extrema$log_lik), cols = 3, "bold") |>
    format_cells(rows = unique(extrema$AIC), cols = 4, "bold") |>
    format_cells(rows = unique(extrema$AICc), cols = 5, "bold") |>
    format_cells(rows = unique(extrema$BIC), cols = 6, "bold") |>
    display_table()
}

# Rounds a value to a specific number of places and returns a character string
round_as_text <- function(x, places) {
  return(as.character(round(x,12)))
}

# Converts a dataframe to char and rounds the values to a specified number of places
convert_df_to_char <- function(df, decimals = 3) {
  out_df <- df |>
    as.data.frame() |>
    mutate_if(is.numeric, round, digits=decimals) |>
    mutate(across(everything(), as.character))
  return(out_df)
}

# Change a df to character, round, and set one specific value to ""
blank_out_one_cell_in_df <- function(df, row_num, col_num, decimals = 3) {
  out_df <- df |>
    convert_df_to_char(decimals)

  out_df[row_num, col_num] <- ""

  return(out_df)
}

# Returns "" for all cells except the first ncols_to_keep columns and nrows_to_keep rows
# Numeric values are rounded to "decimals" places
blank_out_cells_in_df <- function(df, ncols_to_keep = 2, nrows_to_keep = 0, decimals = 3) {
  out_df <- df |>
    convert_df_to_char(decimals)

  for (i in (nrows_to_keep + 1) : nrow(df))
    for (j in (ncols_to_keep + 1) : ncol(df)) {
      out_df[i,j] <- ""
    }
  return(out_df)
}


# Returns "" for all cells except the first ncols_to_keep columns and nrows_to_keep rows
# Numeric values are rounded to "decimals" places
blank_out_partial_row <- function(df, row_number = nrow(df), first_column_number = 2, last_column_number = ncol(df), decimals = 3) {
  out_df <- df |>
    convert_df_to_char(decimals)

  for (j in first_column_number:last_column_number) {
    out_df[row_number,j] <- ""
  }

  return(out_df)
}


# Replace all NAs with a character
replace_na_with_char <- function(df, new_character = "", decimals = 3) {
  out_df <- df |>
    convert_df_to_char(decimals) |>
    mutate(
      across(everything(), ~replace_na(.x, new_character))
    )
  return(out_df)
}

## This is the preferred function for replacing text with any character
replace_cells_with_char <- function(df, rows, cols, new_char = "", decimals = 3){
  for (r in rows){
    for(c in cols){

      # Make sure values are not factors
      df[[c]] <- as.character( df[[c]])

      # Update formatting
      df[r, c] <- new_char
    }
  }

  return(df)
}

#################################### USE THIS CODE TO REVISE MY PREVIOUS FUNCTIONS! ###############################

###### Format text cells in a data frame
#
# https://stackoverflow.com/questions/28166168/how-to-change-fontface-bold-italics-for-a-cell-in-a-kable-table-in-rmarkdown
format_cells <- function(df, rows ,cols, value = c("italics", "bold", "strikethrough")){

  # select the correct markup
  # one * for italics, two ** for bold
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]

  for (r in rows){
    for(c in cols){

      # Make sure values are not factors
      df[[c]] <- as.character( df[[c]])

      # Update formatting
      df[r, c] <- paste0(markup, df[r, c], markup)
    }
  }

  return(df)
}
#### Example of use:
# library(tidyverse)
#
# df <- data.frame(char = c('a','b','c'),
#                  num = c(1,2,3))
#
# df %>%
#   format_cells(1, 1, "italics") %>%
#   format_cells(2, 2, "bold") %>%
#   format_cells(3, 1:2, "strikethrough") %>%
#   knitr::kable()

###### Compute sum or mean of numeric variables in a df

append_sum_to_df <- function(df, label = "Sum") {
  df <- df %>%
    bind_rows(summarise_all(., ~if(is.numeric(.)) sum(., na.rm = TRUE) else label))
  return(df)
}

append_mean_to_df <- function(df, label = "Mean") {
  df <- df %>%
    bind_rows(summarise_all(., ~if(is.numeric(.)) mean(., na.rm = TRUE) else label))
  return(df)
}

sum_of_columns <- function(df, label = "Sum") {
  row <- df %>%
    summarise_all(., ~if(is.numeric(.)) sum(., na.rm = TRUE) else label)
  return(row)
}

mean_of_columns <- function(df, label = "Mean") {
  row <- df %>%
    summarise_all(., ~if(is.numeric(.)) mean(., na.rm = TRUE) else label)
  return(row)
}

sum_of_columns_divided_by_n <- function(df, label, n = nrow(df)) {
  row <- df %>%
    summarise_all(., ~if(is.numeric(.)) sum(., na.rm = TRUE)/n else label)
  return(row)
}



insert_blank_last_row <- function(df, label = "sum", value = "", decimals = 3) {
  temp_df <- df |>
    bind_rows(df |> tail(1))
  convert_df_to_char(decimals)
  num_rows <- nrow(temp_df)
  temp_df[num_rows, ] <- value
  temp_df[num_rows, 1] <- label
  return(temp_df)
}


########### Compute moving average
compute_moving_average <- function(df, variable, periods = 12) {
  sum <- 0

  for (i in 1:(periods / 2)) {
    sum <- sum + lead(eval(substitute(variable), df), i)           # x_{t+i}
    sum <- sum + lag(eval(substitute(variable), df), i)            # x_{t-i}
  }
  sum <- sum + eval(substitute(variable), df)
  sum <- sum - lag(eval(substitute(variable), df), periods / 2) / 2
  sum <- sum - lead(eval(substitute(variable), df), periods / 2) / 2
  df$m_hat <- sum / periods
  return(df)
}

# # # # # # # # # # # # # Testing
# set.seed(1)
# x <- rnorm(100,5,2)
# df1 <- data.frame(x=x)
#
# df1 |>
#   compute_moving_average(x, 12) |>
# mutate( m_hat2 =
#           ( 1 / 2 * lag(x,6) + lag(x,5) + lag(x,4) + lag(x,3) + lag(x,2) + lag(x) + x + lead(x,1) + lead(x,2) + lead(x,3) + lead(x,4) + lead(x,5) + 1 / 2 * lead(x,6)) /12
# ) |> View()


########### String manipulation

# Returns "char" right-most characters of "string"
right <- function (string, char) {
  substr(string, nchar(string)-(char-1), nchar(string))
}

# Returns "char" left-most characters of "string"
left <- function (string, char) {
  substr(string, 1, char)
}

##########################################################################
# These functions color cells in a data frame of character vectors
# Works
color_specific_cell <- function(df, row_num, col_num, color) {
  df[row_num, col_num] = cell_spec(df[row_num, col_num], color = color)
  return(df)
}

# -------- THIS ONE IS NOT WORKING ----------
color_specific_row <- function(df, row_num, color) {
  for (j in 1:ncol(df)) {
    color_specific_cell(df, row_num, j, color)
  }
  return(df)
}

# -------- THIS ONE IS NOT WORKING ----------
color_last_row <- function(df, color) {
  color_specific_row(df, nrow(df), color)
  return(df)
}

# Works
color_last_row2 <- function(df, color) {
  for (j in 1:ncol(df)) {
    df[nrow(df), j] = cell_spec(df[nrow(df), j], color = color)
  }
  return(df)
}
# Works
color_2nd_to_last_row2 <- function(df, color) {
  for (j in 1:ncol(df)) {
    df[nrow(df)-1, j] = cell_spec(df[nrow(df)-1, j], color = color)
  }
  return(df)
}
##########################################################################



#############################################

#

get_toy_data <- function(n = 10, mu = 0, sigma = 3, rho = 0.99, random_seed = 997) {
  set.seed(random_seed)

  # build population correlation matrix
  tmp.r <- matrix(rho, n, n)
  tmp.r <- tmp.r^abs(row(tmp.r)-col(tmp.r))

  return( round(mvrnorm(1, rep(mu,n), sigma^2 * tmp.r),1) )
}


#############################################

deg2rad <- function (x)
{
  x/180 * base::pi
}

rad2deg <- function (x)
{
  x/base::pi * 180
}




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



expand_holt_winters_df_old <- function(df, date_var, value_var, p = 12, predict_periods = 18) {
  # Note that p must be < nrow(df)
  # Note that predict_periods must be < nrow(df)

  # Create new variables
  df$date <- df[[date_var]]
  df$t <- 1:nrow(df)  # Create column t
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
      t = t - p,
      x_t = NA
    ) |>
    head(p)

  footer <- df2 |>
    tail(predict_periods + 1) |>
    mutate(
      date = date - min(date) + max(date),
      t = t + predict_periods,
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
  df <- df |> expand_holt_winters_df_old(date_var, value_var, p, predict_periods)

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


###### For Chapter 3 Lesson 5
hw_additive_slope_multiplicative_seasonal <- function(df, date_var, value_var, p = 12, predict_periods = 18, alpha = 0.2, beta = 0.2, gamma = 0.2, s_initial = rep(1,p)) {

  # Get expanded data frame
  df <- df |> expand_holt_winters_df_old(date_var, value_var, p, predict_periods)

  # Fill in prior belief about s_t
  for (t in 1:p) {
    df$s_t[t] <- s_initial[t]
  }

  # Fill in first row of values
  offset <- p # number of header rows to skip
  df$a_t[1 + offset] <- df$x_t[1 + offset]
  df$b_t[1 + offset] <- (1 / p) * mean(df$x_t[(p + 1 + offset):(2 * p + offset)] - df$x_t[(1 + offset):(p + offset)])
  df$s_t[1 + offset] <- df$s_t[1]

  # Fill in remaining rows of body of df with values
  for (t in (2 + offset):(nrow(df) - predict_periods) ) {
    df$a_t[t] = alpha * (df$x_t[t] / df$s_t[t-p]) + (1 - alpha) * (df$a_t[t-1] + df$b_t[t-1])
    df$b_t[t] = beta * (df$a_t[t] - df$a_t[t-1]) + (1 - beta) * df$b_t[t-1]
    df$s_t[t] = gamma * (df$x_t[t] / df$a_t[t]) + (1 - gamma) * df$s_t[t-p]
  }

  df <- df |>
    mutate(k = ifelse(row_number() >= nrow(df) - predict_periods, row_number() - (nrow(df) - predict_periods), NA))

  # Fill in forecasted values
  offset <- nrow(df) - predict_periods
  for (t in (offset+1):nrow(df)) {
    df$s_t[t] = df$s_t[t - p]
    df$xhat_t[t] = (df$a_t[offset] + df$k[t] * df$b_t[offset]) * df$s_t[t - p]
  }
  df$xhat_t[offset] = (df$a_t[offset] + df$k[offset] * df$b_t[offset]) * df$s_t[offset] #### NOTE THIS ISSUE!!!

  # Delete temporary variable k
  df <- df |> select(-k)

  return(df)
}

################## Holt-Winters ####################

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

  #df$b_t[actual_start] <- mean(diff(df$x_t[actual_start:(actual_start + p - 1)])) / p #chat gpt code doesn't calc correctly
  #df$b_t[actual_start] <- mean(df$x_t[(actual_start + p):(3 * p)] - df$x_t[(actual_start):(p + p)]) / p # simplified Bro Johnson code
  df$b_t[actual_start] <- (df$x_t[actual_start] - roll_mean) / df$x_t[actual_start] # period 4 of 7 period rolling average (Bro Moncayo)
  df$s_t[actual_start] <- df$s_t[1]



  ### Fill Body
  #Add + Add
  # for (t in (actual_start + 1):(nrow(df) - predict_periods)) {
  #   prior_seasonal_index <- ifelse(t <= actual_start + p, t - actual_start, t - p)
  #   df$a_t[t] <- alpha * (df$x_t[t] - df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t - 1] + df$b_t[t - 1])
  #   df$b_t[t] <- beta * (df$a_t[t] - df$a_t[t - 1]) + (1 - beta) * df$b_t[t - 1]
  #   df$s_t[t] <- gamma * (df$x_t[t] - df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index]
  # }
  #
  # #Add + Mult
  # for (t in (2 + actual_start):(nrow(df) - predict_periods) ) {
  #   prior_seasonal_index <- ifelse(t <= actual_start + p, t - actual_start, t - p)
  #   df$a_t[t] = alpha * (df$x_t[t] / df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t-1] + df$b_t[t-1])
  #   df$b_t[t] = beta * (df$a_t[t] - df$a_t[t-1]) + (1 - beta) * df$b_t[t-1]
  #   df$s_t[t] = gamma * (df$x_t[t] / df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index]
  # }

  for (t in (1 + actual_start):(nrow(df) - predict_periods) ) {
    prior_seasonal_index <- ifelse(t <= actual_start + p, t - actual_start, t - p)
    if (slope_type == "add" & season_type == "add"){
      df$a_t[t] <- alpha * (df$x_t[t] - df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t - 1] + df$b_t[t - 1])
      df$b_t[t] <- beta * (df$a_t[t] - df$a_t[t - 1]) + (1 - beta) * df$b_t[t - 1]
      df$s_t[t] <- gamma * (df$x_t[t] - df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index]# add slope & add season
    } else if (slope_type == "add" & season_type == "mult"){
      df$a_t[t] = alpha * (df$x_t[t] / df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t-1] + df$b_t[t-1])
      df$b_t[t] = beta * (df$a_t[t] - df$a_t[t-1]) + (1 - beta) * df$b_t[t-1]
      df$s_t[t] = gamma * (df$x_t[t] / df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index] # add slope & mult season
    } else if (slope_type == "mult" & season_type == "add"){
      df$a_t[t] = alpha * (df$x_t[t] + df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t-1] + df$b_t[t-1])
      df$b_t[t] = beta * (df$a_t[t] / df$a_t[t-1]) + (1 - beta) * df$b_t[t-1]
      df$s_t[t] = gamma * (df$x_t[t] + df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index] # mult slope & add season
    } else {
      df$a_t[t] = alpha * (df$x_t[t] / df$s_t[prior_seasonal_index]) + (1 - alpha) * (df$a_t[t-1] + df$b_t[t-1])
      df$b_t[t] = beta * (df$a_t[t] / df$a_t[t-1]) + (1 - beta) * df$b_t[t-1]
      df$s_t[t] = gamma * (df$x_t[t] / df$a_t[t]) + (1 - gamma) * df$s_t[prior_seasonal_index] # mult slope & mult season
    }
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



