# Load packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(MASS, # MVNorm, loaded before tidyverse so it doesn't overwrite dplyr::select()
               tidyverse, kableExtra,
               tsibble, fable,
               feasts, tsibbledata,
               fable.prophet,
               patchwork,
               ggthemes,
               see,   # okabeito color scheme
               ggokabeito, # colorblind palette
               stringr, # string manipulation
               lubridate, # date manipulation
               rio, # for easy i/o
               tidyquant,
               gt # grammar of tables, for LaTeX in tables
)
################# WARNING: DO NOT USE mosaic. IT MESSES UP THE DECOMPOSITION.

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

# Rounding ----
## Round Dataframe ----
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
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
