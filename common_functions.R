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
               tidyquant
)

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


# Displays the kable table
display_table <- function(df) {
  df %>%
    knitr::kable(format = "html", align='cccccccccccccc', escape = FALSE, width = NA, row.names = FALSE) %>%
    kable_styling(full_width = FALSE, "striped")
}

# Rounds a value to a specific number of places and returns a character string
round_as_text <- function(x, places) {
  return(as.character(round(x,12)))
}

# Converts a dataframe to char and rounds the values to a specified number of places
convert_df_to_char <- function(df, decimals = 3) {
  out_df <- df %>%
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
# df1 %>%
#   compute_moving_average(x, 12) %>%
# mutate( m_hat2 =
#           ( 1 / 2 * lag(x,6) + lag(x,5) + lag(x,4) + lag(x,3) + lag(x,2) + lag(x) + x + lead(x,1) + lead(x,2) + lead(x,3) + lead(x,4) + lead(x,5) + 1 / 2 * lead(x,6)) /12
# ) %>% View


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


display_partial_table <- function(df, nrow_head, nrow_tail, decimals = 3) {
  concat_partial_table(df, nrow_head, nrow_tail, decimals) |>
  display_table()
}

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


