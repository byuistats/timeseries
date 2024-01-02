# Load packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, kableExtra,
               tsibble, fable,
               feasts, tsibbledata,
               fable.prophet,
               patchwork,
               ggthemes,
               see,   # okabeito color scheme
               MASS, # MVNorm
               ggokabeito, # colorblind palette
               kableExtra, # formatting tables
               stringr, # string manipulation
               lubridate # date manipulation
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
    knitr::kable(format = "html", align='cccccccccccccc', escape = FALSE, width = NA) %>%
    kable_styling(full_width = FALSE, "striped")
}

# Rounds a value to a specific number of places and returns a character string
round_as_text <- function(x, places) {
  return(as.character(round(x,12)))
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

numeric_2_char_df <- function(df, decimals = 3) {
  out_df <- df |>
    as.data.frame() |>
    mutate_if(is.numeric, round, digits=3) |>
    mutate(across(everything(), as.character))
  return(out_df)
}

concat_partial_table <- function(df, nrow_head, nrow_tail, decimals = 3) {
  temp_df <- numeric_2_char_df(df, decimals)

  out_df <- head(temp_df, nrow_head) |>
    bind_rows(row_of_vdots(temp_df)) |>
    bind_rows(tail(temp_df, nrow_tail))

  return(out_df)
}

#############################################