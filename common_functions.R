# Load packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggokabeito, kableExtra)

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

## Round places ----
round_to_places <- function(x, places) {
  # Initialize output
  out <- rep_len("", length(x))

  # Handle NAs
  out[is.na(x)] <- NA

  # Handle infinities
  inf <- is.infinite(x)
  out[inf & x > 0] <- "Inf"
  out[inf & x < 0] <- "-Inf"

  # Special cases
  finite <- !inf & !(is.na(x))

  # Round numbers
  x <- round(x, places)

  # Format numbers
  ints <- trunc(x)
  decs <- round(abs(x - ints) * 10^places)

  if (places > 0) {
    out[finite] <- paste0(ints[finite], ".", decs[finite], strrep(0, places - nchar(as.integer(decs[finite]))))
  } else {
    out[finite] <- as.character(ints[finite])
  }

  return(out)
}


# Subsetting ----

## Extract first row ?? ----
delete_all_but_first_row <- function(df, skip_columns = 3, skip_rows = 1){
  df_dimension <- dim(df)
  for (i in (1 + skip_rows):df_dimension[1])
    for (j in (1 + skip_columns):df_dimension[2]) {
      df[i,j] <- " "
    }
  return(df)
}


