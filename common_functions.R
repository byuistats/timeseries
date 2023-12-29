# Load packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggokabeito, kableExtra,
               tsibble, fable,
               feasts, tsibbledata,
               fable.prophet, tidyverse,
               patchwork,
               ggthemes, see,   # for okabeito color scheme
               stringr,
               lubridate,
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
    knitr::kable(format = "html", align='ccccccccc', escape = FALSE, width = NA) %>%
    kable_styling(full_width = FALSE, "striped")
}



