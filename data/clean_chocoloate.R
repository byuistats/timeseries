# Read in Chocolate data

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tsibble", "fable",
               "feasts", "tsibbledata",
               "fable.prophet", "tidyverse",
               "patchwork", "rio")

chocolate_month <- rio::import("data/chocolate.csv")
start_date <- lubridate::ymd("2004-01-01")
date_seq <- seq(start_date,
                start_date + months(nrow(chocolate_month)-1),
                by = "1 months")
chocolate <- tibble(
  dates = date_seq,
  year = lubridate::year(date_seq),
  month = lubridate::month(date_seq),
  value = pull(chocolate_month, chocolate)
)
chocolate_month_ts <- chocolate |>
  mutate(index = tsibble::yearmonth(dates)) |>
  as_tsibble(index = index)

chocolate_annual_ts <- summarise(index_by(chocolate_month_ts, year), value = mean(value))
chocolate_annual_ts

# TS Plot (Monthly and Annual)

mp <- autoplot(chocolate_month_ts, .vars = value) +
  labs(
    y = "Relative Searches for Chocolate",
    title = "Relative Number of Google Searches for Chocolate"
  )
yp <- autoplot(chocolate_annual_ts) +
  labs(y = "Relative Searches for Chocolate") +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2))
mp / yp


# Decomposition

choc_decompose <- chocolate_month_ts |>
  model(feasts::classical_decomposition(value,
                                        type = "add"))  |>
  components()
autoplot(choc_decompose)

choc_decompose |>
  ggplot(aes(x = index)) +
  geom_line(aes(y = trend)) +
  # geom_line(aes(y = value), linetype = 1, color = "#E69F00") +
  geom_line(aes(y = trend + seasonal), linetype = 2, color = "#56B4E9")
