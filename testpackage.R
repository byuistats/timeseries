library(lubridate)
library()
# Set random seed for reproducibility
set.seed(20)
source("common_functions.R")
# Set parameters & initialize vectors
num_years <- 10
n <- 12 * num_years
sigma <- .75
a <- 0.05
b <- 1
c <- 0.5
trend <- seasonal <- x_t <- rep(0,n)
time_seq <- seq(1,n)

# Generate correlated error terms
w <- rnorm(n + 4, 0, 1)
z = w + lead(w,1) + lead(w,2) + lead(w,3) + lead(w,4)
z  = head(z, n) / 2

# Get date
year_seq <- lubridate::year(today()) - num_years  + (time_seq - 1) %/% 12
month_seq <- (time_seq - 1) %% 12 + 1
date_seq <- ymd(paste0(year_seq,"-",month_seq,"-01"))

# Get data
for (t in 1:n) {
  trend[t] <- a * t + 10
  seasonal[t] <- b * sin(t / 12 * 2 * pi * 1)  + c * cos(t / 12 * 2 * pi * 3)
  x_t[t] <- trend[t] + seasonal[t] + z[t]
}

x_df <- data.frame(x_t = x_t, trend = trend, seasonal = seasonal)

start_year <- lubridate::year(today()) - num_years
start_date <- lubridate::ymd(paste0(start_year,"-01-01"))

# start_date <- lubridate::ymd("1958-01-01")
date_seq <- seq(start_date,
                start_date + months(nrow(x_df)-1),
                by = "1 months")

x_df_ts <- x_df |>
  mutate(
    date = date_seq,
    month = tsibble::yearmonth(date)
  ) |>
  select(date, month, trend, seasonal, x_t) |>
  as_tsibble(index = month)

x_decompose <- x_df_ts |>
  model(feasts::classical_decomposition(x_t,
                                        type = "add"))  |>
  components()

autoplot(x_decompose)




library(dplyr)
library(timetk)

x_df_t <- x_df |>
  mutate(
    date = date_seq)

x_df_t<- x_df_t[c(1:100, 105:120),]

x_df_t %>%
  plot_stl_diagnostics(date, x_t,
                       .frequency = "auto", .trend = "auto",
                       .interactive = TRUE)
