# for(seed_num in 1:5000) {
#
#   set.seed(seed_num)
#   n <- 10
#   k <- 2
#
#   x <- rep(10, n + k)
#   for(i in 2:length(x)) {
#     x[i] = x[i-1] + sample(-3:3, 1)
#   }
#
#   z <- sample(-2:2, n + k, replace = TRUE)
#   df <- data.frame(x = x, z = z) |>
#     mutate(y = round(1.5 * lag(x, k) + z), 0) |>
#     na.omit() |>
#     summarize(
#       x = mean(x),
#       y = mean(y)
#     )
#   if (floor(df$x[1]) == df$x[1] & floor(df$y[1]) == df$y[1]) {
#     print(seed_num)
#   }
# }

set.seed(547)
set.seed(607)
set.seed(672)
set.seed(707)
set.seed(815)
set.seed(920)
set.seed(949)
set.seed(1037)
set.seed(1038)
 set.seed(2887)
n <- 10
k <- 2

# x <- c(17, 18, 21, 23, 16, 17, 20, 23, 24, 21, 18, 17)

x <- rep(20, n + k)
for(i in 2:length(x)) {
  x[i] = x[i-1] + sample(-3:3, 1)
}

z <- sample(-2:2, n + k, replace = TRUE)
toy_df <- data.frame(x = x, z = z) |>
  mutate(y = round(1.5 * lag(x, k) + z - 15), 0) |>
  mutate(t = row_number()) |>
  na.omit() |>
  dplyr::select(t, x, y)

mean(toy_df$x)
mean(toy_df$y)

toy_ts <- toy_df |>
  mutate(
    dates = yearmonth( my(paste(row_number(), year(now()) - 1) ) )
  ) |>
  as_tsibble(index = dates)


toy_ts |>
  autoplot(.vars = x) +
  geom_line(data = toy_ts, aes(x = dates, y = y), color = "#E69F00") +
  labs(
    x = "Time",
    y = "Value of x (in black) and y (in orange)",
    title = paste0("Two Time Series Illustrating a Lag")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

toy_ts |>
  CCF(x = x, y = y) |>
  autoplot()

