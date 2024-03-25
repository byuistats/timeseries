if (!require("pacman")) install.packages("pacman")
pacman::p_load(MASS, # MVNorm, loaded before tidyverse so it doesn't overwrite dplyr::select()
               nlme,
               tidymodels,    # for GLS
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
               gt, # grammar of tables, for LaTeX in tables
               quarto, # For LaTeX mode results = 'asis'
               plotly, # For interactive figures
               multilevelmod, # for GLS
               broom.mixed,    # for GLS
               readxl
)

# Read in data
nat_gas_ts <- read_excel(
  path = "data/natural_gas_ngm01vmall.xls",
  sheet = "Data 1",
  skip = 2
  ) |>
  rename(
    wet_bcf = "U.S. Natural Gas Marketed Production (Wet) (Bcf)",
    dry_bcf = "U.S. Dry Natural Gas Production (Bcf)",
    withdrawals_bcf = "U.S. Natural Gas Net Withdrawals from Storage (Bcf)",
    imports_bcf = "U.S. Natural Gas Net Imports (Bcf)"
  ) |>
  mutate(month = yearmonth(ymd(Date))) |>
  select(month, wet_bcf, dry_bcf, withdrawals_bcf, imports_bcf) |>
  as_tsibble(index = month)

# Look at different variables in the data set
nat_gas_ts |>
  autoplot(.vars = wet_bcf)
nat_gas_ts |>
  autoplot(.vars = dry_bcf)

nat_gas_ts |>
  filter(month >= yearmonth(my("Jan 1990"))) |>
  autoplot(.vars = imports_bcf)

nat_gas_ts |>
  autoplot(.vars = withdrawals_bcf)


# Create final TS
nat_gas_ts2 <- nat_gas_ts |>
  mutate(
    TIME = 1:n(),
    std_t = (TIME - mean(TIME)) / sd(TIME)
  ) |>
  mutate(
    cos1 = cos(2 * pi * 1 * TIME/12),
    cos2 = cos(2 * pi * 2 * TIME/12),
    cos3 = cos(2 * pi * 3 * TIME/12),
    cos4 = cos(2 * pi * 4 * TIME/12),
    cos5 = cos(2 * pi * 5 * TIME/12),
    cos6 = cos(2 * pi * 6 * TIME/12),
    sin1 = sin(2 * pi * 1 * TIME/12),
    sin2 = sin(2 * pi * 2 * TIME/12),
    sin3 = sin(2 * pi * 3 * TIME/12),
    sin4 = sin(2 * pi * 4 * TIME/12),
    sin5 = sin(2 * pi * 5 * TIME/12),
    sin6 = sin(2 * pi * 6 * TIME/12)) |>
  as_tsibble(index = TIME) |>
  rename(x = withdrawals_bcf)

# Plot final TS
nat_gas_ts2 |>
  autoplot(.vars = x)

# Find model for TS
final_lm <- nat_gas_ts2 |>
  model(full_cubic = TSLM(x ~ std_t
                          # + I(std_t^2) + I(std_t^3) + I(std_t^4)
                          # + I(std_t^5) + I(std_t^6) + I(std_t^7) + I(std_t^8)
                          # + I(std_t^9) + I(std_t^10) + I(std_t^11) + I(std_t^12)
                          + sin1 + cos1 + sin2 + cos2 + sin3 + cos3
                          # + sin4 + cos4 + sin5 + cos5 + cos6
                          )
        )

# Parameters for final model
final_lm |>
  tidy() |>
  mutate(sig = p.value < 0.05) |>
  display_table()


# plot residuals
final_lm |>
  residuals() |>
  autoplot()

final_lm |>
  residuals() |>
  na.omit() |>
  acf()

final_lm |>
  residuals() |>
  na.omit() |>
  pacf()

# Fit ARMA model to residuals
model_resid <- final_lm |>
  residuals() |>
  select(-.model) |>
  model(
    auto = ARIMA(.resid ~ 1 + pdq(0:3,0,0:3) + PDQ(0, 0, 0)),
    #
    # model000 = ARIMA(.resid ~ 1 + pdq(0,0,3) + PDQ(0, 0, 0)),
    model100 = ARIMA(.resid ~ 1 + pdq(1,0,0) + PDQ(0, 0, 0)),
    model200 = ARIMA(.resid ~ 1 + pdq(2,0,0) + PDQ(0, 0, 0)),
    model300 = ARIMA(.resid ~ 1 + pdq(3,0,0) + PDQ(0, 0, 0)),
    #
    model001 = ARIMA(.resid ~ 1 + pdq(0,0,1) + PDQ(0, 0, 0)),
    model101 = ARIMA(.resid ~ 1 + pdq(1,0,1) + PDQ(0, 0, 0)),
    model201 = ARIMA(.resid ~ 1 + pdq(2,0,1) + PDQ(0, 0, 0)),
    model301 = ARIMA(.resid ~ 1 + pdq(3,0,1) + PDQ(0, 0, 0)),
    #
    model002 = ARIMA(.resid ~ 1 + pdq(0,0,2) + PDQ(0, 0, 0)),
    model102 = ARIMA(.resid ~ 1 + pdq(1,0,2) + PDQ(0, 0, 0)),
    model202 = ARIMA(.resid ~ 1 + pdq(2,0,2) + PDQ(0, 0, 0)),
    model302 = ARIMA(.resid ~ 1 + pdq(3,0,2) + PDQ(0, 0, 0)),
    #
    model003 = ARIMA(.resid ~ 1 + pdq(0,0,3) + PDQ(0, 0, 0)),
    model103 = ARIMA(.resid ~ 1 + pdq(1,0,3) + PDQ(0, 0, 0)),
    model203 = ARIMA(.resid ~ 1 + pdq(2,0,3) + PDQ(0, 0, 0)),
    # model303 = ARIMA(.resid ~ 1 + pdq(3,0,3) + PDQ(0, 0, 0)),
  )

# prepare to identify "best" models
combined_models <- glance(model_resid) |>
  select(.model, AIC, AICc, BIC)
minimum <- combined_models |>
  summarize(
    AIC = which(min(AIC)==AIC),
    AICc = which(min(AICc)==AICc),
    BIC = which(min(BIC)==BIC)
  )
model_resid |>
  glance() |>
  rename(Model = ".model") |>
  round_df(1) |>
  format_cells(rows = minimum$AIC, cols = 4, "bold") |>
  format_cells(rows = minimum$AICc, cols = 5, "bold") |>
  format_cells(rows = minimum$BIC, cols = 6, "bold") |>
  display_table()

model_resid |>
  glance() |>
  filter(AIC == min(AIC) | AICc == min(AICc) | BIC == min(BIC) | .model == "auto")

# To determine what "auto" model is
model_resid |>
  select(.model = "auto")

# Look at acf and pacf of chosen model
model_resid |>
  select(.model = "model201") |>
  residuals() |>
  na.omit() |>
  acf()

model_resid |>
  select(.model = "model201") |>
  residuals() |>
  na.omit() |>
  pacf()



#
#
#
#
# model_best |>
#   residuals() |>
#   ACF() |>
#   autoplot()
#
# new_data <- tibble(
#   date = seq(
#     max(cbe_ts$date) + months(1),
#     max(cbe_ts$date) + months(36),
#     by = "1 months"),
#   month = tsibble::yearmonth(date),
#   time = seq(nrow(cbe_ts), length = 36),
#   imth = rep(1:12, 3)) |>
#   as_tsibble(index = month)
#
# plot_dat <- new_data |>
#   mutate(
#     residuals = forecast(
#       model_best,
#       h = "3 years") |>
#       pull(.mean),
#     expected_log = forecast(
#       elec_lm,
#       new_data = new_data) |>
#       pull(.mean),
#     expected = exp(expected_log + residuals)
#   )
#
# ggplot() +
#   geom_line(data = cbe_ts, aes(x = date, y = elec)) +
#   geom_line(data = plot_dat, aes(x = date, y = expected), linetype = 2)