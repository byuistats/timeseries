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


nat_gas_ts |>
  autoplot(.vars = wet_bcf)
nat_gas_ts |>
  autoplot(.vars = dry_bcf)
nat_gas_ts |>
  autoplot(.vars = withdrawals_bcf)

nat_gas_ts2 <- nat_gas_ts |>
  filter(month >= yearmonth(my("Jan 1990"))) |>
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
  as_tsibble(index = TIME)

nat_gas_ts2 |>
  autoplot(.vars = imports_bcf)

full_lm <- nat_gas_ts2 |>
  model(full_cubic = TSLM(imports_bcf ~ std_t + I(std_t^2) + I(std_t^3) + I(std_t^4)
                          + I(std_t^5) + I(std_t^6) + I(std_t^7) + I(std_t^8)
                          + I(std_t^9) + I(std_t^10) + I(std_t^11) + I(std_t^12)
                          + sin1 + cos1 + sin2 + cos2 + sin3 + cos3
                          + sin4 + cos4 + sin5 + cos5 + cos6 ))

full_lm |>
  tidy() |>
  mutate(sig = p.value < 0.05) |>
  display_table()
