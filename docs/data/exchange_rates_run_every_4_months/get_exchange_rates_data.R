# Get packages, if needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quantmod)
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
               gt, # grammar of tables, for LaTeX in tables
               quarto, # For LaTeX mode results = 'asis'
               plotly # For interactive figures
)


# Read in existing data from previous downloads
usd1_exchange_prior <- rio::import("data/exchange_rates.parquet")


jpy <- getSymbols('USD/JPY', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
eur <- getSymbols('USD/EUR', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
gbp <- getSymbols('USD/GBP', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
aud <- getSymbols('USD/AUD', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
cad <- getSymbols('USD/CAD', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
chf <- getSymbols('USD/CHF', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
cny <- getSymbols('USD/CNY', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
nzd <- getSymbols('USD/NZD', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
hkd <- getSymbols('USD/HKD', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
mxn <- getSymbols('USD/MXN', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
brl <- getSymbols('USD/BRL', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
clp <- getSymbols('USD/CLP', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
ves <- getSymbols('USD/VES', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
ars <- getSymbols('USD/ARS', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
rub <- getSymbols('USD/RUB', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
try <- getSymbols('USD/TRY', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
pkr <- getSymbols('USD/PKR', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
krw <- getSymbols('USD/KRW', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
zar <- getSymbols('USD/ZAR', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
inr <- getSymbols('USD/INR', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)
uah <- getSymbols('USD/UAH', src='oanda', from=Sys.Date()-180,
                  to=Sys.Date(), auto.assign=FALSE)

usd1_exchange <- cbind(
    jpy, eur, gbp, aud, cad, chf, cny, nzd, hkd, mxn,
    brl, clp, ves, ars, rub, try, pkr, krw, zar, inr,
    uah
  ) %>%
  as.data.frame() %>%
  mutate(date = ymd(row.names(.))) %>%
  pivot_longer(cols = starts_with("USD"), values_to = "rate", names_to = "currency")

# Combine the old and new data frames.
# Keep the most. recent values if there is a discrepancy of the
# exchange rate on a given date.
# This could happen, if the exchange rate is updated during the day
# on the day the data are pulled.

# # Use this code for the first time the file is created
# rio::export(usd1_exchange, "data/exchange_rates.parquet")

# # Use this to update an existing file
usd1_exchange_updated <- usd1_exchange_prior %>%
  bind_rows(usd1_exchange) %>%
  group_by(date, currency) %>%
  slice(1) %>%
  ungroup()
rio::export(usd1_exchange_updated, "data/exchange_rates.parquet")



# # #
# # # Plots for the US Dollar to other currencies
# # #
usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.KRW") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.NZD") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.VES") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.GBP") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.TRY") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.CHF") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.BRL") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.JPY") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.ZAR") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.HKD") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.ARS") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.AUD") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.PKR") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.CNY") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.CLP") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.EUR") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.INR") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.MXN") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.RUB") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.CAD") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

usd1_ts <- usd1_exchange_updated %>%
  filter(currency == "USD.UAH") %>%
  mutate(diff = rate - lag(rate)) %>%
  as_tsibble(index = date) %>%
  na.omit()
usd1_ts %>% autoplot(.vars = rate) + labs(title = usd1_ts$currency[1])
acf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))
pacf(usd1_ts$diff, main = paste("ACF of First Difference of", usd1_ts$currency[1]))

