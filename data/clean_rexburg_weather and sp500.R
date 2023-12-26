# Read and clean rexburg weather data

pacman::p_load("tsibble", "fable",
               "feasts", "tsibbledata",
               "fable.prophet", "tidyverse",
               "patchwork",
               "ggthemes", "see",   # for okabeito color scheme
               "tidyverse",
               "rio",
               "lubridate"
)

weather_dat <- rio::import("data/rexburg_weather.csv")

head(weather_dat)

w2 <- weather_dat %>%
  select("NAME", "DAY", "MONTH", "YEAR", "DATE", "TMAX") %>%
  pivot_wider(names_from = "NAME", values_from = c("TMAX")) %>%
  rename(
    "byui" = "REXBURG BYU IDAHO, ID US",
    "airport" = "REXBURG MADISON CO AIRPORT, ID US",
    "moody" = "MOODY IDAHO, ID US"
  )

w3 <- w2 %>%
  select(byui, airport) %>%
  rename(x = byui, y = airport) %>%
  filter(abs(x - y)<50)
plot(w3$x, w3$y)
lm_byu <- lm(y~x, data= w3)

w4 <- w2 %>%
  select(moody, airport) %>%
  rename(x = moody, y = airport) %>%
  filter(abs(x - y)<50)
plot(w4$x, w4$y)
lm_moody <- lm(y~x, data= w4)

weather_clean <- w2 %>%
  mutate(byui_pred = predict(lm_byu, data.frame(x=w2$byui))) %>%
  mutate(moody_pred = predict(lm_moody, data.frame(x=w2$moody))) %>%
  mutate(rexburg = airport) %>%
  mutate(rexburg =
           case_when(
             airport > 100 ~ byui_pred,
             !is.na(airport) ~ airport,
             !is.na(byui_pred) ~ byui_pred,
             !is.na(moody_pred) ~ moody_pred,
             TRUE ~ NA
           )
        ) %>%
  mutate(imputed = !(rexburg == round(rexburg, 0))) %>%
  mutate(rexburg = round(rexburg, 0)) %>%
  mutate(dates = mdy(DATE)) %>%
  filter(dates > mdy("08/31/1998")) %>%
  filter(!is.na(rexburg)) %>%
  rename(rexburg_airport_high = rexburg) %>%
  select(dates, rexburg_airport_high, imputed) %>%
  filter(dates > mdy("01/01/1999"))

weather_clean %>%
  mutate(years = year(dates)) %>%
  group_by(years) %>%
  summarize(average = mean(rexburg_airport_high)) %>%
  plot



rexburg_day <- weather_clean %>%
  mutate(date_seq = dates) %>%
  mutate(
  dates = date_seq,
  year = lubridate::year(date_seq),
  month = lubridate::month(date_seq),
  value = rexburg_airport_high
  ) %>%
  select(-date_seq, -imputed) %>%
  tibble()

rexburg_day_ts <- rexburg_day |>
  mutate(index = dates) |>
  as_tsibble(index = index)

rexburg_annual_ts <- summarise(index_by(rexburg_day_ts, year), value = mean(value))
rexburg_annual_ts


mp <- autoplot(rexburg_day_ts, .vars = value) +
  labs(y = "high temperature")
yp <- autoplot(rexburg_annual_ts) +
  labs(y = "high temperature")
# +
#   scale_x_continuous(breaks = seq(1900, 2010, by = 2))
mp / yp



##################### S&P 500


replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

sp500_dat <- rio::import("data/sp500.csv") %>%
  mutate(dates = mdy(Date))


head(sp500_dat)

sp500_day <- sp500_dat %>%
  mutate(date_seq = dates) %>%
  mutate(
    dates = date_seq,
    year = lubridate::year(date_seq),
    month = lubridate::month(date_seq),
    value = replaceCommas(Close)
  ) %>%
  select(-date_seq) %>%
  tibble()

sp500_ts <- sp500_day |>
  mutate(index = dates) |>
  as_tsibble(index = index)

sp500_annual_ts <- summarise(index_by(sp500_ts, year), value = mean(value))
sp500_annual_ts


mp <- autoplot(sp500_ts, .vars = value) +
  labs(y = "S&P 500 Daily Closing Price")
yp <- autoplot(sp500_annual_ts) +
  labs(y = "S&P 500 Daily Closing Price")
# +
#   scale_x_continuous(breaks = seq(1900, 2010, by = 2))
mp / yp

