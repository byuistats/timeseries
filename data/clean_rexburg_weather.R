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

weather_next <- w2 %>%
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
  mutate(date = mdy(DATE)) %>%
  filter(date > mdy("08/31/1998")) %>%
  filter(!is.na(rexburg)) %>%
  rename(rexburg_high = rexburg) %>%
  select(date, rexburg_high, imputed) %>%
  View
