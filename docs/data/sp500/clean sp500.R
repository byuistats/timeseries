##################### S&P 500

# Read and clean S&P 500 data

pacman::p_load("tsibble", "fable",
               "feasts", "tsibbledata",
               "fable.prophet", "tidyverse",
               "patchwork",
               "ggthemes", "see",   # for okabeito color scheme
               "tidyverse",
               "rio",
               "lubridate"
)


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


# Plot annual and monthly summaries

psum <- autoplot(summarise(index_by(sp500_ts, year), value = sum(value)))
pbox <- ggplot(sp500_ts, aes(x = factor(month), y = value)) +
  geom_boxplot()
psum / pbox

