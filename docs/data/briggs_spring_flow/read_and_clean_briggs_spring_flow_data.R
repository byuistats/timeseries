# Read and clean spring flow data

briggs <- rio::import("data/briggs_spring.parquet") |>
  mutate(date2 = ymd_hm(date)) |>
  mutate(diff = date2 - lag(date2)) |>
  mutate(minute = minute(date2))
briggs %>% head
briggs %>% tail
briggs %>% View
briggs %>%
  ggplot(aes(x = date, y = as.numeric(diff))) +
  geom_line()

briggs %>% group_by(minute) %>% summarize(n = n()) %>% View
briggs %>% filter(!(minute %in% c(0,15,30,45))) %>% arrange(date2)
briggs %>% filter(!(minute %in% c(0,15,30,45))) %>% View
  summarize(min = min(date2), max = max(date2))

briggs %>%
  mutate(round_time = round_date(date2, unit = "hour")) %>%
  mutate(diff = date2 - round_time) %>%
  filter(!(minute %in% c(15,30,45))) %>%
  arrange(round_time, abs(diff)) %>%
  filter(round_time != lag(round_time)) %>%
  # filter(round_time >= mdy("10/1/1995")) %>%  # This eliminates early gap
  # filter(round_time >= mdy("10/1/2006")) %>%  # Note the break here
  filter(round_time >= mdy("10/1/2007")) %>%
  # filter(round_time <= mdy("10/1/2008")) %>%
  as_tsibble(index = round_time) %>%
  fill_gaps() %>%
  autoplot(.vars = flow)

