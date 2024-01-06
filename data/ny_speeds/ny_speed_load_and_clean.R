# The Bruckner Expressway (Highway I-278) cuts through the Bronx in New York City.
# Traffic sensors record the speed of cars every 5 minutes. The traffic speed data
# for this highway between Stratford Avenue and Castle Hill Avenue from 4/15/2022
# to 5/16/2022. (These dates were selected, because they provided uninterrupted
# consecutive measurements.)

# Source: City of New York Department of Transportation
# Online Links: [http://a841‐dotweb01.nyc.gov/datafeeds](https://data.beta.nyc/dataset/nyc-real-time-traffic-speed-data-feed-archived)
# Accessed 1/4/2024

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, kableExtra,
               tsibble, fable,
               feasts, tsibbledata,
               fable.prophet,
               patchwork,
               ggthemes,
               see,   # okabeito color scheme
               MASS, # MVNorm
               ggokabeito, # colorblind palette
               kableExtra, # formatting tables
               stringr, # string manipulation
               lubridate # date manipulation
)


set.seed(1)

# # Read in raw data (now deleted)
# jun_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/june2022.csv") %>% filter(Id == "129")
# jul_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/july2022.csv") %>% filter(Id == "129")
#
# # Create final data file
# mar_df %>%
#   bind_rows(jun_df, jul_df) %>%
#   rio::export("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/ny_speeds_jun_jul_129.csv")


speed_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/ny_speeds_jun_jul_129.csv") %>%
  unique() %>%
  mutate(date = mdy_hms(DataAsOf)) %>%
  filter(date >= mdy_hms("6/22/2022 19:44:10") & date <= mdy_hms("7/22/2022 1:13:15")) %>%
  arrange(date)

five_min <- mdy_hm("1/1/2000 12:05") - mdy_hm("1/1/2000 12:00")

all_times <- seq(mdy_hms("6/22/2022 19:40:00"), mdy_hms("7/22/2022 1:15:00"), five_min)

all_times_df <- data.frame(date = all_times)

stop_point <- mdy_hms("7/17/2022 4:30:00")

cleaned_data <- speed_df |>
  bind_rows(all_times_df) %>%
  arrange(date) %>%
  mutate(rounded = round_date(date, "5 mins")) %>%
  group_by(rounded) %>%
  mutate(count = n()) %>%
  filter(!(is.na(Id) & count > 1)) %>% # Eliminates rows corresponding to duplicated valid times
  mutate(count = n()) %>%
  group_by() %>%
  #
  # Bunched values come before the NA
  mutate(
    rounded_year = year(rounded),
    rounded_month = month(rounded),
    rounded_day = day(rounded),
    rounded_hour = hour(rounded),
    rounded_minute = minute(rounded)
  ) %>%
  mutate(
    rounded_hour = ifelse(rounded_minute == 55 & count > 1 & is.na(lead(Id)), rounded_hour + 1, rounded_hour),
    rounded_minute = ifelse(rounded_minute == 55 & count > 1 & is.na(lead(Id)), -5, rounded_minute)
  ) %>%
  mutate(rounded2 =
           ifelse(
             count > 1 & is.na(lead(Id)),
             paste0(rounded_month,"/",rounded_day,"/",rounded_year," ",rounded_hour,":",rounded_minute+5,":00"),
             paste0(rounded_month,"/",rounded_day,"/",rounded_year," ",rounded_hour,":",rounded_minute,":00")
           )
  ) %>%
  mutate(
    rounded3 = mdy_hms(rounded2)
  ) %>%
  filter(!(is.na(Id) & rounded3 == lag(rounded3))) %>%
  #
  # Bunched values come after NA
  mutate(
    rounded_year = year(rounded),
    rounded_month = month(rounded),
    rounded_day = day(rounded),
    rounded_hour = hour(rounded),
    rounded_minute = minute(rounded)
  ) %>%
  mutate(
    rounded_hour = ifelse(rounded_minute == 0 & count > 1 & is.na(lag(Id)), rounded_hour - 1, rounded_hour),
    rounded_minute = ifelse(rounded_minute == 0 & count > 1 & is.na(lag(Id)), 60, rounded_minute)
  ) %>%
  mutate(rounded2 =
           ifelse(
             count > 1 & is.na(lag(Id)),
             paste0(rounded_month,"/",rounded_day,"/",rounded_year," ",rounded_hour,":",rounded_minute-5,":00"),
             paste0(rounded_month,"/",rounded_day,"/",rounded_year," ",rounded_hour,":",rounded_minute,":00")
           )
  ) %>%
  mutate(
    rounded3 = mdy_hms(rounded2)
  ) %>%
  filter(!(is.na(Id) & rounded3 == lead(rounded3))) %>%
  #
  #
  # Delete extra value
  filter(!(date == mdy_hms("7/21/2022 21:01:20"))) %>%
  #
  #
  # Fills in missing values for a streak on 7/11 #########
  mutate(
    Id =
      case_when(
        rounded3 >= ymd_hms("2022-07-11 08:55:00") & rounded3 <= ymd_hms("2022-07-11 10:00:00") ~ 0,
        rounded3 >= mdy_hm("7/8/2022 3:20") & rounded3 <= mdy_hm("7/8/2022 3:30") ~ 0,
        rounded3 >= mdy_hm("7/11/2022 10:55") & rounded3 <= mdy_hm("7/11/2022 11:00") ~ 0,
        date == ymd_hms("2022-07-14 21:20:00") & is.na(Id) ~ 0,
        month(rounded3) == 6 & is.na(Id) ~ 0,
        month(rounded3) == 7 & day(rounded3) == 1 & is.na(Id) ~ 0,
        month(rounded3) == 7 & day(rounded3) == 11 & is.na(Id) ~ 0,
        month(rounded3) == 7 & day(rounded3) == 16 & is.na(Id) ~ 0,
        month(rounded3) == 7 & day(rounded3) == 21 & is.na(Id) ~ 0,
        TRUE ~ Id
      )
  ) %>%
  mutate(
    Speed =
      case_when(
        rounded3 >= ymd_hms("2022-07-11 08:55:00") & rounded3 <= ymd_hms("2022-07-11 10:00:00") ~ sample(seq(44.73, 53.4, 0.01), n(), replace = TRUE),
        rounded3 >= mdy_hm("7/8/2022 3:20") & rounded3 <= mdy_hm("7/8/2022 3:30") ~ sample(seq(56.54, 59.03, 0.01), n(), replace = TRUE),
        rounded3 >= mdy_hm("7/11/2022 10:55") & rounded3 <= mdy_hm("7/11/2022 11:00") ~ sample(seq(49.08, 56.54, 0.01), n(), replace = TRUE),
        date == ymd_hms("2022-07-14 21:20:00") ~ (lead(Speed) + lag(Speed)) / 2,
        month(rounded3) == 6 & is.na(Speed) ~ (lead(Speed) + lag(Speed)) / 2,
        month(rounded3) == 7 & day(rounded3) == 1 & is.na(Speed) ~ (lead(Speed) + lag(Speed)) / 2,
        month(rounded3) == 7 & day(rounded3) == 11 & is.na(Speed) ~ (lead(Speed) + lag(Speed)) / 2,
        month(rounded3) == 7 & day(rounded3) == 16 & is.na(Speed) ~ (lead(Speed) + lag(Speed)) / 2,
        month(rounded3) == 7 & day(rounded3) == 21 & is.na(Speed) ~ (lead(Speed) + lag(Speed)) / 2,
        TRUE ~ Speed
      )
  ) %>%
  ########################################################
  # Eliminate 7/1 at 00:00:00, which is a spurious value added earlier
  filter(date != ymd_hms("2022-07-01 00:00:00")) %>%
  #
  # Eliminate all other rows with missing data. They do not need to be imputed,
  # since there is enough data on that date
  filter(!(is.na(Speed))) %>%
  #
  #
  mutate(
    rounded_year = year(rounded3),
    rounded_month = month(rounded3),
    rounded_day = day(rounded3),
    rounded_hour = hour(rounded3),
    rounded_minute = minute(rounded3),
    rounded_daily_minutes = rounded_hour * 60 + rounded_minute,
    month_day_year = paste0(rounded_month,"/",rounded_day,"/",rounded_year)
  ) %>%
  # mutate(new_day = ifelse(rounded_hour == 0 & rounded_minute == 0, 1, 0)) %>%
  group_by(month_day_year) %>%
  mutate(minute_sequence = 5 * (row_number() - 1)) %>%
  # mutate(max_min_sequence = max(minute_sequence)) %>% ##################### DEBUG
  group_by() %>%
  mutate(minute_sequence =
           ifelse(
             month(rounded3) == 6 & day(rounded3) == 22,
             minute_sequence + 1185,
             minute_sequence
             )
  ) %>%
  filter(minute_sequence < 1440) %>% # Eliminates extra daily observations
  mutate(
    new_hour = minute_sequence %/% 60,
    new_minute = minute_sequence %% 60,
    new_date_text = paste0(rounded_month,"/",rounded_day,"/",rounded_year," ",new_hour,":",new_minute),
    date = mdy_hm(new_date_text)
  ) %>%
  # filter(new_hour == 24) %>% View
  # filter(date >= ymd_hms("2022-07-01 00:00:00")) %>% View
  # dplyr::select(date) %>% duplicates() %>% View
  # group_by(month_day_year) %>% summarise(max = max(minute_sequence)) %>% group_by() %>% filter(max < 1435) %>% View
  # group_by(month_day_year) %>% summarize(max_min_sequence = max(minute_sequence)) %>% filter(max_min_sequence < 1435) %>% View
  # filter(is.na(Speed) | is.na(lag(Speed)) | is.na(lead(Speed))) %>% View
  dplyr::select(Id, Speed, TravelTime, Status, DataAsOf, linkId, date)
  #  # group_by() %>% mutate(diff = date - lag(date)) %>% tail(-1) %>% summarize(min = min(diff), max = max(diff)) %>%

cleaned_data %>%
  rio::export("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds.csv")

# Only include data for July 2 onward to make it easier to see the decomposition.
cleaned_data %>%
  filter(date >= mdy("7/2/2022")) %>%
  rio::export("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds.csv")



####################### Stuff below here is junk ###########################

#   filter(abs(minute_sequence - rounded_daily_minutes) > 5) %>%
#   View
#   #
#   #
#   ##########
#   group_by(rounded3) %>%
#   mutate(count = n()) %>%
#   group_by() %>%
#   # dplyr::select(Speed, rounded, rounded, id2) %>%
#   # filter(date > stop_point) %>%
#   filter(
#       (
#         count>1 |
#           lead(count)>1 | lead(count,2)>1 | lead(count,3)>1 | lead(count,4)>1 |
#           lead(count,5)>1 | lead(count,6)>1 | lead(count,7)>1 | lead(count,8)>1 |
#           lead(count,9)>1 | lead(count,10)>1 | lead(count,11)>1 | lead(count,12)>1 |
#           lead(count,13)>1 | lead(count,14)>1 | lead(count,15)>1 | lead(count,16)>1 |
#           lead(count,17)>1 | lead(count,18)>1 | lead(count,19)>1 | lead(count,20)>1 |
#           lag(count)>1 | lag(count,2)>1 | lag(count,3)>1 | lag(count,4)>1 |
#           lag(count,5)>1 | lag(count,6)>1 | lag(count,7)>1 | lag(count,8)>1 |
#           lag(count,9)>1 | lag(count,10)>1 | lag(count,11)>1 | lag(count,12)>1 |
#           lag(count,13)>1 | lag(count,14)>1 | lag(count,15)>1 | lag(count,16)>1 |
#           lag(count,17)>1 | lag(count,18)>1 | lag(count,19)>1 | lag(count,20)>1
#       )
#       &
#       is.na(Id)
#     ) %>%
#   mutate(diff3 = rounded3 - lag(rounded3)) %>%
#   # filter(is.na(Id) | is.na(lead(Id)) | is.na(lag(Id))) %>%
#   # filter(hour(rounded3) %>% round() == 21) %>%
#   # filter(is.na(Id)) %>%
#   View()
#   # dplyr::select(DataAsOf) %>%
#   # pull()
#
# temp_df %>%
#   dplyr::select(-date) %>%
#   rio::export("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds.csv")
#
#
# eight_min <- mdy_hm("1/1/2000 12:08") - mdy_hm("1/1/2000 12:00")
# nine_min <- mdy_hm("1/1/2000 12:09") - mdy_hm("1/1/2000 12:00")
# ten_min <- mdy_hm("1/1/2000 12:10") - mdy_hm("1/1/2000 12:00")
#
# # Create final data file
# mar_df %>%
#   bind_rows(apr_df, may_df, jun_df, jul_df, aug_df, sep_df, oct_df) %>%
#   unique() %>%
#   na.omit() %>%
#   mutate(date = mdy_hms(DataAsOf)) %>%
#   # filter(date >= mdy_hms("8/12/2022 21:50:00") & date <= mdy_hms("8/30/2022 14:30:00")) %>%
#   # filter(date >= mdy_hms("8/12/2022 22:04:12") & date <= mdy_hms("8/30/2022 14:09:03")) %>%
#   arrange(date) %>%
#   mutate(diff = date - lag(date)) %>%
#   filter(diff > ten_min) %>%
#   mutate(diff2 = date - lag(date)) %>%
#   # arrange(desc(diff2)) %>%
# #%>%
#   View()
#   # rio::export("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds.csv")
#
#
#   # Create final data file
#   mar_df %>%
#     bind_rows(apr_df, may_df, jun_df, jul_df, aug_df, sep_df, oct_df) %>%
#     unique() %>%
#     na.omit() %>%
#     mutate(date = mdy_hms(DataAsOf)) %>%
#     # filter(date >= mdy_hms("8/12/2022 21:50:00") & date <= mdy_hms("8/30/2022 14:30:00")) %>%
#     # filter(date >= mdy_hms("8/12/2022 22:04:12") & date <= mdy_hms("8/30/2022 14:09:03")) %>%
#     arrange(date) %>%
#     mutate(diff = date - lag(date)) %>%
#     filter(diff > ten_min) %>%
#     mutate(diff2 = date - lag(date)) %>%
#     arrange(desc(diff2)) %>%
#     head()
#
#
#   mar_df %>%
#     bind_rows(apr_df, may_df, jun_df, jul_df, aug_df, sep_df, oct_df) %>%
#     unique() %>%
#     na.omit() %>%
#     mutate(date = mdy_hms(DataAsOf)) %>%
#     arrange(date) %>%
#     mutate(diff = date - lag(date)) %>%
#     mutate(diff1 = lag(diff,1)) %>%
#     mutate(diff2 = lag(diff,2)) %>%
#     mutate(diff3 = lag(diff,3)) %>%
#     mutate(diff4 = lag(diff,4)) %>%
#     mutate(rolling = (diff+diff1+diff2+diff3+diff4)/5 ) %>%
#     mutate(start = ifelse(lag(rolling) > eight_min,1,0)) %>%
#     mutate(end = ifelse(rolling > eight_min, 1, 0)) %>%
#     filter(start + end == 1) %>%
#     mutate(diff10 = date - lag(date)) %>%
#     filter(end == 1) %>%
#     arrange(desc(diff10)) %>%
#     head
#     View()
#
#
#     mar_df %>%
#       bind_rows(apr_df, may_df, jun_df, jul_df, aug_df, sep_df, oct_df) %>%
#       unique() %>%
#       na.omit() %>%
#       mutate(date = mdy_hms(DataAsOf)) %>%
#       filter(month(date) == 10 & day(date) == 13 & hour(date) >= 1) %>%
#       View
#
#
#     mar_df %>%
#       bind_rows(apr_df, may_df, jun_df, jul_df, aug_df, sep_df, oct_df) %>%
#       unique() %>%
#       na.omit() %>%
#       mutate(date = mdy_hms(DataAsOf)) %>%
#       arrange(date) %>%
#       mutate(diff = date - lag(date)) %>%
#       mutate(diff_num = as.numeric(diff)) %>%
#       filter(date > ymd_hms("2022-06-22 19:40:00") & date < ymd_hms("2022-07-22 01:15:00")) %>%
#       mutate(rounded_time = round_date(date, "5 mins")) %>%
#       mutate(gaps = ifelse(rounded_time - lag(rounded_time) > eight_min, (rounded_time - lag(rounded_time)) *10/6,0)) %>%
#       mutate(duplicate = if_else(rounded_time == lag(rounded_time), 1, 0)) %>%
#       rio::export("C:/Users/craigaj/Documents/deleteme.csv")
#
