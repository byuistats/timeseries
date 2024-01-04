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

mar_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/march2022.csv") %>% filter(Id == "129")
apr_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/april2022.csv") %>% filter(Id == "129")
may_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/may2022.csv") %>% filter(Id == "129")
jun_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/june2022.csv") %>% filter(Id == "129")
jul_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/july2022.csv") %>% filter(Id == "129")
aug_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/august2022.csv") %>% filter(Id == "129")
sep_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/september2022.csv") %>% filter(Id == "129")
oct_df <- rio::import("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds/october2022.csv") %>% filter(Id == "129")

# Create final data file
mar_df %>%
  bind_rows(apr_df, may_df, jun_df, jul_df, aug_df, sep_df, oct_df) %>%
  unique() %>%
  mutate(date = mdy_hms(DataAsOf)) %>%
  filter(date >= mdy_hms("4/15/2022 10:13:11") & date <= mdy_hms("5/16/2022 14:00:00")) %>%
  arrange(date) %>%
  dplyr::select(-date) %>%
  rio::export("C:/Users/craigaj/Documents/BYUI-Timeseries-Drafts/data/ny_speeds.csv")
