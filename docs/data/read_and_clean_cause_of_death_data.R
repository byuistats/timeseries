death_cause <- rio::import("data/cause_of_death_weekly.parquet") |>
  rename(location = "Jurisdiction of Occurrence") |>
  filter(location == "United States") |>
  select(!contains("flag_")) |>
  rename(
    year = "MMWR Year",
    week = "MMWR Week",
    date = "Week Ending Date",
    all_causes = "All  Cause",
    natural_causes = "Natural Cause",
    septicemia = "Septicemia (A40-A41)",
    neoplasms = "Malignant neoplasms (C00-C97)",
    diabetes = "Diabetes mellitus (E10-E14)",
    alzheimer = "Alzheimer disease (G30)",
    flu = "Influenza and pneumonia (J10-J18)",
    respiratory = "Chronic lower respiratory diseases (J40-J47)",
    other_respiratory = "Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)",
    nephritis = "Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)",
    other = "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)",
    heart = "Diseases of heart (I00-I09,I11,I13,I20-I51)",
    cerebrovascular = "Cerebrovascular diseases (I60-I69)"
  )
death_cause %>% View
death_cause %>% names
death_ts <- death_cause %>%
  mutate(date = dmy(date)) |>
  filter(!is.na(date)) |>
  as_tsibble(index = date)
death_ts |> autoplot(.vars = all_causes)
death_ts |> autoplot(.vars = natural_causes)
death_ts |> autoplot(.vars = septicemia)
death_ts |> autoplot(.vars = neoplasms)
death_ts |> autoplot(.vars = diabetes)
death_ts |> autoplot(.vars = alzheimer)
death_ts |> autoplot(.vars = flu)
death_ts |> autoplot(.vars = respiratory)
death_ts |> autoplot(.vars = other_respiratory)
death_ts |> autoplot(.vars = nephritis)
death_ts |> autoplot(.vars = other)
death_ts |> autoplot(.vars = heart)
death_ts |> autoplot(.vars = cerebrovascular)

