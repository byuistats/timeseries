library(readxl)

# Read and clean one worksheet from the Excel file
read_one_sheet <- function(year) {
  temp <- read_xlsx("data/retail/retail_sales_by_business_type.xlsx", sheet = as.character(year), skip = 4, n_max = 67) |>
    rename(
      naics = "...1",
      business = "...2"
    ) |>
    select(-TOTAL) |>
    convert_df_to_char() |>
    mutate(ordering = row_number()) |>
    pivot_longer(cols = contains(" "), names_to = "month", values_to = "sales") |>
    select("naics", "business", "month", "sales", "ordering") |>
    mutate(
      month = yearmonth(my(month)),
      naics = as.integer(naics),
      sales = as.integer(sales)) |>
    rename(sales_millions = sales)
  return(temp)
}

# Plot one business type
plot_retail_sales <- function(df, naics_value) {
  temp_ts <- df |>
    filter(naics == naics_value) |>
    as_tsibble(index = month)

  temp_ts |>
    autoplot(.vars = sales_millions) +
    labs(
      x = "Month",
      y = "Sales (Millions of U.S. Dollars)",
      title = paste0(temp_ts$business[1], " (", temp_ts$naics[1], ")")
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}

# Plot decomposition for one business type
decompose_retail_sales <- function(df, naics_value, model_type = "multiplicative") {
  temp_ts <- df |>
    filter(naics == naics_value) |>
    as_tsibble(index = month)

  temp_ts |>
    model(feasts::classical_decomposition(sales_millions, type = model_type)) |>
    components() |>
    autoplot()
    # +
    # labs(
    #   x = "Month",
    #   y = "Sales (Millions of U.S. Dollars)",
    #   title = paste0(temp_ts$business[1], " (", temp_ts$naics[1], ")")
    # ) +
    # theme(plot.title = element_text(hjust = 0.5))
}


# Read in data
retail <- read_one_sheet(1992)
for (i in 1993:2022) {
  retail <- retail |> bind_rows(read_one_sheet(i))
}

retail |>
  filter(naics == 4441) |>
  as_tsibble(index = month) |>
  model(feasts::classical_decomposition(sales_millions, type = "mult")) |>
  components() |>
  autoplot()

decompose_retail_sales(retail, 4441, "multiplicative")


plot_retail_sales(retail, 441)
# plot_retail_sales(retail, 4411,4412)
plot_retail_sales(retail, 4411)
plot_retail_sales(retail, 44111)
plot_retail_sales(retail, 44112)
plot_retail_sales(retail, 4413)
# plot_retail_sales(retail, 442,443)
plot_retail_sales(retail, 442)
plot_retail_sales(retail, 4421)
plot_retail_sales(retail, 4422) # Multiplicative
plot_retail_sales(retail, 44221)
plot_retail_sales(retail, 442299)
plot_retail_sales(retail, 443)
plot_retail_sales(retail, 443141) # Roughly additive
plot_retail_sales(retail, 443142)
plot_retail_sales(retail, 444)
plot_retail_sales(retail, 4441) # Beautiful multiplicative ******
plot_retail_sales(retail, 44412)
plot_retail_sales(retail, 44413) # Multiplicative
plot_retail_sales(retail, 445)
plot_retail_sales(retail, 4451)
plot_retail_sales(retail, 44511)
plot_retail_sales(retail, 4453)
plot_retail_sales(retail, 446)
plot_retail_sales(retail, 44611)
plot_retail_sales(retail, 447) # Fun, wild multiplicative
plot_retail_sales(retail, 448)
plot_retail_sales(retail, 4481)
plot_retail_sales(retail, 44811)
plot_retail_sales(retail, 44812) # Beautiful additive
plot_retail_sales(retail, 44814)
plot_retail_sales(retail, 44819)
plot_retail_sales(retail, 4482) # Fun multiplicative
plot_retail_sales(retail, 44831)
plot_retail_sales(retail, 451)
plot_retail_sales(retail, 45111) # Great multiplicative pattern
plot_retail_sales(retail, 45112)
plot_retail_sales(retail, 451211) # Multiplicative: Rise and fall...very interesting
plot_retail_sales(retail, 452) # Maybe additive, linear
plot_retail_sales(retail, 4521) # Multiplicative: Rise and fall
plot_retail_sales(retail, 452111)
plot_retail_sales(retail, 452112) # Multiplicative: Rise and fall
plot_retail_sales(retail, 4529) # Multiplicative
plot_retail_sales(retail, 45291) # Multiplicative - very nice
plot_retail_sales(retail, 45299)
plot_retail_sales(retail, 453)
plot_retail_sales(retail, 4532)
plot_retail_sales(retail, 45321)
plot_retail_sales(retail, 45322)
plot_retail_sales(retail, 45330)
plot_retail_sales(retail, 454)
plot_retail_sales(retail, 4541) # Multiplicative - nice
plot_retail_sales(retail, 45431)
plot_retail_sales(retail, 722)
plot_retail_sales(retail, 7224)
plot_retail_sales(retail, 7225) # Multiplicative - Cut off before covid???
plot_retail_sales(retail, 722511)

