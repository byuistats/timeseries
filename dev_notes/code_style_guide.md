## Principles

1. Correctness
2. Simplicity
3. Readability
4. Cohesion
5. Professional Beauty

Where these principles come in conflict, show multiple methods. For example, keys often will include in a tabset the simplest and the highest quality charts side by side.

In line with these principles, code will always follow student comprehension first, and developer comprehension second.

### Textbook vs Keys

Code in the textbook has a purpose: to understand time series.

Code in keys has a purpose: for clients to understand the analysis.


## package::indicators

Packages are declared inline for functions outside the tidyverse/tidyverts ecosystem (ie not in `_common_packages.R`). For example, `plotly::plot_ly(...)`.


## General Convention

Code is to follow all common standards for R, including spacing and carriage return conventions


## Pipe

Use `|>` over `%>%`

`|>` is faster, has clearer debugging messages, and is R native (requiring no `dplyr`/`tidyverse` dependencies)

Requires R 4.1+, `|>` uses `_` for placeholder while `%>%` uses `.`


## Separation

The following are separated by a blank line:
- Import/cleaning
- Analysis
- Reporting/display

```{r}
gdp_ar <- gdp_ts |>
  model(ar = ARIMA(year_over_year ~ 1 + pdq(4,0,0) + PDQ(0, 0, 0)))

tidy(gdp_ar)
```


## Plots

All plots should have meaningful axis names and titles unless obvious or irrelevant (such as quarters on the x-axis).

Diagnostic or interim plots prioritize code simplicity and readability for the analyzer, while final presentation plots prioritize understanding for an external audience. Students should not be expected to make every plot beautiful.

### Order

1. Plot: declared on the first line, even if piped
2. Aesthetics: in descending order of importance, supporting graphics at the bottom
3. Labels
4. Themes: theme_minimal() preferred

Plot is declared on the first line, even if piped

```{r}
# Example 1
gdp_ts |> autoplot(year_over_year) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Yearly Percentage Change in US Real GDP",
       subtitle = "Year-over-Year Growth Rate (1990-2024)",
       x = "Quarter",
       y = "Year-over-Year Growth Rate (decimal)") +
  theme_minimal()
```

```{r}
# Example 2
ggplot(gdp_ts, aes(quarter, year_over_year)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Yearly Percentage Change in US Real GDP",
       subtitle = "Year-over-Year Growth Rate (1990-2024)",
       x = "Quarter",
       y = "Year-over-Year Growth Rate (decimal)") +
  theme_minimal()
```




## TODO Tables

[ ] Considering `gt()` vs `pander()`


