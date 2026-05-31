## Principles

1. Correctness
2. Simplicity
3. Readability
4. Cohesion
5. Professional Beauty

Keys often will include in a tabset the simplest and the highest quality charts side by side.

In line with these principles, code will always follow student comprehension first, and developer comprehension second.


## package::indicators

Packages are declared for functions outside the tidyverse/tidyverts ecosystem where the package is rarely used. For example, `plotly::plot_ly(...)`.


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


## Code vs. Output

In most cases, prioritize readability, simplicity, and cohesion over beauty.


## Plots

All plots should have meaningful axis names and titles unless obvious or irrelevant (such as quarters on the x-axis).

Diagnostic or interim plots prioritize code simplicity and readability for the analyzer, while final presentation plots prioritize understanding for an external audience. Students should not be expected to make every plot beautiful.


## Tables

[ ] Considering `gt()` vs `pander()`


