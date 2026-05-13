The following code simulates $(x,y)$-pairs of random variables.

```{r}
# Unset random seed
set.seed(Sys.time())

# Specify means and correlation coefficient
n <- 50             # number of points
mu <- c(1.25, 2.5)  # mean vector (mu_x, mu_y)
sigma_x <- 1        # standard deviation x
sigma_y <- 3        # standard deviation y
rho <- 0.8          # correlation coefficient

# Define variance-covariance matrix
sigma <- matrix(
  c(sigma_x^2,
    rho*sigma_x*sigma_y,
    rho*sigma_x*sigma_y,
    sigma_y^2),
  nrow = 2)

# Simulate bivariate normal data
mvn_data <- MASS::mvrnorm(n, mu, sigma) |>
  data.frame() |>
  rename(x = X1, y = X2)
```

The following table illustrates some of the simulated values. The mean of the $x$ values is $\bar x = `r round(mean(mvn_data$x), 3)`$. The mean of the $y$ values is $\bar y =`r round(mean(mvn_data$y), 3)`$. We will soon use the values $(x-\bar x)$, $(x-\bar x)^2$, $(y-\bar y)$, $(y-\bar y)^2$, and $(x-\bar x)(y-\bar y)$. For convenience, they are included in the table below.

```{r}
#| echo: false

cov_dat <- mvn_data |>
  mutate(t = row_number()) |>
  dplyr::select(t, x, y) |>
  mutate(
    xx = x - mean(x),
    xx2 = xx^2,
    yy = y - mean(y),
    yy2 = yy^2,
    xy = (x - mean(x)) * (y - mean(y)),
    sign = case_when(
      xy > 0 ~ "positive",
      xy < 0 ~ "negative",
      TRUE ~ "zero")
  )

cov_dat_summary <- cov_dat |>
  summarize(
    x = sum(x),
    y = sum(y),
    xx = sum(xx),
    xx2 = sum(xx2),
    yy = sum(yy),
    yy2 = sum(yy2),
    xy = sum(xy)
  ) |>
  round_df(3) |>
  mutate(across(everything(), as.character)) |>
  mutate(
    t = "sum",
    sign = case_when(
      xy > 0 ~ "positive",
      xy < 0 ~ "negative",
      TRUE ~ "zero")
  )

min_row <- cov_dat |>
  mutate(
    positive = if_else(xy > 0, 1, 0),
    negative = if_else(xy < 0, 1, 0)
  ) |>
  mutate(
    pos_sum = cumsum(positive),
    neg_sum = cumsum(negative),
    both = pos_sum > 0 & neg_sum > 0,
    sum_both = cumsum(both)
  ) |>
  filter(sum_both <= 1) |>
  nrow()

cov_dat |>
  convert_df_to_char() |>
  bind_rows(cov_dat_summary) |>
  mutate(
    xy = cell_spec(xy,
                   color = case_when(
                     xy > 0 ~ "#56B4E9",
                     xy < 0 ~ "#E69F00"
                   )
    ),
    sign = cell_spec(sign,
                     color = case_when(
                       sign == "positive" ~ "#56B4E9",
                       sign == "negative" ~ "#E69F00"
                     )
    )
  ) |>
  rename(
    "x_t" = x,
    "y_t" = y,
    "x_t-mean(x)" = xx,
    "(x_t-mean(x))^2" = xx2,
    "y_t-mean(y)" = yy,
    "(y_t-mean(y))^2" = yy2,
    "(x_t-mean(x))(y_t-mean(y))" = xy
  ) |>
  concat_partial_table(min(25,max(6, min_row)), 6) |>
  display_table()
```

The simulated values are plotted below, with vertical lines drawn at $x = \bar x$ and $y = \bar y$. The first simulated point $(t=1)$ is circled.

```{r}
#| warning: false
#| echo: false

ggplot(cov_dat, aes(x = x, y = y, color = sign)) +
  geom_point(data=cov_dat |> filter(row_number() == 1),
             pch=21,
             size=4,
             colour="black") +
  geom_point() +
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     labels = c(expression((x-bar(x))(y-bar(y))<0),
                                expression((x-bar(x))(y-bar(y))>0))) +
  geom_vline(xintercept = mean(cov_dat$x), color = "#009E73") +
  geom_hline(yintercept = mean(cov_dat$y), color = "#009E73") +
  labs(x="x", y="y") +
  theme_bw() +
  ggtitle(paste0("Simulated Data (n = ",n,", ρ = ",rho,")")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(title = "Quadrant", reverse = TRUE)) +
  annotate("text", x = mean(cov_dat$x), y = min(cov_dat$y),
           label = expression(bar(x)), hjust = 0, vjust = 0) +
  annotate("text", y = mean(cov_dat$y), x = min(cov_dat$x),
           label = expression(bar(y)), hjust = 0, vjust = 0)
# + geom_segment(aes(x = 5, y = 30, xend = 3.5, yend = 25), arrow = arrow(length = unit(0.5, "cm")))
```

If the quantity $(x-\bar x)(y-\bar y)$ is greater than zero, the points are colored blue. Otherwise, they are colored orange.

::: {.callout-tip icon=false title="Check Your Understanding"}

-   What color are the points if $(x-\bar x)$ and $(y-\bar y)$ have the same sign?
  -   What color are the points if $(x-\bar x)$ and $(y-\bar y)$ have different signs?

  :::




  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->
  <!-- start editing again here -->



  To compute the sample covariance, we divide the sum of the $(x - \bar x)(y - \bar y)$ values by $n-1$:

  $$
  cov(x,y)
=
  \frac{\sum\limits_{t=1}^n (x - \bar x)(y - \bar y)}{n-1}
=
  \frac{`r cov_dat_summary$xy`}{`r n` - 1}
=
  `r cov(cov_dat$x, cov_dat$y) |> round(3)`
$$

  You can think of this as an "average" of the $(x - \bar x)(y - \bar y)$ values. The only difference is that we divide by $n-1$ instead of $n$.

::: {.callout-tip icon=false title="Check Your Understanding"}

-   If there are more blue points than orange points, what should the sign of the sample covariance be? Why?
  -   What does the sample covariance tell us?

  :::

  The sample covariance is related to the sample standard deviation of $x$ and $y$ and the sample correlation coefficient between $x$ and $y$.

The sample standard deviations are:

  $$
  \begin{align*}
s_x &= \sqrt{ \frac{\sum\limits_{t=1}^n (x - \bar x)^2}{n-1} }
=
  \sqrt{
    \frac{
      `r sum((cov_dat$x - mean(cov_dat$x))^2) |> round(3)`
    }{
      `r n`-1
    }
  }
=
  `r sd(cov_dat$x) |> round(3)`
\\
s_y &= \sqrt{ \frac{\sum\limits_{t=1}^n (y - \bar y)^2}{n-1} }
=
  \sqrt{
    \frac{
      `r sum((cov_dat$y - mean(cov_dat$y))^2) |> round(3)`
    }{
      `r n`-1
    }
  }
=
  `r sd(cov_dat$y) |> round(3)`
\end{align*}
$$

  The sample correlation coefficient is:
  $$
  r = \frac{\sum\limits_{t=1}^n (x - \bar x)(y - \bar y)}{\sqrt{\sum\limits_{t=1}^n (x - \bar x)^2} \sqrt{\sum\limits_{t=1}^n (y - \bar y)^2}}
=
  \frac{
    `r sum((cov_dat$x - mean(cov_dat$x))*(cov_dat$y - mean(cov_dat$y))) |> round(3)`
  }{
    \sqrt{ `r sum((cov_dat$x - mean(cov_dat$x))^2) |> round(3)`}
    \sqrt{ `r sum((cov_dat$y - mean(cov_dat$y))^2) |> round(3)`}
  }
=
  `r cor(cov_dat$x, cov_dat$y) |> round(3)`
$$