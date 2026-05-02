# Applied Time Series Analysis

Course materials for **Applied Time Series Analysis** at Brigham Young University–Idaho. The site is a Quarto-based interactive textbook covering the theory and practice of time series analysis in R, from exploratory decomposition through ARIMA modeling.

## Live Site

The rendered course website is published via GitHub Pages from the `docs/` directory.

## Course Structure

| Chapter | Topic |
|---------|-------|
| 1 | Time Series Data — plots, trends, seasonal decomposition, additive and multiplicative models |
| 2 | Autocorrelation Concepts — covariance, correlation, ACF |
| 3 | Exploration of Autocorrelation — cross-correlation, PACF, Bass model |
| 4 | Basic Stochastic Models — white noise, random walks, backward shift and difference operators |
| 5 | Regression — time series regression, harmonic variables |
| 6 | Stationary Models — MA, AR, ARMA |
| 7 | Non-Stationary Models — differencing, ARIMA |

An appendix covers harmonic seasonal variables and ARCH/GARCH models.

## Repository Layout

```
.
├── chapter_X.qmd               # Chapter overview pages
├── chapter_X_lesson_Y.qmd      # Individual lesson files
├── homework/                   # Student homework assignments
├── class_activities/           # In-class exercises
├── handouts/                   # Excel worksheets with answer keys
├── exams/                      # Exam materials
├── data/                       # Datasets (CSV, XLSX, Parquet)
├── outcomes/                   # Per-lesson learning objectives
├── resources/                  # Package reference guides
├── ShinyApps/                  # Interactive Shiny visualizations
├── common_functions.R          # Shared R utilities and helpers
├── _quarto.yml                 # Quarto site configuration
└── docs/                       # Rendered HTML output (GitHub Pages)
```

## Prerequisites

- [R](https://cran.r-project.org/) ≥ 4.2
- [Quarto](https://quarto.org/) ≥ 1.3
- R packages are managed via `_packages.R`; the main dependencies are:

  | Category | Packages |
  |----------|----------|
  | Time series | `fable`, `feasts`, `tsibble`, `tsibbledata` |
  | Data wrangling | `tidyverse`, `lubridate` |
  | Modeling | `nlme`, `tidymodels` |
  | Visualization | `plotly`, `patchwork`, `ggokabeito` |
  | Reporting | `kableExtra`, `gt` |

## Building the Site

```r
# Install dependencies (run once)
source("_packages.R")

# Render the full site
quarto::quarto_render()
```

The output is written to `docs/`. To preview locally:

```bash
quarto preview
```

## Assessment Breakdown

| Component | Weight |
|-----------|--------|
| Reading Journals | 10% |
| Homework | 40% |
| Projects | 25% |
| Exams | 25% |

## Data Sources

Real-world datasets included in `data/` cover economics (GDP, unemployment, energy prices), environmental data (sea ice extent, global temperatures), public health (CDC disease surveillance), and business (Apple revenue, retail).
