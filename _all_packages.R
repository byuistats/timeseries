######### WARNING: DO NOT USE mosaic. IT MESSES UP THE DECOMPOSITION. #need to test loading this in the front, might be ok

if (!require("pacman")) install.packages("pacman") # Installs pacman if it's not on your machine
pacman::p_load( # Installs and loads packages
  # Interactive plots
  plotly,  # Interactive visualizations with plot_ly(); loaded before tidyverse so dplyr::select() overwrites plotly::select(). Note: High conflict Potential

  # Core packages
  MASS,       # For MVRNorm(); loaded before tidyverse so dplyr::select() overwrites MASS::select()
  tidyverse,  # This will also load the dependencies; dplyr, readr, stringr, tibble, tidyr, purrr, forcats, gglot2, & lubridate

  # Statistical modeling (GLS - Chpt 6-7)
  # nlme,           # loaded before feasts to avoid ACF() conflict
  # tidymodels,     # for GLS, This will also load the dependencies; broom, rsample, dials, tune, infer, workflows, modeldata, workflowsets, parsnip, yardstick, & recipies. Note: High conflict Potential
  # multilevelmod,  # for GLS
  # broom.mixed,    # for GLS

  # tidyverts ecosystem for time series data
  tsibble,        # Tidyverse Temporal data
  tsibbledata,    # Sample Tsibble datasets
  fable,          # Forecasting Models for Tidy Time Series. Note: High conflict Potential
  feasts,         # collection of features, decomposition methods, statistical summaries and graphics for tsibble data, Loaded after nlme to avoid ACF() conflict
  fable.prophet,  # Converts prophet (forecasting) package for fable workflow

  # Data exploration & visualization
  patchwork,   # Multiple plot outputs, allows + and / with plots
  ggthemes,    # Plot styling
  see,         # okabeito color scheme
  ggokabeito,  # colorblind palette
  # ggrepel,     # For visualization annotation

  # Reporting & output
  kableExtra,  # Create nice-looking tables from data.frames
  rio,         # Easy import/export of data between R and other software
  # gt,          # Grammar of Tables for advanced table creation
  # quarto,      # For generating reports in LaTeX format

  # Additional packages
  tidyquant,    # Quantitative analysis tools using tidyverse principles, This will also load the dependencies; PerformanceAnalytics, xts, & zoo. Important Masks: ‘package:base’: as.Date, as.Date.numeric. Note: High conflict Potential
  # stringr,      # string manipulation
  # lubridate,    # date manipulation
  # nanoparquet  # For parquet files
  # data.table  # For transpose in Chapter 1 Lesson 5 (creates conflicts)
)
