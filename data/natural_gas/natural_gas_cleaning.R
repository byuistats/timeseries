if (!require("pacman")) install.packages("pacman")
pacman::p_load(MASS, # MVNorm, loaded before tidyverse so it doesn't overwrite dplyr::select()
               nlme,
               tidymodels,    # for GLS
               tidyverse, kableExtra,
               tsibble, fable,
               feasts, tsibbledata,
               fable.prophet,
               patchwork,
               ggthemes,
               see,   # okabeito color scheme
               ggokabeito, # colorblind palette
               stringr, # string manipulation
               lubridate, # date manipulation
               rio, # for easy i/o
               tidyquant,
               gt, # grammar of tables, for LaTeX in tables
               quarto, # For LaTeX mode results = 'asis'
               plotly, # For interactive figures
               multilevelmod, # for GLS
               broom.mixed    # for GLS
)

# Read in natural gas data
