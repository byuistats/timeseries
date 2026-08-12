pacman::p_load(
  tidyverse,  # ggplot, mutate(), cleaning...
  tsibble,    # as_tsibble()
  fable,      # model(...), forecast(), tidy(), glance()...
  feasts,     # ACF(), PACF()
  ggtime,     # autoplot() for tsibbles
  rio,        # import()
  pander,     # pander() markdown tables
  patchwork   # + and / for ggplots
)