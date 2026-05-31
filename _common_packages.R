pacman::p_load(
  tidyverse,  # ggplot, mutate(), cleaning...
  tsibble,    # as_tsibble()
  fable,      # model(...), forecast(), tidy(), glance()...
  feasts,     # ACF(), PACF()
  ggtime,     # autoplot() for tsibbles
  patchwork,  # + and / for ggplots
  rio         # import()
)