if (!require("pacman")) install.packages("pacman")
pacman::p_load("tsibble", "fable",
               "feasts", "tsibbledata",
               "fable.prophet", "tidyverse",
               "patchwork",
               "ggthemes", "see",   # for okabeito color scheme
               "MASS", # for MVNorm
               "rio"
)

max_reps <- 10^6
max_offset <- 3

max_buffer <- 10000

cov_dat_init <- data.frame(offset = 0, seed_val = 1:(max_buffer * max_offset), xbar = 0, ybar =0, cov = 0, varx = 0, vary = 0)
cov_dat <- cov_dat_init
export(cov_dat %>% mutate(offset = NA) %>% na.omit(),
       "data_new.csv",
       append = FALSE
)

row_num <- 0
for (seed_val in 1:max_reps) {
  for (offset in 1:max_offset) {
    row_num <- row_num + 1
    set.seed(seed_val)

    # set parameters
    n <- 10
    rho <- 0.99
    mu <- 10
    sigma <- 3

    # build population correlation matrix
    tmp.r <- matrix(rho, n, n)
    tmp.r <- tmp.r^abs(row(tmp.r)-col(tmp.r))

    # simulate correlated normal random data
    x1 <- round(mvrnorm(1, rep(mu,n), sigma^2 * tmp.r),1)

    # build a data frame
    df <- data.frame(#t = 1:length(x1),
                     x = x1,
                     y = lead(x1, offset)) %>%
      mutate(
        xx = x - mean(x),
        xx2 = xx^2,
        yy = y - mean(x),
        yy2 = yy^2,
        xy = xx * yy
      )

    cov_dat$offset[row_num] <- offset
    cov_dat$seed_val[row_num] <- seed_val
    cov_dat$xbar[row_num] <- mean(df$x)
    cov_dat$ybar[row_num] <- mean(df$y, na.rm = TRUE)
    cov_dat$cov[row_num] <- sum(df$xy, na.rm = TRUE) / n
    cov_dat$varx[row_num] <- sum(df$xx2) / n
    cov_dat$vary[row_num] <- sum(df$yy2, na.rm = TRUE) / n
  }

  if (seed_val %% max_buffer == 0) {
    export(cov_dat,
           "data_new.csv",
           append = TRUE
           )
    cov_dat <- cov_dat_init
    row_num <- 0
    print(paste0(offset, ", ", seed_val))
  }
}

cov_dat <- rio::import("data_new.csv")

x <- cov_dat %>%
  # filter(offset <= 3) %>%
  mutate(
    cov1 = as.character(cov),
    xbar1 = as.character(xbar),
    ybar1 = as.character(ybar),
    varx1 = as.character(varx),
    vary1 = as.character(vary)
  ) %>%
  group_by(seed_val) %>%
  mutate(
    count = n(),
    nchar = sum(
      (nchar(xbar1) + nchar(ybar1)) * 3 + nchar(cov1) + nchar(varx1) + nchar(vary1)
      )
  ) #%>%
  # select(c(offset, seed_val, xbar, ybar, cov, varx, vary, count, nchar )

  x01 <- x %>% arrange(nchar) %>%
    head(3000) %>%
    group_by(seed_val) %>%
    mutate(
      ncharx = mean(nchar(xbar1)),
      nchary = mean(nchar(ybar1))
    ) %>%
    filter(ncharx <= 3 & nchary <= 3) %>%
    # select(-ncharx, -nchary) %>%
    arrange(nchary) %>%
    head(3000) %>%
    arrange(nchary, nchar, seed_val, offset) %>%
    dplyr::select(offset, seed_val, xbar1, ybar1,  varx1,vary1, cov1, ncharx,nchary) %>%
    View()
