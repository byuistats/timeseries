if (!require("pacman")) install.packages("pacman")
pacman::p_load("tsibble", "fable",
               "feasts", "tsibbledata",
               "fable.prophet", "tidyverse",
               "patchwork",
               "ggthemes", "see",   # for okabeito color scheme
               "MASS", # for MVNorm
               "rio"
)

max_reps <- 6 * 10^5
max_offset <- 3

cov_dat_init <- data.frame(offset = 0, seed_val = 1:(10000 * max_offset), xbar = 0, ybar =0, cov = 0, varx = 0, vary = 0)
cov_dat <- cov_dat_init
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
    df <- data.frame(i = 1:length(x1),
                     x = x1,
                     y = lag(x1, offset)) %>%
      na.omit()

    cov_dat$offset[row_num] <- offset
    cov_dat$seed_val[row_num] <- seed_val
    cov_dat$xbar[row_num] <- mean(df$x)
    cov_dat$ybar[row_num] <- mean(df$y)
    cov_dat$cov[row_num] <- cov(df$x, df$y)
    cov_dat$varx[row_num] <- var(df$x)
    cov_dat$vary[row_num] <- var(df$y)


  }
  if (seed_val %% 10000 == 0) {
    # write.table(cov_dat, file="data.csv", append=TRUE, row.names=FALSE) # , col.names=FALSE)
    export(cov_dat,
           "data.csv",
           append = TRUE
           # ,
           # fileFormat = "csv",
           # delimiter = "\t"
           )
    cov_dat <- cov_dat_init
    row_num <- 0
    print(paste0(offset, ", ", seed_val))
  }
}

cov_dat %>%
  filter(offset <= 2) %>%
  mutate(
    xbar = as.character(xbar),
    ybar = as.character(ybar),
    varx = as.character(varx),
    vary = as.character(vary)
  ) %>%
  mutate(
    nchar = nchar(cov) + nchar(varx) + nchar(vary),
    nchar_mean = nchar(xbar) + nchar(ybar)
  ) %>%
  arrange(nchar) %>%
  head(5000) %>%
  group_by(seed_val) %>%
  mutate(count = n(), sumchar = sum(nchar_mean)) %>%
  arrange(desc(count), sumchar, seed_val, offset) %>%
  View()
