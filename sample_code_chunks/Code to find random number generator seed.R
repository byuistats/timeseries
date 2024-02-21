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
max_offset <- 4

cov_dat_init <- data.frame(
  seed_val = as.integer(),
  r_k_1 = as.integer(), r_k_2 = as.integer(),
  # xbar_1 = as.integer(), xvar_1 = as.integer(), cov_1 = as.integer(),
  # xbar_2 = as.integer(), xvar_2 = as.integer(), cov_2 = as.integer(),
  xbar_3 = as.integer(), xvar_3 = as.integer(), cov_3 = as.integer(), r_k_3 = as.integer(), three = as.integer(),
  xbar_4 = as.integer(), xvar_4 = as.integer(), cov_4 = as.integer(), r_k_4 = as.integer(), four = as.integer()
)
cov_dat <- cov_dat_init
export(cov_dat,
       "data_new.csv",
       append = FALSE
)

# set parameters
n <- 10
rho <- 0.99
mu <- 0
sigma <- 3

# seed_val <- 92401 # Temp

for (seed_val in 1:max_reps) {
  set.seed(seed_val)

  # build population correlation matrix
  tmp.r <- matrix(rho, n, n)
  tmp.r <- tmp.r^abs(row(tmp.r)-col(tmp.r))

  # simulate correlated normal random data
  x1 <- round(mvrnorm(1, rep(mu,n), sigma^2 * tmp.r),1)

  # build a data frame
  df <- data.frame(#t = 1:length(x1),
    x = x1,
    y = lead(x1, 1)) %>%
    mutate(
      xx = x - mean(x),
      xx2 = xx^2,
      xy = xx * (y - mean(x))
    ) %>%
    summarize(
      xbar = mean(x),
      xx2 = sum(xx2),
      xy = sum(xy, na.rm = TRUE)
    ) %>%
    mutate(r_k = nchar(round(abs(xy / xx2),10))) %>%
    mutate(
      xbar = nchar(round(abs(xbar),10)),
      xx2 = nchar(round(abs(xx2),10)),
      xy = nchar(round(abs(sum(xy, na.rm = TRUE)),10))
    )
  r_k_1 <- df$r_k

  # Offset = 2
  if(df$xbar <= 3 & df$xx2 <=4 & df$xy <= 4)  {
    df <- data.frame(#t = 1:length(x1),
      x = x1,
      y = lead(x1, 2)) %>%
      mutate(
        xx = x - mean(x),
        xx2 = xx^2,
        xy = xx * (y - mean(x))
      ) %>%
      summarize(
        xbar = mean(x),
        xx2 = sum(xx2),
        xy = sum(xy, na.rm = TRUE)
      ) %>%
      mutate(r_k = nchar(round(abs(xy / xx2),10))) %>%
      mutate(
        xbar = nchar(round(abs(xbar),10)),
        xx2 = nchar(round(abs(xx2),10)),
        xy = nchar(round(abs(sum(xy, na.rm = TRUE)),10))
      )
    r_k_2 <- df$r_k

    if(df$xbar <= 3 & df$xx2 <=4 & df$xy <= 4)  {
      offset <- 3
      df <- data.frame(#t = 1:length(x1),
        x = x1,
        y = lead(x1, offset)) %>%
        mutate(
          xx = x - mean(x),
          xx2 = xx^2,
          xy = xx * (y - mean(x))
        ) %>%
        summarize(
          xbar = mean(x),
          xx2 = sum(xx2),
          xy = sum(xy, na.rm = TRUE)
        ) %>%
        mutate(r_k = nchar(round(abs(xy / xx2),10))) %>%
        mutate(
          xbar = nchar(round(abs(xbar),10)),
          xx2 = nchar(round(abs(xx2),10)),
          xy = nchar(round(abs(sum(xy, na.rm = TRUE)),10))
        )

      cov_dat <- cov_dat %>%
        bind_rows(
          c(
            seed_val = seed_val,
            r_k_1 = r_k_1,
            r_k_2 = r_k_2,
            xbar_3 = df$xbar,
            xvar_3 = df$xx2,
            cov_3 = df$xy,
            r_k_3 = df$r_k,
            three = sum(df$xbar, df$xx2, df$xy)
          )
        )

      offset <- 4
      # build a data frame
      df <- data.frame(#t = 1:length(x1),
        x = x1,
        y = lead(x1, offset)) %>%
        mutate(
          xx = x - mean(x),
          xx2 = xx^2,
          xy = xx * (y - mean(x))
        ) %>%
        summarize(
          xbar = mean(x),
          xx2 = sum(xx2),
          xy = sum(xy, na.rm = TRUE)
        ) %>%
        mutate(r_k = nchar(round(abs(xy / xx2),10))) %>%
        mutate(
          xbar = nchar(round(abs(xbar),10)),
          xx2 = nchar(round(abs(xx2),10)),
          xy = nchar(round(abs(sum(xy, na.rm = TRUE)),10))
        )

        cov_dat$xbar_4[1] = df$xbar
        cov_dat$xvar_4[1] = df$xx2
        cov_dat$cov_4[1] = df$xy
        cov_dat$r_k_4[1] = df$r_k
        cov_dat$four[1] = sum(df$xbar, df$xx2, df$xy)

        export(cov_dat,
               "data_new.csv",
               append = TRUE
        )

        cov_dat <- cov_dat_init
    }
  }

  if (seed_val %% 5000 == 0) {
    print(seed_val)
  }
} # For


cov_dat <- rio::import("data_new.csv")

x <- cov_dat

x %>%
  mutate(r_K_12 = sum(r_k_1, r_k_2)) %>%



# %>%
#   # filter(offset <= 3) %>%
#   mutate(
#     cov1 = as.character(cov),
#     xbar1 = as.character(xbar),
#     ybar1 = as.character(ybar),
#     varx1 = as.character(varx),
#     vary1 = as.character(vary)
#   ) %>%
#   group_by(seed_val) %>%
#   mutate(
#     count = n(),
#     nchar = sum(
#       (nchar(xbar1) + nchar(ybar1)) * 3 + nchar(cov1) + nchar(varx1) + nchar(vary1)
#       )
#   ) #%>%
#   # select(c(offset, seed_val, xbar, ybar, cov, varx, vary, count, nchar )
#
#   x01 <- x %>% arrange(nchar) %>%
#     head(3000) %>%
#     group_by(seed_val) %>%
#     mutate(
#       ncharx = mean(nchar(xbar1)),
#       nchary = mean(nchar(ybar1))
#     ) %>%
#     filter(ncharx <= 3 & nchary <= 3) %>%
#     # select(-ncharx, -nchary) %>%
#     arrange(nchary) %>%
#     head(3000) %>%
#     arrange(nchary, nchar, seed_val, offset) %>%
#     dplyr::select(offset, seed_val, xbar1, ybar1,  varx1,vary1, cov1, ncharx,nchary) %>%
#     View()
