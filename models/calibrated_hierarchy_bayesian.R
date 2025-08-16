# source("setup.R")

# Priors informed by external data
mean_ext <- mean(external$y)
sd_ext <- sd(external$y)

calib_fit_bayes <- brm(
  y ~ x + (1 | group),
  data = internal,
  family = gaussian(),
  prior = c(
    prior(normal(mean_ext, sd_ext), class = "Intercept"),
    prior(normal(0, 1), class = "b") # regular slope prior
  ),
  chains = 4, iter = 2000, warmup = 500, seed = 123
)

summary(calib_fit_bayes)
plot(calib_fit_bayes)
