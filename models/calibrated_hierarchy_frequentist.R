# source("setup.R")

# Simulated internal + external calibration datasets
set.seed(123)
internal <- data.frame(
  y = rnorm(50, 10, 2),
  x = rnorm(50),
  group = rep(1:5, each = 10)
)

external <- data.frame(
  y = rnorm(50, 11, 2),
  x = rnorm(50)
)

# Compute calibration weight (simple example: inverse variance ratio)
internal_var <- var(internal$y)
external_var <- var(external$y)
weight_ratio <- internal_var / external_var

# Fit weighted hierarchical model
calib_fit_freq <- lmer(
  y ~ x + (1 | group),
  data = internal,
  weights = rep(weight_ratio, nrow(internal))
)

summary(calib_fit_freq)
