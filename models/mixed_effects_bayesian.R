# source("setup.R")
data("sleepstudy")

# Bayesian MEM using brms
mem_bayes <- brm(
  Reaction ~ Days + (1 | Subject),
  data = sleepstudy,
  family = gaussian(),
  chains = 4, iter = 2000, warmup = 500, seed = 123
)

summary(mem_bayes)
plot(mem_bayes)
posterior_interval(mem_bayes, prob = 0.95)
