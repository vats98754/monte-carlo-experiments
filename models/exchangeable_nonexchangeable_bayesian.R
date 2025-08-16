library(brms)

set.seed(123)
J <- 6
n_j <- 20
mu <- 5
tau <- 2

group <- factor(rep(1:J, each = n_j))
theta <- rnorm(J, mu, tau)
y <- rnorm(J * n_j, rep(theta, each = n_j), 1)
dat <- data.frame(y, group)

# Exchangeable groups model
ex_formula <- bf(y ~ 1 + (1 | group))
ex_fit <- brm(ex_formula, data = dat, chains = 2, iter = 2000)

# Non-exchangeable: separate priors for some groups
# Here we'll treat group 1 & 2 as fixed, others random
dat$group_type <- ifelse(dat$group %in% c(1, 2), "nonex", "ex")
nex_formula <- bf(y ~ 0 + group_type + (1 | group))
nex_fit <- brm(nex_formula, data = dat, chains = 2, iter = 2000)

summary(ex_fit)
plot(ex_fit)
summary(nex_fit)
plot(nex_fit)