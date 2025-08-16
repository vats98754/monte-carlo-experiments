library(lme4)

set.seed(123)
J <- 6   # number of groups
n_j <- 20
mu <- 5
tau <- 2

# Simulate data
group <- factor(rep(1:J, each = n_j))
theta <- rnorm(J, mu, tau)
y <- rnorm(J * n_j, rep(theta, each = n_j), 1)

# Treat all as exchangeable
ex_model <- lmer(y ~ 1 + (1 | group))
summary(ex_model)

# Treat some groups as fixed (non-exchangeable)
non_ex_group <- ifelse(group %in% c(1, 2), "nonex", "ex")
fe_model <- lm(y ~ non_ex_group)
summary(fe_model)
plot(fe_model)
