library(monomvn)  # Bayesian LASSO

# Same data as before
set.seed(123)
n <- 200
p <- 20
X <- matrix(rnorm(n * p), n, p)
beta_true <- c(3, -2, rep(0, p - 2))
y <- X %*% beta_true + rnorm(n)

# Bayesian LASSO fit
b_lasso <- blasso(X, y, T = 5000, verb = 500)
plot(b_lasso)
summary(b_lasso)
