library(glmnet)

# Simulate sparse regression data
set.seed(123)
n <- 200
p <- 20
X <- matrix(rnorm(n * p), n, p)
beta_true <- c(3, -2, rep(0, p - 2))
y <- X %*% beta_true + rnorm(n)

# Fit LASSO (Frequentist)
lasso_fit <- cv.glmnet(X, y, alpha = 1)  # alpha=1 means LASSO
plot(lasso_fit)
coef(lasso_fit, s = "lambda.min")
