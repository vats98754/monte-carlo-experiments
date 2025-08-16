# level-2 Hierarchical Bayesian model for grouped coin flip data
set.seed(1)

## ----- Simulate grouped coin data -----
J <- 30                                  # number of groups (coins)
n_j <- sample(20:60, J, replace = TRUE)  # trials per group
a_true <- 4; b_true <- 6                 # true hyperparameters (mean ~ 0.4)
theta_true <- rbeta(J, a_true, b_true)
y <- rbinom(J, size = n_j, prob = theta_true)

## ----- Hyperpriors for (a,b) -----
alpha_a <- 1; beta_a <- 1   # Gamma(shape=1, rate=1) -> Exp(1)
alpha_b <- 1; beta_b <- 1

## ----- Utilities: log-posterior for (a,b) collapsed over theta -----
log_post_ab <- function(a, b, y, n_j, alpha_a, beta_a, alpha_b, beta_b) {
  if (a <= 0 || b <= 0) return(-Inf)
  # log prior Gamma(a; alpha_a, beta_a) + Gamma(b; alpha_b, beta_b)
  lp <- (alpha_a - 1) * log(a) - beta_a * a +
    (alpha_b - 1) * log(b) - beta_b * b
  # collapsed likelihood: product_j B(a+y_j, b+n_j-y_j) / B(a,b)
  # use logs for stability: lbeta(x,y) = log Beta(x,y)
  ll <- sum(lbeta(a + y, b + n_j - y)) - J * lbeta(a, b)
  lp + ll
}

## ----- Metropolis–Hastings on (a,b) with log-normal proposals -----
mh_ab <- function(a_init, b_init, y, n_j,
                  alpha_a, beta_a, alpha_b, beta_b,
                  n_iter = 5000, prop_sd = c(0.20, 0.20), burn = 1000, thin = 2) {
  a <- a_init; b <- b_init
  out_a <- numeric(n_iter); out_b <- numeric(n_iter)
  acc <- 0L
  curr_lp <- log_post_ab(a, b, y, n_j, alpha_a, beta_a, alpha_b, beta_b)
  for (t in 1:n_iter) {
    # propose on log-scale to keep positivity
    a_prop <- a * exp(rnorm(1, 0, prop_sd[1]))
    b_prop <- b * exp(rnorm(1, 0, prop_sd[2]))
    prop_lp <- log_post_ab(a_prop, b_prop, y, n_j, alpha_a, beta_a, alpha_b, beta_b)
    # log-normal symmetry correction (Jacobian cancels in RW on log-scale)
    log_acc <- prop_lp - curr_lp
    if (log(runif(1)) < log_acc) {
      a <- a_prop; b <- b_prop
      curr_lp <- prop_lp
      acc <- acc + 1L
    }
    out_a[t] <- a; out_b[t] <- b
  }
  keep_idx <- seq(from = burn, to = n_iter, by = thin)
  list(
    a = out_a[keep_idx],
    b = out_b[keep_idx],
    acc_rate = acc / n_iter
  )
}

## ----- Run MH for (a,b) -----
ab_draws <- mh_ab(
  a_init = 1, b_init = 1,
  y = y, n_j = n_j,
  alpha_a = alpha_a, beta_a = beta_a,
  alpha_b = alpha_b, beta_b = beta_b,
  n_iter = 12000, prop_sd = c(0.15, 0.15), burn = 2000, thin = 5
)

cat(sprintf("MH acceptance rate: %.3f\n", ab_draws$acc_rate))
a_hat <- mean(ab_draws$a); b_hat <- mean(ab_draws$b)
cat(sprintf("Posterior mean of (a,b): (%.3f, %.3f)\n", a_hat, b_hat))

## ----- Draw group-level thetas given (a,b) and data (conjugate) -----
## We can either:
##  1) plug in (a_hat, b_hat)  [Empirical-Bayes-ish], or
##  2) draw per-iteration thetas paired with (a,b) draws.
## Below is a simple plug-in to visualize shrinkage.

theta_post_mean <- (a_hat + y) / (a_hat + b_hat + n_j)  # E[theta_j | a,b,y]
theta_mle       <- y / n_j                               # no pooling (per-group MLE)
theta_pool_mean <- sum(y) / sum(n_j)                     # complete pooling
theta_full_pool <- rep(theta_pool_mean, J)

## ----- Compare shrinkage: no pooling vs partial vs complete pooling -----
shrinkage <- data.frame(
  group = 1:J,
  n = n_j,
  y = y,
  mle = theta_mle,
  partial_pool = theta_post_mean,
  full_pool = theta_full_pool,
  theta_true = theta_true
)

print(head(shrinkage, 10))

## ----- (Optional) posterior samples for theta_j with uncertainty bands -----
S <- length(ab_draws$a)
theta_samples <- matrix(NA_real_, nrow = S, ncol = J)
for (s in 1:S) {
  a_s <- ab_draws$a[s]; b_s <- ab_draws$b[s]
  theta_samples[s, ] <- rbeta(J, a_s + y, b_s + n_j - y)
}
theta_ci_low  <- apply(theta_samples, 2, quantile, probs = 0.05)
theta_ci_high <- apply(theta_samples, 2, quantile, probs = 0.95)

## Bind into results
shrinkage$ci5  <- theta_ci_low
shrinkage$ci95 <- theta_ci_high

## ----- Minimal base R plots (optional) -----
par(mfrow = c(1, 2))
plot(ab_draws$a, type = "l", main = "Trace: a", ylab = "a", xlab = "iter")
plot(ab_draws$b, type = "l", main = "Trace: b", ylab = "b", xlab = "iter")
par(mfrow = c(1, 1))

plot(theta_mle, theta_post_mean,
     xlab = "No pooling MLE (y_j / n_j)",
     ylab = "Partial pooling E[θ_j | a,b,y]",
     main = "Shrinkage toward pooled mean")
abline(a = 0, b = 1, lty = 2)
abline(h = theta_pool_mean, lty = 3)