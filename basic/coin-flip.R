# level-1 Bayesian model
set.seed(42)
N <- 100000  # number of tosses
theta_true <- 0.6  # true probability of heads
tosses <- rbinom(N, size = 1, prob = theta_true)

# Count heads
heads <- sum(tosses)
tails <- N - heads

# Bayesian estimate with Beta prior
alpha_prior <- 2
beta_prior <- 2

# Posterior: Beta + Binomial → Beta(alpha + heads, beta + tails)
alpha_post <- alpha_prior + heads
beta_post <- beta_prior + tails

# Monte Carlo sample from posterior
posterior_samples <- rbeta(10000, alpha_post, beta_post)

# Plot posterior
hist(posterior_samples, main="Posterior of θ", xlab="θ", col="skyblue", breaks=50)
abline(v=theta_true, col="red", lwd=2, lty=2)
