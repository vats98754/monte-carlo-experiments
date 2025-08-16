set.seed(42)

# Simulate data
J <- 8
n_j <- rep(5, J)
sigma2 <- 1
tau2 <- 2
mu_true <- 10
theta_true <- rnorm(J, mu_true, sqrt(tau2))
y <- lapply(theta_true, function(th) rnorm(5, th, sqrt(sigma2)))

# Priors
mu0 <- 0; sigma0_sq <- 100

# Storage
T <- 5000
theta <- matrix(0, nrow=T, ncol=J)
mu <- numeric(T)

# Init
theta[1,] <- sapply(y, mean)
mu[1] <- mean(theta[1,])

for(t in 2:T){
  # sample theta_j
  for(j in 1:J){
    ybar <- mean(y[[j]])
    prec <- n_j[j]/sigma2 + 1/tau2
    mean_post <- (n_j[j]*ybar/sigma2 + mu[t-1]/tau2) / prec
    var_post <- 1/prec
    theta[t,j] <- rnorm(1, mean_post, sqrt(var_post))
  }
  
  # sample mu
  prec_mu <- J/tau2 + 1/sigma0_sq
  mean_mu <- (J*mean(theta[t,])/tau2 + mu0/sigma0_sq) / prec_mu
  var_mu <- 1/prec_mu
  mu[t] <- rnorm(1, mean_mu, sqrt(var_mu))
}

# Traceplot
plot(mu, type='l', main=expression(mu), ylab='value')
