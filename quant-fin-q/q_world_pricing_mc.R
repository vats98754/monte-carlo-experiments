# Monte Carlo risk-neutral pricing of a European call
bs_mc_call <- function(S0, K, T, r, sigma, n_paths = 1e5){
  Z <- rnorm(n_paths)
  ST <- S0 * exp((r - 0.5*sigma^2)*T + sigma*sqrt(T)*Z) # Q-dynamics
  mean(pmax(ST - K, 0)) * exp(-r*T)
}

# sanity check vs closed form
bs_call <- function(S0, K, T, r, sigma){
  d1 <- (log(S0/K) + (r + 0.5*sigma^2)*T) / (sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  S0*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
}

S0 <- 500; K <- 500; T <- 0.5; r <- 0.03; sigma <- 0.2
set.seed(1)
mc  <- bs_mc_call(S0,K,T,r,sigma, n_paths = 200000)
bsc <- bs_call(S0,K,T,r,sigma)
cat(sprintf("MC(Q) ≈ %.4f  |  Black–Scholes ≈ %.4f\n", mc, bsc))

# plot the distribution of ST (under Q-measure)
library(ggplot2)
library(dplyr)
set.seed(1)
Z <- rnorm(200000)
ST <- S0 * exp((r - 0.5*sigma^2)*T + sigma*sqrt(T)*Z)
df_ST <- data.frame(ST = ST)
ggplot(df_ST, aes(x = ST)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = S0 * exp(r * T), sd = S0 * sigma * sqrt(T)),
                color = "red", size = 1) +
  labs(title = "Distribution of ST under Q-measure",
       x = "Stock Price at T (ST)", y = "Density") +
  theme_minimal()
