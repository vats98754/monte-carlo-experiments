suppressPackageStartupMessages({
  library(quantmod); library(PerformanceAnalytics); library(dplyr)
})

# 1) Data: SPY last 365 days
getSymbols("SPY", from = Sys.Date()-365, auto.assign = TRUE)
ret <- dailyReturn(Ad(SPY)) # arithmetic daily return
ret <- na.omit(ret); colnames(ret) <- "r"

# 2) Estimate P-world moments
mu_hat <- mean(ret$r) * 252
sig_hat <- sd(ret$r) * sqrt(252)

cat(sprintf("P-world annualized drift mu ≈ %.4f, vol sigma ≈ %.4f\n", mu_hat, sig_hat))

# 3) Risk metrics under P via historical and Gaussian
VaR_99_hist <- quantile(ret$r, probs = 0.01)
VaR_99_gauss <- mean(ret$r) + sd(ret$r) * qnorm(0.01)
ES_99_gauss <- mean(ret$r) + sd(ret$r) * dnorm(qnorm(0.01))/0.01

cat(sprintf("Daily VaR(1%%) hist: %.4f  | Gaussian: %.4f  | ES Gaussian: %.4f\n",
            VaR_99_hist, VaR_99_gauss, ES_99_gauss))
plot(ret$r, main = "Daily Returns of SPY", ylab = "Returns", xlab = "Time")
abline(h = VaR_99_hist, col = "red", lty = 2)