install.packages(c("lme4", "lmerTest", "boot", "ggplot2",
                   "brms", "rstanarm", "bayesplot",
                   "glmnet", "dplyr"))

library(lme4)
library(lmerTest)
library(boot)
library(ggplot2)
library(brms)
library(rstanarm)
library(bayesplot)
library(glmnet)
library(dplyr)

# Speed up Stan
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
