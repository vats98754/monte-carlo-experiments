# source("setup.R")

# Example dataset: sleepstudy from lme4
data("sleepstudy")

# Frequentist MEM
mem_fit <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
summary(mem_fit)

# Parametric bootstrap
boot_fn <- function(fit) fixef(fit)
boot_results <- boot(mem_fit, boot_fn, R = 1000)
boot.ci(boot_results, type = "perc")
