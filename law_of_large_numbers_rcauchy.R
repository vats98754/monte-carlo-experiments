library(ggplot2)
library(gganimate)
library(gifski)

xbars = c()
observations = c()

# Draw a new observation, calculate sample mean, store for plotting
for (n in 1:10000) {
  newdata = rcauchy(1, location=0, scale=1) # sample from population
  observations = c(observations, newdata)
  xbars = c(xbars, mean(observations))
}

# Create a data frame from the loop output
df <- data.frame(
  n = 1:length(xbars),
  xbar = xbars
)

# Create dataframe for plotting
df <- data.frame(n = 1:1000, xbar = xbars)

# Plot
p <- ggplot(df, aes(x = n, y = xbar)) +
  geom_line(color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Cauchy Distribution: Sample Mean Over Time",
    subtitle = "Step: {frame}",
    x = "Number of Observations",
    y = "Sample Mean"
  ) +
  transition_reveal(n) +
  theme_minimal(base_size = 14)

# Save as GIF
animate(p, duration=10, renderer = gifski_renderer("cauchy_lln_fail.gif"), width = 600, height = 400, fps = 30)