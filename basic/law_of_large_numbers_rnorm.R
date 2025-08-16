library(ggplot2)
library(gganimate)
library(gifski)

xbars = c()
observations = c()

# Draw a new observation, calculate sample mean, store for plotting
for (n in 1:10000) {
  newdata = rnorm(1, mean=2, sd=1) # sample from population
  observations = c(observations, newdata)
  xbars = c(xbars, mean(observations))
}

# Create dataframe for plotting
df <- data.frame(
  n = 1:length(xbars),
  xbar = xbars
)

# Create animated plot
p <- ggplot(df, aes(x = n, y = xbar)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 2, color = "red", linetype = "dashed", linewidth = 1) +  # ← this is the horizontal line
  labs(
    title = "Law of Large Numbers",
    subtitle = "Sample Mean after {frame_along} draws",
    x = "Number of Observations (n)",
    y = "Sample Mean"
  ) +
  transition_reveal(n) +
  theme_minimal(base_size = 14)

# Render and save GIF
animate(p, fps = 30, duration = 10, width = 800, height = 500,
        renderer = gifski_renderer("normal_lln_pass.gif"))
