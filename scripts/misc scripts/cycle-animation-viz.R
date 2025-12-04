# Install packages if needed
# install.packages(c("ggplot2", "gganimate", "lubridate", "transformr"))

library(ggplot2)
library(gganimate)
library(lubridate)

# Generate example dates
start_date <- as.Date("2025-01-01")
end_date <- as.Date("2025-12-31")
dates <- seq(start_date, end_date, by = "day")

# Define number of animation frames (e.g., one frame per week)
frames <- 52
frame_dates <- seq(start_date, end_date, length.out = frames)

# Create a data frame for the wave
wave_data <- expand.grid(date = dates, frame = 1:frames)
wave_data$days_since_start <- as.numeric(wave_data$date - start_date)

# Create a cyclical wave: sine wave scaled to months
wave_data$y <- sin(2 * pi * wave_data$days_since_start / 365)  

# Optional: add amplitude change over frames to visualize animation
wave_data$y <- wave_data$y * (1 + 0.2 * sin(2 * pi * wave_data$frame / frames))

# Plot
p <- ggplot(wave_data, aes(x = date, y = y)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = 'Cyclical Wave Over Time', subtitle = 'Frame: {frame}') +
  theme_minimal() +
  ylim(-1.5, 1.5) +
  transition_manual(frame)

# Animate
animate(p, nframes = frames, fps = 10, width = 800, height = 400)
