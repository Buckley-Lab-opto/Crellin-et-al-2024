library(ggplot2)

angles <- seq(-180, 180, by = 45)
angles <- as.data.frame(angles)

bars <- data.frame(
  angle = c(90, -90),
  direction = c('posterior', 'anterior')
)

ggplot() +
  geom_col(data = bars, aes(x = angle, y = 1, fill = direction), width = 90) +
  geom_point(data = angles, aes(x = angles, y = 1), size = 0.1) +
  coord_polar(theta = "x", start = 1.57, direction = -1) + 
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Reference for vector direction by degrees")

# Plot angles in a given region on circular plot
# Can do just do angles
# Or vectors
# Can do as a density, or plot points
# Could do two transparent densities to compare two regions by overlap (or more than two, but maybe messy)