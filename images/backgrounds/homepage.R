
library(tidyverse)
library(svglite)

routes <- tribble(
  ~route, ~x, ~y,
  "red",    -1, 8,   "red",     2, 8,   "red",     2, 5,   "red",     7, 5,   "red",     7, 2,   "red",    13, 2,
  "gold",   -1, 3,   "gold",    4, 3,   "gold",    4, 7,   "gold",    9, 7,   "gold",    9, 10,
  "blue",    1, -1,  "blue",    1, 4,   "blue",    5, 4,   "blue",    5, 9,   "blue",   12, 9,
  "green",  -1, 6,   "green",   3, 6,   "green",   3, 1,   "green",  10, 1,   "green",  10, 5,   "green", 13, 5,
  "purple",  6, -1,  "purple",  6, 3,   "purple", 11, 3,   "purple", 11, 8,   "purple", 13, 8,
  "gray",   -1, 1,   "gray",    8, 1,   "gray",    8, 4,   "gray",   13, 4
) |>
  mutate(
    x = as.numeric(x),
    y = as.numeric(y)
  )

plot <-
  ggplot(routes, aes(x, y, group = route, color = route)) +
  geom_path(
    linewidth = 6,
    lineend = "round",
    linejoin = "round"
  ) +
  scale_color_manual(
    values = c(
      red = "#B31B1B",
      gold = "#F2A900",
      blue = "#3B82F6",
      green = "#10B981",
      purple = "#7C3AED",
      gray = "#6B7280"
    )
  ) +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 9), expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(
  "images/backgrounds/homepage.svg",
  plot,
  width = 14,
  height = 8,
  bg = "transparent"
)