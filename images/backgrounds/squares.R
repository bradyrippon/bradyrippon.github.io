
# ---------- background image ----------

## load reference tools ----------
source(here::here("images/backgrounds/0- ref.R"))


# ---------- create design ----------

## make plot ----------
bg_squares <- function(opacity = 1, seed = 99) {

  set.seed(seed)

  ## create squares ----------
  squares <- tibble(id = seq_len(75)) |>
    mutate(
      x     = runif(n(), -1, 15),
      y     = runif(n(), -1, 15),
      side  = runif(n(),  1, 3),
      color = rep(1:6, length.out = n()) |> sample() |> factor(levels = 1:6)
    )

  ## construct plot ----------
  ggplot(squares, aes(
    xmin = x - side / 2, xmax = x + side / 2,
    ymin = y - side / 2, ymax = y + side / 2,
    color = color
  )) +
    geom_rect(
      fill = NA, linewidth = 1.75, linejoin = "mitre",
      alpha = opacity
    ) +
    scale_color_manual(values = palette) +
    coord_fixed(
      xlim = c(0, 14),
      ylim = c(0, 8),
      expand = FALSE
    ) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(
        fill = "transparent",
        color = NA
      )
    )
}


# ---------- save image ----------

# .svg
bg_squares() |>
  ggsave(
    here("images/backgrounds/mocks", "squares.svg"),
    plot = _,
    width = 14, height = 8, bg = "transparent"
  )

# .png
bg_squares() |>
  ggsave(
    here("images/backgrounds/mocks", "squares.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )

# .png (soft)
bg_squares(opacity = 0.3) |>
  ggsave(
    here("images/backgrounds/mocks", "squares_soft.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )
