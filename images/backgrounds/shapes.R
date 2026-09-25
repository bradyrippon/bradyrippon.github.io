
# ---------- background image ----------

## load reference tools ----------
source(here::here("images/backgrounds/0- ref.R"))


# ---------- create design ----------

## make plot ----------
bg_shapes <- function(opacity = 1, seed = 1) {

  set.seed(seed)

  ## create shapes ----------
  .shapes <- tibble(x = numeric(), y = numeric(), size = numeric())

  # leave room around each shape, including after rotation
  for (attempt in seq_len(10000)) {
    size <- runif(1, 0.25, 0.50)
    x <- runif(1, size + 0.1, 14 - size - 0.1)
    y <- runif(1, size + 0.1, 8 - size - 0.1)

    clear_of_shapes <- all(
      sqrt((.shapes$x - x)^2 + (.shapes$y - y)^2) > .shapes$size + size + 0.5
    )

    if (clear_of_shapes) {
      .shapes <- .shapes |> add_row(x = x, y = y, size = size)
    }

    # plot no more than 40 shapes
    if (nrow(.shapes) >= 40) break
  }

  shapes <- .shapes |>
    mutate(
      id       = row_number(),
      color    = rep(1:6, length.out = n()) |> sample() |> factor(levels = 1:6),
      shape    = rep(1:3, length.out = n()) |> sample() |> factor(levels = 1:3),
      rotation = runif(n(), 0, 2 * pi),
      sides    = c(100, 4, 3)[as.integer(shape)]
    )

  ## vertices ----------
  vertices <- shapes |>
    mutate(
      points = map2(sides, rotation, function(sides, rotation) {
        tibble(
          angle = seq(0, 2 * pi, length.out = sides + 1)[seq_len(sides)] + rotation
        )
      })
    ) |>
    unnest(points) |>
    mutate(
      id     = id,
      color  = color,
      x_plot = x + size * cos(angle),
      y_plot = y + size * sin(angle),

      .keep  = "none"
    ) |>
    rename(x = x_plot, y = y_plot)

  ## construct plot ----------
  ggplot(vertices, aes(x = x, y = y, fill = color, group = id)) +
    geom_polygon(alpha = opacity, color = NA) +
    scale_fill_manual(values = palette) +
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
bg_shapes() |>
  ggsave(
    here("images/backgrounds/mocks", "shapes.svg"),
    plot = _,
    width = 14, height = 8, bg = "transparent"
  )

# .png
bg_shapes() |>
  ggsave(
    here("images/backgrounds/mocks", "shapes.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )

# .png (soft)
bg_shapes(opacity = 0.3) |>
  ggsave(
    here("images/backgrounds/mocks", "shapes_soft.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )
