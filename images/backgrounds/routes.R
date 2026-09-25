
# ---------- background image ----------

## load reference tools ----------
source(here::here("images/backgrounds/0- ref.R"))


# ---------- create design ----------

## make plot ----------
bg_routes <- function(opacity = 1) {

  ## create routes ----------
  routes <- tribble(
    ~id, ~x, ~y,
    1, -1, 8,
    1,  2, 8,
    1,  2, 5,
    1,  7, 5,
    1,  7, 2,
    1, 13, 2,
    
    2, -1, 3,
    2,  4, 3,
    2,  4, 7,
    2,  9, 7,
    2,  9, 10,
    
    3,  1, -1,
    3,  1, 4,
    3,  5, 4,
    3,  5, 8.5,
    3, 12, 8.5,
    
    4, -1, 6,
    4,  3, 6,
    4,  3, 1,
    4, 10, 1,
    4, 10, 5,
    4, 13, 5,
    
    5,  6, -1,
    5,  6, 3,
    5, 11, 3,
    5, 11, 8,
    5, 13, 8,
    
    6, -1, 1.5,
    6,  8, 1.5,
    6,  8, 6,
    6, 13, 6
  ) |>
    mutate(color = id |> factor(levels = 1:6))

  ## construct plot ----------
  ggplot(routes, aes(x = x, y = y, group = id, color = color)) +
    geom_path(
      linewidth = 6,
      lineend = "round",
      linejoin = "round",
      alpha = opacity
    ) +
    scale_color_manual(values = palette) +
    coord_cartesian(
      xlim = c(0, 12),
      ylim = c(0, 9),
      expand = FALSE,
      clip = "off"
    ) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(
        fill = "transparent",
        color = NA
      ),
      panel.background = element_rect(
        fill = "transparent",
        color = NA
      )
    )
}


# ---------- save image ----------

# .svg
bg_routes() |>
  ggsave(
    here("images/backgrounds/mocks", "routes.svg"),
    plot = _,
    width = 14, height = 8, bg = "transparent"
  )

# .png
bg_routes() |>
  ggsave(
    here("images/backgrounds/mocks", "routes.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )

# .png (soft)
bg_routes(opacity = 0.3) |>
  ggsave(
    here("images/backgrounds/mocks", "routes_soft.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )
