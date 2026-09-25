# ---------- background image ----------

## load reference tools ----------
source(here::here("images/backgrounds/0- ref.R"))


# ---------- create design ----------

## make plot ----------
bg_wave <- function(opacity = 1) {

  ## create waves ----------
  waves <- expand_grid(lane = 0:11, x = seq(-1, 15, length.out = 500)) |>
    mutate(
      y     = lane * 0.8 - 0.5 + 0.5 * sin(x * 0.7 + lane * 0.3),
      color = factor(lane %% 6 + 1, levels = 1:6)
    )

  ## construct plot ----------
  ggplot(waves, aes(x = x, y = y, group = lane, color = color)) +
    geom_path(
      linewidth = 3, lineend = "round",
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
bg_wave() |>
  ggsave(
    here("images/backgrounds/mocks", "waves.svg"),
    plot = _,
    width = 14, height = 8, bg = "transparent"
  )

# .png
bg_wave() |>
  ggsave(
    here("images/backgrounds/mocks", "waves.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )

# .png (soft)
bg_wave(opacity = 0.3) |>
  ggsave(
    here("images/backgrounds/mocks", "waves_soft.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )
