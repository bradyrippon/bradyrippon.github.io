
# ---------- background image ----------

## load reference tools ----------

# common libraries
source(here::here("images/backgrounds/0- ref.R"))


# ---------- create design ----------

## define shape styles ----------
motif <- function(kind) {
  
  t <- seq(0, 1, length.out = 160)
  
  # what to do for each kind of shape
  switch(
    kind,
    spiral = tibble(
      x = (.08 + 0.55 * t) * cos(2.8 *t * pi),
      y = (.08 + 0.55 * t) * sin(2.8 *t * pi)
    ),
    wave = tibble(
      x = (t - 0.5) * 1.3,
      y = 0.23 * sin(3.5 * t * pi)
    ),
    loop = tibble(
      x = (t - 0.5) + 0.25 * sin(3.5 * t * pi),
      y = 0.3 * cos(3.5 * t * pi)
    ),
    arch = {
      a <- seq(-0.3, 1.65 * pi, length.out = length(t))
      tibble(
        x = 0.4 * cos(a),
        y = 0.57 * sin(a)
      )
    },
    zigzag = tibble(
      x = seq(-0.65, 0.65, length.out = 7),
      y = c(-0.18, 0.22, -0.22, 0.22, -0.22, 0.22, -0.18)
    ),
    tibble(
      x = (t - 0.5) * 1.2,
      y = 0.27 * sin(t * 2 * pi) + 0.12 * sin(t * 4 * pi + 0.3)
    )
  )
}


## make plot ----------
bg_abstract <- function(opacity = 1, seed = 81) {
  
  set.seed(seed)
  
  ## create shapes ----------
  .kinds <- c("spiral", "wave", "loop", "arch", "zigzag", "swoosh")
  
  shapes <- expand_grid(row = 0:5, col = 0:9) |>
    mutate(
      id    = row_number(),
      kind  = .kinds |> rep(length.out = n()) |> sample(),
      color = rep(1:6, length.out = n()) |> sample() |> factor(levels = 1:6),
      angle = runif(n(), 0, 2 * pi),
      size  = runif(n(), 0.8, 1.12),
      cx    = col * 1.56 + (row %% 2) * 0.3 + runif(n(), -0.16, 0.16),
      cy    = row * 1.6 + runif(n(), -0.15, 0.15)
    )
  
  
  ## paths ----------
  paths <- shapes |>
    mutate(points = map(kind, motif)) |>
    unnest(points) |>
    mutate(
      id     = id,
      color  = color,
      x_plot = cx + size * (x * cos(angle) - y * sin(angle)),
      y_plot = cy + size * (x * sin(angle) + y * cos(angle)),
      
      .keep  = "none"
    ) |>
    rename(x = x_plot, y = y_plot)
  
  
  ## dots ----------
  .dots <- tibble(x = numeric(), y = numeric())
  
  for (attempt in seq_len(5000)) {
    x <- runif(1, 0, 14)
    y <- runif(1, 0, 8)
    
    clear_of_paths <- all(
      (paths$x - x)^2 + (paths$y - y)^2 > 0.22^2
    )
    
    clear_of_dots <- all(
      (.dots$x - x)^2 + (.dots$y - y)^2 > 0.45^2
    )
    
    if (clear_of_paths && clear_of_dots) {
      .dots <- .dots |> add_row(x = x, y = y)
    }
    
    # plot no more than 25 dots
    if (nrow(.dots) >= 25) break
  }
  
  # assign dots color and sizes
  dots <- .dots |>
    mutate(
      color = rep(1:6, length.out = n()) |> sample() |> factor(levels = 1:6),
      size  = runif(n(), 5, 7)
    )
  
  
  ## construct plot ----------
  ggplot(mapping = aes(x = x, y = y, color = color)) +
    geom_path(
      data = paths, 
      aes(group = id),
      linewidth = 4, lineend = "round", linejoin = "round",
      alpha = opacity
    ) +
    geom_point(
      data = dots, 
      aes(size = size),
      alpha = opacity, stroke = 0
    ) +
    scale_color_manual(values = palette) +
    scale_size_identity() +
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
bg_abstract() |> 
  ggsave(
  here("images/backgrounds/mocks", "abstract.svg"),
  plot = _,
  width = 14, height = 8, bg = "transparent"
)

# .png
bg_abstract() |> 
  ggsave(
    here("images/backgrounds/mocks", "abstract.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )

# .png (soft)
bg_abstract(0.3) |> 
  ggsave(
    here("images/backgrounds/mocks", "abstract_soft.png"),
    plot = _,
    width = 14, height = 8, dpi = 300, bg = "white"
  )
