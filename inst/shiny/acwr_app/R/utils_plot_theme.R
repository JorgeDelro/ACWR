# ============================================================
# Tema de gráficos compartido (dark/light mode)
# ============================================================

get_plot_theme <- function(is_dark = FALSE) {
  list(
    is_dark    = is_dark,
    bg         = "transparent",
    text_color = if (is_dark) "#E0E0E0" else "#2C3E50",
    grid_color = if (is_dark) "#444444" else "#E0E0E0",
    bar_fill   = if (is_dark) "#7B8CA3" else "#B8C5D6",
    bar_border = if (is_dark) "#6A7B8F" else "#9BAEC1"
  )
}
