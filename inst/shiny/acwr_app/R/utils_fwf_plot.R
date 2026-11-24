# ============================================================
# Build article-style FWF heatmap (Matas-Bustos et al. 2025, Fig 5-6)
# Solo grafico: los valores vienen de ACWR::fwf_cumulative() y
# ACWR::fwf_delta() (ver fwf_wide() en utils_app.R)
#
# Layout:  X = variables GPS (columnas)
#          Y = τ filas: Σ τ=1..28 arriba, Δ τ=1..28 abajo
#          Un solo día seleccionado
#          Valores normalizados (z-score) por feature
#          Escala divergente única
# ============================================================

build_fwf_combined_plot <- function(sigma_list, delta_list, vars,
                                     labels, sel_day) {
  tau_max <- 28
  rows <- list()

  for (k in seq_along(vars)) {
    s_df <- sigma_list[[vars[k]]]
    d_df <- delta_list[[vars[k]]]
    row_s <- which(s_df$day == sel_day)
    if (length(row_s) == 0) next

    s_vals <- as.numeric(s_df[row_s, -1])  # 28 valores
    d_vals <- as.numeric(d_df[row_s, -1])  # 28 valores

    # Z-score de Σ usando historial completo de ese feature
    s_mat <- as.matrix(s_df[, -1])
    for (tau in 1:tau_max) {
      col_vals <- s_mat[, tau]
      mu <- mean(col_vals, na.rm = TRUE)
      sd_val <- sd(col_vals, na.rm = TRUE)
      z <- if (!is.na(sd_val) && sd_val > 0) (s_vals[tau] - mu) / sd_val else 0
      rows[[length(rows) + 1]] <- data.frame(
        variable = labels[k],
        feature  = paste0("\u03a3 \u03c4=", tau),
        z        = z,
        raw      = s_vals[tau],
        type     = "sigma",
        tau_num  = tau,
        stringsAsFactors = FALSE
      )
    }

    # Z-score de Δ
    d_mat <- as.matrix(d_df[, -1])
    for (tau in 1:tau_max) {
      col_vals <- d_mat[, tau]
      mu <- mean(col_vals, na.rm = TRUE)
      sd_val <- sd(col_vals, na.rm = TRUE)
      z <- if (!is.na(sd_val) && sd_val > 0) (d_vals[tau] - mu) / sd_val else 0
      if (is.na(d_vals[tau])) z <- NA_real_
      rows[[length(rows) + 1]] <- data.frame(
        variable = labels[k],
        feature  = paste0("\u0394 \u03c4=", tau),
        z        = z,
        raw      = d_vals[tau],
        type     = "delta",
        tau_num  = tau,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) return(NULL)
  plot_df <- do.call(rbind, rows)

  # Ordenar Y: Σ τ=1..28 arriba, Δ τ=1..28 abajo (de arriba a abajo)
  sigma_levels <- paste0("\u03a3 \u03c4=", 1:tau_max)
  delta_levels <- paste0("\u0394 \u03c4=", 1:tau_max)
  all_levels   <- c(delta_levels[tau_max:1], sigma_levels[tau_max:1])
  plot_df$feature <- factor(plot_df$feature, levels = all_levels)

  # Variables en X
  plot_df$variable <- factor(plot_df$variable, levels = labels)

  # Clamp z-scores a [-3, 3]
  plot_df$z_clamp <- pmin(pmax(plot_df$z, -3, na.rm = TRUE), 3, na.rm = TRUE)

  ggplot(plot_df, aes(x = variable, y = feature, fill = z_clamp)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient2(
      low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
      midpoint = 0, limits = c(-3, 3),
      name = "z-score",
      na.value = "#D9D9D9"
    ) +
    # Separador visual entre Σ y Δ
    geom_hline(yintercept = tau_max + 0.5, linewidth = 1.2, color = "black") +
    annotate("text", x = 0.4, y = tau_max + tau_max / 2 + 0.5,
             label = "\u03a3", fontface = "bold", size = 4, hjust = 1) +
    annotate("text", x = 0.4, y = tau_max / 2 + 0.5,
             label = "\u0394", fontface = "bold", size = 4, hjust = 1) +
    labs(
      x = NULL,
      y = NULL,
      title = paste0("FWF \u2014 D\u00eda ", sel_day)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid      = element_blank(),
      plot.title       = element_text(size = 13, hjust = 0.5, face = "bold"),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
      axis.text.y      = element_text(size = 7),
      legend.key.height = unit(1.2, "cm"),
      legend.key.width  = unit(0.4, "cm"),
      plot.margin       = margin(10, 10, 10, 30)
    ) +
    coord_cartesian(clip = "off")
}
