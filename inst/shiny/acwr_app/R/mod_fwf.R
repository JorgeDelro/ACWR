# ============================================================
# Módulo: Análisis FWF (Footballer Workload Footprint)
# ============================================================

fwfUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "FWF - Par\u00e1metros",
      width = 350,
      sliderInput(ns("fwf_day_select"), "D\u00eda para tabla de features:",
                  min = 1, max = 28, value = 28, step = 1),
      hr(),
      uiOutput(ns("fwf_var_info")),
      hr(),
      tags$strong("Descargar resultados:"),
      tags$div(
        style = "margin-top: 6px;",
        tags$p(class = "text-muted mb-1", style = "font-size: 0.85em;", "Heatmaps (PNG)"),
        tags$div(
          class = "d-flex flex-column gap-2 mb-2",
          downloadButton(ns("download_fwf_sigma_png"), "\u2211\u1d40FWF Acumulativas", class = "btn-outline-secondary btn-sm"),
          downloadButton(ns("download_fwf_delta_png"), "\u0394\u1d40FWF Temporales", class = "btn-outline-secondary btn-sm"),
          downloadButton(ns("download_fwf_combined_png"), "FWF Combinado", class = "btn-outline-secondary btn-sm")
        ),
        tags$p(class = "text-muted mb-1 mt-2", style = "font-size: 0.85em;", "Tabla de features"),
        downloadButton(ns("download_fwf_xlsx"), "XLSX Features", class = "btn-outline-success btn-sm w-100")
      )
    ),

    # Panel principal con sub-pestañas
    navset_card_tab(
      id = ns("fwf_tabs"),
      title = "FWF",

      nav_panel(
        title = "\u2211\u1d40FWF",
        icon = icon("layer-group"),
        card_body(
          card(
            card_header(tags$span(style = "font-size: 0.95em; font-weight: 600;",
              "\u2211\u1d40FWF \u2014 Variaciones acumulativas")),
            card_body(uiOutput(ns("fwf_sigma_heatmap_ui")))
          ),
          tags$p(
            class = "text-muted mt-2",
            style = "font-size: 0.8em; font-style: italic;",
            "Referencia: Matas-Bustos JB, Mora-Garc\u00eda AM, de Hoyo Lora M, ",
            "Nieto-Alarc\u00f3n A, Gonzalez-Fern\u00e1ndez FT (2025). ",
            "PLoS One 20(7): e0327960. doi: 10.1371/journal.pone.0327960"
          )
        )
      ),

      nav_panel(
        title = "\u0394\u1d40FWF",
        icon = icon("arrow-right-arrow-left"),
        card_body(
          card(
            card_header(tags$span(style = "font-size: 0.95em; font-weight: 600;",
              "\u0394\u1d40FWF \u2014 Variaciones temporales")),
            card_body(uiOutput(ns("fwf_delta_heatmap_ui")))
          ),
          tags$p(
            class = "text-muted mt-2",
            style = "font-size: 0.8em; font-style: italic;",
            "Referencia: Matas-Bustos JB, Mora-Garc\u00eda AM, de Hoyo Lora M, ",
            "Nieto-Alarc\u00f3n A, Gonzalez-Fern\u00e1ndez FT (2025). ",
            "PLoS One 20(7): e0327960. doi: 10.1371/journal.pone.0327960"
          )
        )
      ),

      nav_panel(
        title = "FWF",
        icon = icon("braille"),
        card_body(
          fillable = FALSE,
          card(
            fill = FALSE,
            card_header(tags$span(style = "font-size: 0.95em; font-weight: 600;",
              "\u2211\u1d40FWF + \u0394\u1d40FWF \u2014 D\u00eda seleccionado (todas las variables)")),
            card_body(
              fillable = FALSE,
              tags$p(class = "text-muted", style = "font-size: 0.85em;",
                "Representaci\u00f3n estilo art\u00edculo: X = variables GPS, Y = \u03c4 (ventanas), ",
                "valores normalizados (z-score). Usa el slider del panel lateral para cambiar el d\u00eda."),
              uiOutput(ns("fwf_combined_heatmap_ui"))
            )
          ),
          tags$p(
            class = "text-muted mt-2",
            style = "font-size: 0.8em; font-style: italic;",
            "Referencia: Matas-Bustos JB, Mora-Garc\u00eda AM, de Hoyo Lora M, ",
            "Nieto-Alarc\u00f3n A, Gonzalez-Fern\u00e1ndez FT (2025). ",
            "PLoS One 20(7): e0327960. doi: 10.1371/journal.pone.0327960"
          )
        )
      ),

      nav_panel(
        title = "Features",
        icon = icon("table"),
        card_body(
          tags$h6(class = "mb-2", textOutput(ns("fwf_table_title"))),
          DTOutput(ns("fwf_table")),
          tags$p(
            class = "text-muted mt-3",
            style = "font-size: 0.8em; font-style: italic;",
            "Referencia: Matas-Bustos JB, Mora-Garc\u00eda AM, de Hoyo Lora M, ",
            "Nieto-Alarc\u00f3n A, Gonzalez-Fern\u00e1ndez FT (2025). ",
            "Advanced feature engineering in Acute:Chronic Workload Ratio (ACWR) ",
            "calculation for injury forecasting in elite soccer. ",
            "PLoS One 20(7): e0327960. doi: 10.1371/journal.pone.0327960"
          )
        )
      ),

      nav_panel(
        title = "Teor\u00eda",
        icon = icon("book"),
        card_body(
          withMathJax(),
          includeHTML("www/theory_fwf.html")
        )
      )
    )
  )
}

fwfServer <- function(id, current_data, plot_theme) {
  moduleServer(id, function(input, output, session) {

    # Detectar variables de carga disponibles
    fwf_load_vars <- reactive({
      req(current_data())
      df <- current_data()
      all_cols <- names(df)
      extra <- setdiff(all_cols, c("date", "load"))
      if (length(extra) == 0) return("load")
      is_num <- vapply(extra, function(cn) is.numeric(df[[cn]]), logical(1))
      extra_num <- extra[is_num]
      if (length(extra_num) > 0) {
        return(extra_num)
      } else {
        return("load")
      }
    })

    # Nombres legibles para variables
    fwf_var_labels <- reactive({
      vars <- fwf_load_vars()
      labels <- c(
        "sRPE_UA"      = "sRPE (UA)",
        "distancia_m"  = "Distancia (m)",
        "dist_21_kmh"  = "Dist >21 km/h (m)",
        "player_load"  = "Player Load",
        "n_acc_3"      = "Acel >3 m/s\u00b2",
        "dist_hmld"    = "HMLD (m)",
        "load"         = "UA"
      )
      ifelse(vars %in% names(labels), labels[vars], vars)
    })

    # Sigma y delta para TODAS las variables, con el paquete ACWR
    # (días de calendario: sesiones del mismo día sumadas, descanso = 0)
    fwf_db <- reactive({
      req(current_data())
      df <- current_data()
      db <- df[, fwf_load_vars(), drop = FALSE]
      db$ID <- 1
      db$date <- df$date
      db
    })

    time_unit <- reactive(attr(current_data(), "time"))

    # Día mostrado -> fecha (solo para los títulos con datos con fechas)
    day_label <- function(day) {
      df <- current_data()
      if (!identical(time_unit(), "date")) return(paste("D\u00eda", day))
      paste0("D\u00eda ", day, " (", format(min(df$date) + day - 1), ")")
    }

    fwf_sigma_list <- reactive({
      vars <- fwf_load_vars()
      long <- ACWR::fwf_cumulative(fwf_db(), ID = "ID", vars = vars,
                                   training_dates = "date", windows = 1:28)
      lapply(setNames(vars, vars), function(v) fwf_wide(long, v, "sigma", time_unit()))
    })

    fwf_delta_list <- reactive({
      vars <- fwf_load_vars()
      long <- ACWR::fwf_delta(fwf_db(), ID = "ID", vars = vars,
                              training_dates = "date", windows = 1:28)
      lapply(setNames(vars, vars), function(v) fwf_wide(long, v, "delta", time_unit()))
    })

    observe({
      req(current_data())
      days <- day_number(current_data()$date, time_unit())
      updateSliderInput(session, "fwf_day_select",
        min = min(days), max = max(days), value = max(days))
    })

    # Info dinámica en sidebar
    output$fwf_var_info <- renderUI({
      vars <- fwf_load_vars()
      labels <- fwf_var_labels()
      n <- length(vars)
      if (n == 1 && vars[1] == "load") {
        tags$div(
          style = "padding: 10px; border-radius: 8px; border-left: 4px solid var(--bs-info); background: var(--bs-tertiary-bg); font-size: 0.85em;",
          icon("info-circle"),
          " Variable de carga: UA (unidades arbitrarias). ",
          "El FWF alcanza su m\u00e1ximo potencial con m\u00faltiples m\u00e9tricas GPS."
        )
      } else {
        tags$div(
          style = "padding: 10px; border-radius: 8px; border-left: 4px solid var(--bs-success); background: var(--bs-tertiary-bg); font-size: 0.85em;",
          icon("check-circle"),
          tags$strong(paste0(n, " variables de carga detectadas:")),
          tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
            lapply(labels, function(lb) tags$li(lb))
          ),
          tags$small(class = "text-muted",
            paste0("Total features: ", n * 28, " \u2211\u1d40 + ", n * 28, " \u0394\u1d40 = ", n * 56))
        )
      }
    })

    # Altura dinámica de heatmaps
    fwf_heatmap_height <- reactive({
      n <- length(fwf_load_vars())
      paste0(max(120, n * 55 + 80), "px")
    })

    output$fwf_sigma_heatmap_ui <- renderUI({
      plotOutput(session$ns("fwf_sigma_heatmap"), height = fwf_heatmap_height())
    })

    output$fwf_delta_heatmap_ui <- renderUI({
      plotOutput(session$ns("fwf_delta_heatmap"), height = fwf_heatmap_height())
    })

    # Función interna: construir ggplot sigma heatmap
    build_fwf_sigma_plot <- function(sigma_list, vars, labels, sel_day) {
      plot_rows <- list()
      for (k in seq_along(vars)) {
        df_s <- sigma_list[[vars[k]]]
        row_idx <- which(df_s$day == sel_day)
        if (length(row_idx) == 0) next
        vals <- as.numeric(df_s[row_idx, -1])
        plot_rows[[k]] <- data.frame(
          tau = factor(1:28), variable = labels[k],
          value = vals, stringsAsFactors = FALSE
        )
      }
      if (length(plot_rows) == 0) return(NULL)
      plot_df <- do.call(rbind, plot_rows)
      plot_df$variable <- factor(plot_df$variable, levels = rev(labels))

      if (length(vars) > 1) {
        plot_df$norm_value <- ave(plot_df$value, plot_df$variable, FUN = function(x) {
          rng <- range(x, na.rm = TRUE)
          if (rng[1] == rng[2]) rep(0.5, length(x))
          else (x - rng[1]) / (rng[2] - rng[1])
        })
        txt_size <- if (length(vars) > 3) 2.2 else 3
        ggplot(plot_df, aes(x = tau, y = variable, fill = norm_value)) +
          geom_tile(color = "white", linewidth = 0.5) +
          geom_text(aes(label = ifelse(is.na(value), "", round(value, 0))),
                    size = txt_size, color = "white") +
          scale_fill_gradient(low = "#A8A4CE", high = "#2D2780",
                              name = "Normalizado", na.value = "#E0E0E0",
                              labels = c("Min", "Max"), breaks = c(0, 1)) +
          scale_x_discrete(breaks = seq(2, 28, by = 2)) +
          labs(x = expression(paste("Ventana ", tau, " (d\u00edas)")),
               y = NULL, title = paste("\u2211\u1d40FWF \u2014 D\u00eda", sel_day)) +
          theme_minimal(base_size = 13) +
          theme(axis.text.y = element_text(face = "bold", size = 10),
                panel.grid = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5))
      } else {
        ggplot(plot_df, aes(x = tau, y = variable, fill = value)) +
          geom_tile(color = "white", linewidth = 0.5) +
          geom_text(aes(label = round(value, 0)), size = 3, color = "white") +
          scale_fill_gradient(low = "#A8A4CE", high = "#2D2780",
                              name = "Carga (UA)", na.value = "#E0E0E0") +
          scale_x_discrete(breaks = seq(2, 28, by = 2)) +
          labs(x = expression(paste("Ventana ", tau, " (d\u00edas)")),
               y = NULL, title = paste("\u2211\u1d40FWF \u2014 D\u00eda", sel_day)) +
          theme_minimal(base_size = 13) +
          theme(axis.text.y = element_text(face = "bold"),
                panel.grid = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5))
      }
    }

    # Función interna: construir ggplot delta heatmap
    build_fwf_delta_plot <- function(delta_list, vars, labels, sel_day) {
      plot_rows <- list()
      for (k in seq_along(vars)) {
        df_d <- delta_list[[vars[k]]]
        row_idx <- which(df_d$day == sel_day)
        if (length(row_idx) == 0) next
        vals <- as.numeric(df_d[row_idx, -1])
        plot_rows[[k]] <- data.frame(
          tau = factor(1:28), variable = labels[k],
          value = vals, stringsAsFactors = FALSE
        )
      }
      if (length(plot_rows) == 0) return(NULL)
      plot_df <- do.call(rbind, plot_rows)
      plot_df$variable <- factor(plot_df$variable, levels = rev(labels))

      if (length(vars) > 1) {
        plot_df$norm_value <- ave(plot_df$value, plot_df$variable, FUN = function(x) {
          mx <- max(abs(x), na.rm = TRUE)
          if (is.na(mx) || mx == 0) rep(0, length(x)) else x / mx
        })
        txt_size <- if (length(vars) > 3) 2.2 else 3
        ggplot(plot_df, aes(x = tau, y = variable, fill = norm_value)) +
          geom_tile(color = "white", linewidth = 0.5) +
          geom_text(aes(label = ifelse(is.na(value), "", round(value, 0))),
                    size = txt_size) +
          scale_fill_gradient2(low = "#6C63FF", mid = "#FFFFFF", high = "#EF476F",
                               midpoint = 0, limits = c(-1, 1),
                               name = "Normalizado", na.value = "#E0E0E0") +
          scale_x_discrete(breaks = seq(2, 28, by = 2)) +
          labs(x = expression(paste("Ventana ", tau, " (d\u00edas)")),
               y = NULL, title = paste("\u0394\u1d40FWF \u2014 D\u00eda", sel_day)) +
          theme_minimal(base_size = 13) +
          theme(axis.text.y = element_text(face = "bold", size = 10),
                panel.grid = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5))
      } else {
        abs_max <- max(abs(plot_df$value), na.rm = TRUE)
        if (is.na(abs_max) || abs_max == 0) abs_max <- 1
        ggplot(plot_df, aes(x = tau, y = variable, fill = value)) +
          geom_tile(color = "white", linewidth = 0.5) +
          geom_text(aes(label = ifelse(is.na(value), "", round(value, 0))),
                    size = 3) +
          scale_fill_gradient2(low = "#6C63FF", mid = "#FFFFFF", high = "#EF476F",
                               midpoint = 0, limits = c(-abs_max, abs_max),
                               name = "\u0394 Carga", na.value = "#E0E0E0") +
          scale_x_discrete(breaks = seq(2, 28, by = 2)) +
          labs(x = expression(paste("Ventana ", tau, " (d\u00edas)")),
               y = NULL, title = paste("\u0394\u1d40FWF \u2014 D\u00eda", sel_day)) +
          theme_minimal(base_size = 13) +
          theme(axis.text.y = element_text(face = "bold"),
                panel.grid = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5))
      }
    }

    output$fwf_sigma_heatmap <- renderPlot({
      req(fwf_sigma_list())
      build_fwf_sigma_plot(fwf_sigma_list(), fwf_load_vars(),
                           fwf_var_labels(), input$fwf_day_select)
    })

    output$fwf_delta_heatmap <- renderPlot({
      req(fwf_delta_list())
      build_fwf_delta_plot(fwf_delta_list(), fwf_load_vars(),
                           fwf_var_labels(), input$fwf_day_select)
    })

    # --- Tab FWF Combinado (estilo artículo) ---

    # Altura: 56 filas (28 Σ + 28 Δ) → fija, no depende de n_days
    output$fwf_combined_heatmap_ui <- renderUI({
      n_vars <- length(fwf_load_vars())
      # Ancho proporcional a variables, alto fijo para 56 filas
      w <- paste0(max(400, n_vars * 120 + 100), "px")
      plotOutput(session$ns("fwf_combined_heatmap"),
                 height = "900px", width = "100%")
    })

    # Build plot (nueva firma: pasa todas las variables a la vez)
    build_current_combined_plot <- function() {
      req(fwf_sigma_list(), fwf_delta_list())
      build_fwf_combined_plot(
        fwf_sigma_list(), fwf_delta_list(),
        fwf_load_vars(), fwf_var_labels(),
        input$fwf_day_select
      )
    }

    output$fwf_combined_heatmap <- renderPlot({
      build_current_combined_plot()
    })

    output$fwf_table_title <- renderText({
      n_vars <- length(fwf_load_vars())
      paste0("Features FWF \u2014 ", day_label(input$fwf_day_select),
             " (", n_vars, " variable", ifelse(n_vars > 1, "s", ""), ")")
    })

    output$fwf_table <- renderDT({
      req(fwf_sigma_list(), fwf_delta_list())

      sel_day <- input$fwf_day_select
      vars <- fwf_load_vars()
      labels <- fwf_var_labels()
      sigma_list <- fwf_sigma_list()
      delta_list <- fwf_delta_list()

      tbl <- data.frame(tau = 1:28)

      for (k in seq_along(vars)) {
        df_s <- sigma_list[[vars[k]]]
        df_d <- delta_list[[vars[k]]]
        row_idx <- which(df_s$day == sel_day)
        if (length(row_idx) == 0) next
        s_vals <- round(as.numeric(df_s[row_idx, -1]), 1)
        d_vals <- round(as.numeric(df_d[row_idx, -1]), 1)
        lbl <- labels[k]
        tbl[[paste0("\u2211 ", lbl)]] <- s_vals
        tbl[[paste0("\u0394 ", lbl)]] <- d_vals
      }

      datatable(
        tbl,
        options = list(
          pageLength = 28,
          scrollX = TRUE,
          dom = "t",
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"),
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE
      ) |> formatStyle(names(tbl), fontSize = "0.85em")
    })

    # Descarga heatmap sigma como PNG
    output$download_fwf_sigma_png <- downloadHandler(
      filename = function() {
        paste0("FWF_sigma_dia_", input$fwf_day_select, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(fwf_sigma_list())
        p <- build_fwf_sigma_plot(fwf_sigma_list(), fwf_load_vars(),
                                  fwf_var_labels(), input$fwf_day_select)
        if (is.null(p)) return()
        n_vars <- length(fwf_load_vars())
        h <- max(3, n_vars * 0.9 + 2)
        ggsave(file, p, width = 14, height = h, dpi = 300, bg = "white")
      }
    )

    # Descarga heatmap delta como PNG
    output$download_fwf_delta_png <- downloadHandler(
      filename = function() {
        paste0("FWF_delta_dia_", input$fwf_day_select, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(fwf_delta_list())
        p <- build_fwf_delta_plot(fwf_delta_list(), fwf_load_vars(),
                                  fwf_var_labels(), input$fwf_day_select)
        if (is.null(p)) return()
        n_vars <- length(fwf_load_vars())
        h <- max(3, n_vars * 0.9 + 2)
        ggsave(file, p, width = 14, height = h, dpi = 300, bg = "white")
      }
    )

    # Descarga heatmap combinado como PNG
    output$download_fwf_combined_png <- downloadHandler(
      filename = function() {
        paste0("FWF_combinado_dia_", input$fwf_day_select, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- build_current_combined_plot()
        if (is.null(p)) return()
        n_vars <- length(fwf_load_vars())
        w <- max(8, n_vars * 2 + 3)
        ggsave(file, p, width = w, height = 14, dpi = 300, bg = "white")
      }
    )

    # Descarga tabla features como XLSX
    output$download_fwf_xlsx <- downloadHandler(
      filename = function() {
        paste0("FWF_features_dia_", input$fwf_day_select, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(fwf_sigma_list(), fwf_delta_list())

        sel_day <- input$fwf_day_select
        vars <- fwf_load_vars()
        labels <- fwf_var_labels()
        sigma_list <- fwf_sigma_list()
        delta_list <- fwf_delta_list()

        tbl <- data.frame(tau_dias = 1:28)

        for (k in seq_along(vars)) {
          df_s <- sigma_list[[vars[k]]]
          df_d <- delta_list[[vars[k]]]
          row_idx <- which(df_s$day == sel_day)
          if (length(row_idx) == 0) next
          s_vals <- round(as.numeric(df_s[row_idx, -1]), 2)
          d_vals <- round(as.numeric(df_d[row_idx, -1]), 2)
          lbl <- gsub("[^A-Za-z0-9_]", "_", labels[k])
          tbl[[paste0("Sigma_", lbl)]] <- s_vals
          tbl[[paste0("Delta_", lbl)]] <- d_vals
        }

        writexl::write_xlsx(tbl, file)
      }
    )
  })
}
