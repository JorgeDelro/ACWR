# ============================================================
# Module: FWF analysis (Footballer Workload Footprint)
# ============================================================

fwfUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "FWF - Settings",
      width = 350,
      sliderInput(ns("fwf_day_select"), "Day for the feature table:",
                  min = 1, max = 28, value = 28, step = 1),
      hr(),
      uiOutput(ns("fwf_var_info")),
      hr(),
      tags$strong("Download results:"),
      tags$div(
        style = "margin-top: 6px;",
        tags$p(class = "text-muted mb-1", style = "font-size: 0.85em;", "Heatmaps (PNG)"),
        tags$div(
          class = "d-flex flex-column gap-2 mb-2",
          downloadButton(ns("download_fwf_sigma_png"), "\u2211\u1d40FWF cumulative", class = "btn-outline-secondary btn-sm"),
          downloadButton(ns("download_fwf_delta_png"), "\u0394\u1d40FWF temporary", class = "btn-outline-secondary btn-sm"),
          downloadButton(ns("download_fwf_combined_png"), "Combined FWF", class = "btn-outline-secondary btn-sm")
        ),
        tags$p(class = "text-muted mb-1 mt-2", style = "font-size: 0.85em;", "Feature table"),
        downloadButton(ns("download_fwf_xlsx"), "Features XLSX", class = "btn-outline-success btn-sm w-100")
      )
    ),

    # Main panel with sub-tabs
    navset_card_tab(
      id = ns("fwf_tabs"),
      title = "FWF",

      nav_panel(
        title = "\u2211\u1d40FWF",
        icon = icon("layer-group"),
        card_body(
          card(
            card_header(tags$span(style = "font-size: 0.95em; font-weight: 600;",
              "\u2211\u1d40FWF \u2014 Cumulative variations")),
            card_body(uiOutput(ns("fwf_sigma_heatmap_ui")))
          )
        )
      ),

      nav_panel(
        title = "\u0394\u1d40FWF",
        icon = icon("arrow-right-arrow-left"),
        card_body(
          card(
            card_header(tags$span(style = "font-size: 0.95em; font-weight: 600;",
              "\u0394\u1d40FWF \u2014 Temporary variations")),
            card_body(uiOutput(ns("fwf_delta_heatmap_ui")))
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
              "\u2211\u1d40FWF + \u0394\u1d40FWF \u2014 Selected day (all variables)")),
            card_body(
              fillable = FALSE,
              tags$p(class = "text-muted", style = "font-size: 0.85em;",
                "Article-style layout: X = GPS variables, Y = \u03c4 (windows), ",
                "normalised values (z-score). Use the slider in the side panel to change the day."),
              uiOutput(ns("fwf_combined_heatmap_ui"))
            )
          )
        )
      ),

      nav_panel(
        title = "Features",
        icon = icon("table"),
        card_body(
          tags$h6(class = "mb-2", textOutput(ns("fwf_table_title"))),
          DTOutput(ns("fwf_table"))
        )
      ),

      nav_panel(
        title = "Theory",
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

    # Detect the available load variables
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

    # Readable variable names
    fwf_var_labels <- reactive({
      vars <- fwf_load_vars()
      labels <- c(
        "sRPE_UA"      = "sRPE (AU)",
        "distance_m"   = "Distance (m)",
        "distancia_m"  = "Distance (m)",
        "dist_21_kmh"  = "Dist >21 km/h (m)",
        "player_load"  = "Player Load",
        "n_acc_3"      = "Acc >3 m/s\u00b2",
        "dist_hmld"    = "HMLD (m)",
        "load"         = "Load (AU)"
      )
      ifelse(vars %in% names(labels), labels[vars], vars)
    })

    # Sigma and delta for ALL variables, with the ACWR package
    # (calendar days: same-day sessions summed, rest = 0)
    fwf_db <- reactive({
      req(current_data())
      df <- current_data()
      db <- df[, fwf_load_vars(), drop = FALSE]
      db$ID <- 1
      db$date <- df$date
      db
    })

    time_unit <- reactive(attr(current_data(), "time"))

    # Day shown -> date (only for the titles of data with dates)
    day_label <- function(day) {
      df <- current_data()
      if (!identical(time_unit(), "date")) return(paste("Day", day))
      paste0("Day ", day, " (", format(min(df$date) + day - 1), ")")
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

    # Dynamic info in the sidebar
    output$fwf_var_info <- renderUI({
      vars <- fwf_load_vars()
      labels <- fwf_var_labels()
      n <- length(vars)
      if (n == 1 && vars[1] == "load") {
        tags$div(
          style = "padding: 10px; border-radius: 8px; border-left: 4px solid var(--bs-info); background: var(--bs-tertiary-bg); font-size: 0.85em;",
          icon("info-circle"),
          " Load variable: AU (arbitrary units). ",
          "The FWF is most informative with several GPS metrics."
        )
      } else {
        tags$div(
          style = "padding: 10px; border-radius: 8px; border-left: 4px solid var(--bs-success); background: var(--bs-tertiary-bg); font-size: 0.85em;",
          icon("check-circle"),
          tags$strong(paste0(n, " load variables detected:")),
          tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
            lapply(labels, function(lb) tags$li(lb))
          ),
          tags$small(class = "text-muted",
            paste0("Total features: ", n * 28, " \u2211\u1d40 + ", n * 28, " \u0394\u1d40 = ", n * 56))
        )
      }
    })

    # Dynamic heatmap height
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

    # Internal function: build the sigma heatmap (ggplot)
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
                              name = "Normalised", na.value = "#E0E0E0",
                              labels = c("Min", "Max"), breaks = c(0, 1)) +
          scale_x_discrete(breaks = seq(2, 28, by = 2)) +
          labs(x = expression(paste("Window ", tau, " (days)")),
               y = NULL, title = paste("\u2211\u1d40FWF \u2014 Day", sel_day)) +
          theme_minimal(base_size = 13) +
          theme(axis.text.y = element_text(face = "bold", size = 10),
                panel.grid = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5))
      } else {
        ggplot(plot_df, aes(x = tau, y = variable, fill = value)) +
          geom_tile(color = "white", linewidth = 0.5) +
          geom_text(aes(label = round(value, 0)), size = 3, color = "white") +
          scale_fill_gradient(low = "#A8A4CE", high = "#2D2780",
                              name = "Load (AU)", na.value = "#E0E0E0") +
          scale_x_discrete(breaks = seq(2, 28, by = 2)) +
          labs(x = expression(paste("Window ", tau, " (days)")),
               y = NULL, title = paste("\u2211\u1d40FWF \u2014 Day", sel_day)) +
          theme_minimal(base_size = 13) +
          theme(axis.text.y = element_text(face = "bold"),
                panel.grid = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5))
      }
    }

    # Internal function: build the delta heatmap (ggplot)
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
                               name = "Normalised", na.value = "#E0E0E0") +
          scale_x_discrete(breaks = seq(2, 28, by = 2)) +
          labs(x = expression(paste("Window ", tau, " (days)")),
               y = NULL, title = paste("\u0394\u1d40FWF \u2014 Day", sel_day)) +
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
                               name = "\u0394 Load", na.value = "#E0E0E0") +
          scale_x_discrete(breaks = seq(2, 28, by = 2)) +
          labs(x = expression(paste("Window ", tau, " (days)")),
               y = NULL, title = paste("\u0394\u1d40FWF \u2014 Day", sel_day)) +
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

    # --- Combined FWF tab (article style) ---

    # Height: 56 rows (28 Σ + 28 Δ), fixed, does not depend on n_days
    output$fwf_combined_heatmap_ui <- renderUI({
      n_vars <- length(fwf_load_vars())
      # Width proportional to the variables, fixed height for 56 rows
      w <- paste0(max(400, n_vars * 120 + 100), "px")
      plotOutput(session$ns("fwf_combined_heatmap"),
                 height = "900px", width = "100%")
    })

    # Build plot (all variables at once)
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
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE
      ) |> formatStyle(names(tbl), fontSize = "0.85em")
    })

    # Download the sigma heatmap as PNG
    output$download_fwf_sigma_png <- downloadHandler(
      filename = function() {
        paste0("FWF_sigma_day_", input$fwf_day_select, "_", Sys.Date(), ".png")
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

    # Download the delta heatmap as PNG
    output$download_fwf_delta_png <- downloadHandler(
      filename = function() {
        paste0("FWF_delta_day_", input$fwf_day_select, "_", Sys.Date(), ".png")
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

    # Download the combined heatmap as PNG
    output$download_fwf_combined_png <- downloadHandler(
      filename = function() {
        paste0("FWF_combined_day_", input$fwf_day_select, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- build_current_combined_plot()
        if (is.null(p)) return()
        n_vars <- length(fwf_load_vars())
        w <- max(8, n_vars * 2 + 3)
        ggsave(file, p, width = w, height = 14, dpi = 300, bg = "white")
      }
    )

    # Download the feature table as XLSX
    output$download_fwf_xlsx <- downloadHandler(
      filename = function() {
        paste0("FWF_features_day_", input$fwf_day_select, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(fwf_sigma_list(), fwf_delta_list())

        sel_day <- input$fwf_day_select
        vars <- fwf_load_vars()
        labels <- fwf_var_labels()
        sigma_list <- fwf_sigma_list()
        delta_list <- fwf_delta_list()

        tbl <- data.frame(tau_days = 1:28)

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
