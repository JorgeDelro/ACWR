# ============================================================
# Módulo: Análisis ACWR
# ============================================================

acwrUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Par\u00e1metros de c\u00e1lculo",
      width = 350,
      checkboxGroupInput(
        ns("methods"), "M\u00e9todos de c\u00e1lculo:",
        choices = c("Coupled (Rolling Average)" = "coupled",
                    "Uncoupled (Rolling Average)" = "uncoupled",
                    "EWMA" = "ewma"),
        selected = c("coupled", "ewma")
      ),
      hr(),
      tags$div(
        style = "padding: 10px; border-radius: 8px; border-left: 4px solid var(--bs-info); background: var(--bs-tertiary-bg); font-size: 0.85em;",
        icon("info-circle"),
        " Ventana aguda: ", tags$strong("7 d\u00edas"),
        ". Ventana cr\u00f3nica: ", tags$strong("28 d\u00edas"),
        " (21 d\u00edas previos en el m\u00e9todo desacoplado).",
        " Son las ventanas del paquete ACWR: los c\u00e1lculos de la app",
        " son los del paquete."
      ),
      hr(),
      checkboxInput(ns("show_risk_zones"), "Mostrar zonas de riesgo", value = TRUE),
      conditionalPanel(
        condition = paste0("input['", ns("show_risk_zones"), "']"),
        tags$div(
          style = "margin-top: 6px; padding: 10px; border-radius: 8px; background: var(--bs-tertiary-bg); border-left: 4px solid var(--bs-warning);",
          tags$p(class = "mb-2", style = "font-size: 0.85em; font-weight: 600;", "Umbrales de las zonas de riesgo:"),
          tags$div(
            class = "d-flex gap-2",
            tags$div(style = "flex: 1;", numericInput(ns("threshold_low"), label = NULL, value = 0.8, min = 0.1, max = 2.0, step = 0.05)),
            tags$div(style = "flex: 1;", numericInput(ns("threshold_mid"), label = NULL, value = 1.3, min = 0.5, max = 2.5, step = 0.05)),
            tags$div(style = "flex: 1;", numericInput(ns("threshold_high"), label = NULL, value = 1.5, min = 0.8, max = 3.0, step = 0.05))
          ),
          tags$div(
            style = "font-size: 0.78em; color: var(--bs-secondary); line-height: 1.5;",
            tags$span(style = "color: #922B21;", HTML("&#9632;")), " < 0.8 Infraentrenado", tags$br(),
            tags$span(style = "color: #1E8449;", HTML("&#9632;")), " 0.8\u20131.3 Sweet spot", tags$br(),
            tags$span(style = "color: #7D6608;", HTML("&#9632;")), " 1.3\u20131.5 Precauci\u00f3n", tags$br(),
            tags$span(style = "color: #922B21;", HTML("&#9632;")), " > 1.5 Alto riesgo"
          )
        )
      ),
      hr(),
      tags$strong("Descargar resultados:"),
      tags$div(
        style = "margin-top: 6px;",
        tags$p(class = "text-muted mb-1", style = "font-size: 0.85em;", "Tabla de resultados"),
        tags$div(
          class = "d-flex gap-2 mb-2",
          downloadButton(ns("download_csv"), "CSV", class = "btn-outline-primary btn-sm", style = "flex: 1;"),
          downloadButton(ns("download_xlsx"), "XLSX", class = "btn-outline-success btn-sm", style = "flex: 1;")
        ),
        tags$p(class = "text-muted mb-1 mt-2", style = "font-size: 0.85em;", "Gr\u00e1ficos (PNG)"),
        tags$div(
          class = "d-flex flex-column gap-2",
          downloadButton(ns("download_plot"), "ACWR y Carga diaria", class = "btn-outline-secondary btn-sm"),
          downloadButton(ns("download_loads_plot"), "Cargas Aguda y Cr\u00f3nica", class = "btn-outline-secondary btn-sm")
        ),
        tags$p(class = "text-muted mb-1 mt-2", style = "font-size: 0.85em;", "C\u00e1lculo detallado"),
        downloadButton(ns("download_detail_xlsx"), "XLSX Detallado", class = "btn-outline-success btn-sm w-100")
      )
    ),

    # Panel principal con sub-pestañas
    navset_card_tab(
      id = ns("acwr_tabs"),
      title = "Resultados",

      nav_panel(
        title = "ACWR y Carga diaria",
        icon = icon("chart-area"),
        card_body(
          plotlyOutput(ns("acwr_plot"), height = "600px")
        )
      ),

      nav_panel(
        title = "Cargas Aguda y Cr\u00f3nica",
        icon = icon("chart-line"),
        card_body(
          plotlyOutput(ns("loads_plot"), height = "600px")
        )
      ),

      nav_panel(
        title = "Tabla de resultados",
        icon = icon("table"),
        card_body(
          DTOutput(ns("results_table"))
        )
      ),

      nav_panel(
        title = "C\u00e1lculo Detallado",
        icon = icon("calculator"),
        card_body(
          tags$div(
            class = "mb-3",
            radioButtons(
              ns("detail_method"), "M\u00e9todo:",
              choices = c("Acoplado (Coupled)" = "coupled",
                          "Desacoplado (Uncoupled)" = "uncoupled",
                          "EWMA" = "ewma"),
              selected = "coupled",
              inline = TRUE
            )
          ),
          uiOutput(ns("detail_formula_header")),
          DTOutput(ns("detail_table"))
        )
      ),

      nav_panel(
        title = "Teor\u00eda",
        icon = icon("book"),
        card_body(
          withMathJax(),
          includeHTML("www/theory_acwr.html")
        )
      )
    )
  )
}

acwrServer <- function(id, current_data, plot_theme) {
  moduleServer(id, function(input, output, session) {

    # --- Serie diaria y cálculos del paquete ACWR ---
    package_results <- reactive({
      req(current_data())
      acwr_daily(daily_series(current_data()))
    })

    # --- Aviso: historial insuficiente ---
    observe({
      res <- package_results()
      if (nrow(res) < 28) {
        showNotification(
          paste0("Hay ", nrow(res), " d\u00edas de datos: con menos de 28 ",
                 "d\u00edas el ACWR de los m\u00e9todos Coupled y Uncoupled ",
                 "es NA."),
          type = "warning", duration = 5
        )
      }
    })

    # --- Resultados (valores sin redondear; se redondean al mostrarlos) ---
    acwr_results <- reactive({
      req(length(input$methods) > 0)
      res <- package_results()

      prefix <- c(coupled = "RAC", uncoupled = "RAU", ewma = "EWMA")
      first <- prefix[input$methods[1]]

      result <- data.frame(
        Day     = res$day,
        Load    = res$load,
        Acute   = res[[paste0(first, "_acute")]],
        Chronic = res[[paste0(first, "_chronic")]]
      )

      if ("coupled" %in% input$methods)   result$ACWR_COUPLED   <- res$RAC_ACWR
      if ("uncoupled" %in% input$methods) result$ACWR_UNCOUPLED <- res$RAU_ACWR
      if ("ewma" %in% input$methods)      result$ACWR_EWMA      <- res$EWMA_ACWR

      result
    })

    # --- Gráfico principal ---
    output$acwr_plot <- renderPlotly({
      req(acwr_results())
      th <- plot_theme()

      df <- acwr_results()

      acwr_cols <- grep("^ACWR_", names(df), value = TRUE)
      all_acwr <- unlist(df[acwr_cols], use.names = FALSE)
      all_acwr <- all_acwr[!is.na(all_acwr)]

      if (length(all_acwr) == 0) {
        return(plotly_empty() %>% layout(title = "No hay datos suficientes para calcular el ACWR"))
      }

      max_acwr <- max(c(all_acwr, 1.6), na.rm = TRUE) * 1.05

      method_colors <- c(
        ACWR_COUPLED   = "#6C63FF",
        ACWR_UNCOUPLED = "#FF9F43",
        ACWR_EWMA      = "#EF476F"
      )
      method_labels <- c(
        ACWR_COUPLED   = "Coupled (RA)",
        ACWR_UNCOUPLED = "Uncoupled (RA)",
        ACWR_EWMA      = "EWMA"
      )

      plotly_font <- list(color = th$text_color)

      p <- plot_ly(df, x = ~Day) %>%
        add_bars(y = ~Load, name = "Carga diaria",
                 marker = list(color = th$bar_fill,
                               line = list(color = th$bar_border, width = 1)),
                 opacity = 0.85)

      shapes <- list()
      if (input$show_risk_zones) {
        tl <- input$threshold_low
        tm <- input$threshold_mid
        th_val <- input$threshold_high
        shapes <- list(
          list(type = "rect", xref = "paper", yref = "y2",
               x0 = 0, x1 = 1, y0 = tl, y1 = tm,
               fillcolor = "#57CC99", opacity = 0.12, line = list(width = 0)),
          list(type = "rect", xref = "paper", yref = "y2",
               x0 = 0, x1 = 1, y0 = tm, y1 = th_val,
               fillcolor = "#FFD166", opacity = 0.15, line = list(width = 0)),
          list(type = "rect", xref = "paper", yref = "y2",
               x0 = 0, x1 = 1, y0 = th_val, y1 = max_acwr,
               fillcolor = "#EF476F", opacity = 0.10, line = list(width = 0))
        )
      }

      for (col in acwr_cols) {
        lbl <- method_labels[col]
        clr <- method_colors[col]
        p <- p %>%
          add_trace(y = df[[col]], name = lbl, type = "scatter",
                    mode = "lines+markers",
                    line = list(color = clr, width = 2.5),
                    marker = list(color = clr, size = 5),
                    yaxis = "y2",
                    connectgaps = FALSE)
      }

      p %>% layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.08,
                      font = plotly_font),
        shapes = shapes,
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(title = "D\u00eda", tickfont = plotly_font, titlefont = plotly_font,
                     gridcolor = th$grid_color),
        yaxis = list(title = "Carga diaria (UA)", tickfont = plotly_font,
                     titlefont = plotly_font, gridcolor = th$grid_color,
                     rangemode = "tozero"),
        yaxis2 = list(
          title = list(text = "ACWR", standoff = 15),
          overlaying = "y",
          side = "right",
          range = c(0, max_acwr),
          dtick = 0.2,
          showgrid = FALSE,
          tickfont = plotly_font,
          titlefont = plotly_font
        )
      )
    })

    # --- Gráfico cargas aguda/crónica ---
    output$loads_plot <- renderPlotly({
      req(acwr_results())
      th <- plot_theme()

      df <- acwr_results()

      if (all(is.na(df$Acute)) && all(is.na(df$Chronic))) {
        return(plotly_empty() %>% layout(title = "Selecciona un m\u00e9todo para ver las cargas"))
      }

      first_method <- input$methods[1]
      method_titles <- c(coupled = "Coupled (RA)", uncoupled = "Uncoupled (RA)", ewma = "EWMA")
      title_txt <- paste("Cargas Aguda y Cr\u00f3nica -", method_titles[first_method])
      # RAC/RAU: suma semanal equivalente; EWMA: escala de una carga diaria
      y_title <- if (first_method == "ewma") "Carga (UA/d\u00eda)" else "Carga (UA/7 d\u00edas)"

      plot_df <- df[, c("Day", "Acute", "Chronic")]
      pfont <- list(color = th$text_color)

      p <- plot_ly(plot_df, x = ~Day) %>%
        add_lines(y = ~Acute, name = "Carga Aguda",
                  line = list(color = "#EF476F", width = 2.5),
                  connectgaps = FALSE) %>%
        add_lines(y = ~Chronic, name = "Carga Cr\u00f3nica",
                  line = list(color = "#6C63FF", width = 2.5, dash = "dash"),
                  connectgaps = FALSE) %>%
        layout(
          title = list(text = title_txt, font = list(size = 14, color = th$text_color)),
          xaxis = list(title = "D\u00eda", tickfont = pfont, titlefont = pfont,
                       gridcolor = th$grid_color),
          yaxis = list(title = y_title, tickfont = pfont, titlefont = pfont,
                       gridcolor = th$grid_color),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12,
                        font = pfont),
          hovermode = "x unified",
          paper_bgcolor = "transparent",
          plot_bgcolor = "transparent"
        )

      p
    })

    # --- Tabla de resultados ---
    output$results_table <- renderDT({
      req(acwr_results())

      df <- acwr_results()
      acwr_col_names <- grep("^ACWR_", names(df), value = TRUE)

      dt <- datatable(
        df,
        options = list(
          pageLength = 30,
          scrollX = TRUE,
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"),
          dom = "tip"
        ),
        rownames = FALSE
      )

      dt <- dt %>% formatRound(c("Load", "Acute", "Chronic", acwr_col_names), 2)

      if (length(acwr_col_names) > 0) {
        tl <- input$threshold_low
        tm <- input$threshold_mid
        th_val <- input$threshold_high
        dt <- dt %>%
          formatStyle(
            columns = acwr_col_names,
            backgroundColor = styleInterval(
              c(tl, tm, th_val),
              c("#FADBD8", "#D5F5E3", "#FEF9E7", "#FADBD8")
            ),
            color = styleInterval(
              c(tl, tm, th_val),
              c("#922B21", "#1E8449", "#7D6608", "#922B21")
            )
          )
      }

      dt
    })

    # --- Cálculo Detallado ---
    detail_data <- reactive({
      res <- package_results()
      tl <- input$threshold_low
      tm <- input$threshold_mid
      th_val <- input$threshold_high

      switch(input$detail_method,
        "coupled"   = detail_rolling(res, "coupled", tl, tm, th_val),
        "uncoupled" = detail_rolling(res, "uncoupled", tl, tm, th_val),
        "ewma"      = detail_ewma(res, tl, tm, th_val)
      )
    })

    output$detail_formula_header <- renderUI({
      info <- switch(input$detail_method,
        "coupled" = list(
          title = "M\u00e9todo Acoplado (Coupled) \u2014 ACWR::RAC()",
          desc  = paste0("Aguda = suma de los \u00faltimos 7 d\u00edas  |  ",
                         "Cr\u00f3nica = suma de los \u00faltimos 28 d\u00edas / 4 ",
                         "(incluye la semana aguda)  |  ACWR = Aguda / Cr\u00f3nica  |  ",
                         "Los d\u00edas sin sesi\u00f3n cuentan como carga 0")
        ),
        "uncoupled" = list(
          title = "M\u00e9todo Desacoplado (Uncoupled) \u2014 ACWR::RAU()",
          desc  = paste0("Aguda = suma de los \u00faltimos 7 d\u00edas  |  ",
                         "Cr\u00f3nica = suma de los d\u00edas t\u221227 a t\u22127 / 3 ",
                         "(excluye la semana aguda)  |  ACWR = Aguda / Cr\u00f3nica  |  ",
                         "Los d\u00edas sin sesi\u00f3n cuentan como carga 0")
        ),
        "ewma" = list(
          title = "EWMA \u2014 ACWR::EWMA()",
          desc  = paste0("\u03bba = 2/(7+1) = ", round(2 / (7 + 1), 4),
                         "  |  \u03bbc = 2/(28+1) = ", round(2 / (28 + 1), 4),
                         "  |  EWMA(t) = \u03bb\u00b7Carga(t) + (1\u2212\u03bb)\u00b7EWMA(t\u22121)",
                         "  |  D\u00eda 1: inicializaci\u00f3n = Carga")
        )
      )

      tags$div(
        style = "padding: 10px 14px; border-radius: 8px; background: var(--bs-tertiary-bg); border-left: 4px solid var(--bs-primary); margin-bottom: 14px;",
        tags$strong(info$title),
        tags$br(),
        tags$span(style = "font-size: 0.88em; color: var(--bs-secondary);", info$desc)
      )
    })

    output$detail_table <- renderDT({
      req(detail_data())
      df <- detail_data()

      col_labels <- switch(input$detail_method,
        "coupled" = c(
          "D\u00eda", "Carga", "Ventana Acute (d\u00edas)", "Cargas incluidas (Acute)",
          "Suma Acute", "Aguda (RAC_acute)", "Ventana Chronic (d\u00edas)",
          "Suma Chronic", "Cr\u00f3nica = Suma / 4 (RAC_chronic)", "ACWR", "Zona"
        ),
        "uncoupled" = c(
          "D\u00eda", "Carga", "Ventana Acute (d\u00edas)", "Cargas incluidas (Acute)",
          "Suma Acute", "Aguda (RAU_acute)", "Ventana Chronic (excl. Acute)",
          "Cargas incluidas (Chronic)", "Suma Chronic",
          "Cr\u00f3nica = Suma / 3 (RAU_chronic)", "ACWR", "Zona"
        ),
        "ewma" = c(
          "D\u00eda", "Carga",
          "\u03bba \u00d7 Carga", "(1\u2212\u03bba) \u00d7 EWMA_a(t\u22121)",
          "EWMA Acute",
          "\u03bbc \u00d7 Carga", "(1\u2212\u03bbc) \u00d7 EWMA_c(t\u22121)",
          "EWMA Chronic", "ACWR", "Zona"
        )
      )

      dt <- datatable(
        df,
        colnames = col_labels,
        options = list(
          pageLength = 50,
          scrollX = TRUE,
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"),
          dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE
      )

      # Cargas y sumas con 2 decimales; cargas aguda/crónica y ACWR con 4
      num_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))],
                          c("Dia", "Ventana_Acute", "Ventana_Chronic"))
      cols_2 <- intersect(num_cols, c("Carga", "Suma_Acute", "Suma_Chronic"))
      dt <- dt %>% formatRound(cols_2, 2) %>% formatRound(setdiff(num_cols, cols_2), 4)

      dt %>% formatStyle(
        columns = "Zona",
        backgroundColor = styleEqual(
          c("Infraentrenado", "Sweet spot", "Precauci\u00f3n", "Alto riesgo",
            "Sin datos cr\u00f3nicos", "Inicializaci\u00f3n"),
          c("#FADBD8", "#D5F5E3", "#FEF9E7", "#FADBD8", "#F2F3F4", "#F2F3F4")
        ),
        color = styleEqual(
          c("Infraentrenado", "Sweet spot", "Precauci\u00f3n", "Alto riesgo",
            "Sin datos cr\u00f3nicos", "Inicializaci\u00f3n"),
          c("#922B21", "#1E8449", "#7D6608", "#922B21", "#7F8C8D", "#7F8C8D")
        )
      )
    })

    output$download_detail_xlsx <- downloadHandler(
      filename = function() {
        paste0("ACWR_detalle_", input$detail_method, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(detail_data(), file)
      }
    )

    # --- Descargas ---
    download_data <- reactive({
      df <- acwr_results()
      tl <- input$threshold_low
      tm <- input$threshold_mid
      th_val <- input$threshold_high

      acwr_cols <- grep("^ACWR_", names(df), value = TRUE)
      for (col in acwr_cols) {
        zone_col <- sub("^ACWR_", "Zona_", col)
        df[[zone_col]] <- assign_zone(df[[col]], tl, tm, th_val)
      }
      df
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("ACWR_resultados_", Sys.Date(), ".csv"),
      content = function(file) write.csv(download_data(), file, row.names = FALSE)
    )

    output$download_xlsx <- downloadHandler(
      filename = function() paste0("ACWR_resultados_", Sys.Date(), ".xlsx"),
      content = function(file) writexl::write_xlsx(download_data(), file)
    )

    output$download_plot <- downloadHandler(
      filename = function() paste0("ACWR_gr\u00e1fico_", Sys.Date(), ".png"),
      content = function(file) {
        df <- acwr_results()

        acwr_cols <- grep("^ACWR_", names(df), value = TRUE)
        all_acwr <- unlist(df[acwr_cols], use.names = FALSE)
        all_acwr <- all_acwr[!is.na(all_acwr)]

        max_load <- max(df$Load, na.rm = TRUE)
        max_acwr <- max(c(all_acwr, 1.6), na.rm = TRUE) * 1.05
        scale_factor <- max_load / max_acwr

        method_colors <- c(ACWR_COUPLED = "#6C63FF", ACWR_UNCOUPLED = "#FF9F43", ACWR_EWMA = "#EF476F")
        method_labels <- c(ACWR_COUPLED = "Coupled (RA)", ACWR_UNCOUPLED = "Uncoupled (RA)", ACWR_EWMA = "EWMA")

        p <- ggplot(df, aes(x = Day)) +
          geom_col(aes(y = Load), fill = "#B8C5D6", color = "#9BAEC1", width = 0.7, alpha = 0.85)

        if (input$show_risk_zones) {
          tl <- input$threshold_low
          tm <- input$threshold_mid
          th_val <- input$threshold_high
          p <- p +
            annotate("rect", xmin = -Inf, xmax = Inf,
                     ymin = tl * scale_factor, ymax = tm * scale_factor,
                     fill = "#57CC99", alpha = 0.12) +
            annotate("rect", xmin = -Inf, xmax = Inf,
                     ymin = tm * scale_factor, ymax = th_val * scale_factor,
                     fill = "#FFD166", alpha = 0.15) +
            annotate("rect", xmin = -Inf, xmax = Inf,
                     ymin = th_val * scale_factor, ymax = max_acwr * scale_factor,
                     fill = "#EF476F", alpha = 0.10)
        }

        for (col in acwr_cols) {
          if (col %in% names(df)) {
            lbl <- method_labels[col]
            p <- p +
              geom_line(aes(y = .data[[col]] * scale_factor, color = !!lbl),
                        linewidth = 1.1, na.rm = TRUE) +
              geom_point(aes(y = .data[[col]] * scale_factor, color = !!lbl),
                         size = 2, na.rm = TRUE)
          }
        }

        color_vals <- setNames(method_colors[acwr_cols], method_labels[acwr_cols])

        p <- p +
          scale_color_manual(values = color_vals, name = "M\u00e9todo") +
          scale_y_continuous(
            name = "Carga diaria (UA)",
            sec.axis = sec_axis(~ . / scale_factor, name = "ACWR"),
            expand = expansion(mult = c(0, 0.05))
          ) +
          scale_x_continuous(breaks = seq(min(df$Day), max(df$Day), by = max(1, floor(nrow(df) / 20))),
                             name = "D\u00eda") +
          theme_minimal(base_size = 14) +
          theme(legend.position = "top", panel.grid.minor = element_blank()) +
          labs(title = "ACWR - Acute:Chronic Workload Ratio")

        ggsave(file, p, width = 12, height = 6, dpi = 300, bg = "white")
      }
    )

    output$download_loads_plot <- downloadHandler(
      filename = function() paste0("Cargas_aguda_cr\u00f3nica_", Sys.Date(), ".png"),
      content = function(file) {
        df <- acwr_results()

        first_method <- input$methods[1]
        method_titles <- c(coupled = "Coupled (RA)", uncoupled = "Uncoupled (RA)", ewma = "EWMA")
        title_txt <- paste("Cargas Aguda y Cr\u00f3nica -", method_titles[first_method])
      # RAC/RAU: suma semanal equivalente; EWMA: escala de una carga diaria
      y_title <- if (first_method == "ewma") "Carga (UA/d\u00eda)" else "Carga (UA/7 d\u00edas)"

        plot_df <- df[, c("Day", "Acute", "Chronic")]

        p <- ggplot(plot_df, aes(x = Day)) +
          geom_line(aes(y = Acute, color = "Carga Aguda"), linewidth = 1.1, na.rm = TRUE) +
          geom_line(aes(y = Chronic, color = "Carga Cr\u00f3nica"), linewidth = 1.1, linetype = "dashed", na.rm = TRUE) +
          scale_color_manual(values = c("Carga Aguda" = "#EF476F", "Carga Cr\u00f3nica" = "#6C63FF"), name = "") +
          scale_x_continuous(name = "D\u00eda") +
          scale_y_continuous(name = y_title) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "top", panel.grid.minor = element_blank()) +
          labs(title = title_txt)

        ggsave(file, p, width = 12, height = 6, dpi = 300, bg = "white")
      }
    )
  })
}
