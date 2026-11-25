# ============================================================
# Module: ACWR analysis
# ============================================================

# Warning that the risk zones are not validated (threshold box and "Zone"
# column)
ZONE_WARNING <- paste0(
  "Indicative zones: the thresholds (0.8 / 1.3 / 1.5 by default) are not ",
  "validated for predicting injuries. See Theory, section 5."
)

zone_warning_box <- function() {
  tags$div(
    style = "padding: 6px 12px; border-radius: 8px; border-left: 4px solid var(--bs-warning); background: var(--bs-tertiary-bg); font-size: 0.82em; margin-bottom: 8px;",
    icon("triangle-exclamation"), " ", ZONE_WARNING
  )
}

# X axis: dates if the file had them, day number otherwise
x_values <- function(df) {
  if ("Date" %in% names(df)) as.Date(df$Date) else df$Day
}

x_title <- function(df) {
  if ("Date" %in% names(df)) "Date" else "Day"
}

acwrUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Calculation settings",
      width = 350,
      checkboxGroupInput(
        ns("methods"), "Calculation methods:",
        choices = c("Coupled (Rolling Average)" = "coupled",
                    "Uncoupled (Rolling Average)" = "uncoupled",
                    "EWMA" = "ewma"),
        selected = c("coupled", "uncoupled", "ewma")
      ),
      hr(),
      sliderInput(ns("acute_window"), "Acute window (days):",
                  min = 3, max = 14, value = 7, step = 1),
      sliderInput(ns("chronic_window"), "Chronic window (days):",
                  min = 14, max = 56, value = 28, step = 1),
      uiOutput(ns("window_ratio_warning")),
      tags$p(
        class = "text-muted", style = "font-size: 0.8em; margin-top: -6px;",
        icon("info-circle"),
        " 7/28 days is the convention; other windows are for exploring how",
        " the choice changes the ratio."
      ),
      input_switch(ns("partial"), "Partial windows", value = FALSE),
      tags$p(
        class = "text-muted", style = "font-size: 0.8em; margin-top: -6px;",
        "Off (as in the package): the ACWR is NA until there are as many days",
        " of history as the chronic window. On (", tags$code("partial = TRUE"),
        "): the days available are used, rescaled to 7 days; the first",
        " values rely on few days and are unstable. EWMA is not affected."
      ),
      hr(),
      checkboxInput(ns("show_risk_zones"), "Show risk zones", value = TRUE),
      conditionalPanel(
        condition = paste0("input['", ns("show_risk_zones"), "']"),
        tags$div(
          style = "margin-top: 6px; padding: 10px; border-radius: 8px; background: var(--bs-tertiary-bg); border-left: 4px solid var(--bs-warning);",
          tags$p(class = "mb-2", style = "font-size: 0.85em; font-weight: 600;", "Risk zone thresholds:"),
          tags$div(
            class = "d-flex gap-2",
            tags$div(style = "flex: 1;", numericInput(ns("threshold_low"), label = NULL, value = 0.8, min = 0.1, max = 2.0, step = 0.05)),
            tags$div(style = "flex: 1;", numericInput(ns("threshold_mid"), label = NULL, value = 1.3, min = 0.5, max = 2.5, step = 0.05)),
            tags$div(style = "flex: 1;", numericInput(ns("threshold_high"), label = NULL, value = 1.5, min = 0.8, max = 3.0, step = 0.05))
          ),
          tags$div(
            style = "font-size: 0.78em; color: var(--bs-secondary); line-height: 1.5;",
            tags$span(style = "color: #922B21;", HTML("&#9632;")), " < 0.8 Undertrained", tags$br(),
            tags$span(style = "color: #1E8449;", HTML("&#9632;")), " 0.8\u20131.3 Sweet spot", tags$br(),
            tags$span(style = "color: #7D6608;", HTML("&#9632;")), " 1.3\u20131.5 Caution", tags$br(),
            tags$span(style = "color: #922B21;", HTML("&#9632;")), " > 1.5 High risk"
          ),
          tags$div(
            style = "font-size: 0.78em; color: #7D6608; margin-top: 6px;",
            icon("triangle-exclamation"), " ", ZONE_WARNING
          )
        )
      ),
      hr(),
      tags$strong("Download results:"),
      tags$div(
        style = "margin-top: 6px;",
        tags$p(class = "text-muted mb-1", style = "font-size: 0.85em;", "Results table"),
        tags$div(
          class = "d-flex gap-2 mb-2",
          downloadButton(ns("download_csv"), "CSV", class = "btn-outline-primary btn-sm", style = "flex: 1;"),
          downloadButton(ns("download_xlsx"), "XLSX", class = "btn-outline-success btn-sm", style = "flex: 1;")
        ),
        tags$p(class = "text-muted mb-1 mt-2", style = "font-size: 0.85em;", "Plots (PNG)"),
        tags$div(
          class = "d-flex flex-column gap-2",
          downloadButton(ns("download_plot"), "ACWR and daily load", class = "btn-outline-secondary btn-sm"),
          downloadButton(ns("download_loads_plot"), "Acute and chronic loads", class = "btn-outline-secondary btn-sm")
        ),
        tags$p(class = "text-muted mb-1 mt-2", style = "font-size: 0.85em;", "Detailed calculation"),
        downloadButton(ns("download_detail_xlsx"), "Detailed XLSX", class = "btn-outline-success btn-sm w-100")
      )
    ),

    # Main panel with sub-tabs
    navset_card_tab(
      id = ns("acwr_tabs"),
      title = "Results",

      nav_panel(
        title = "ACWR and daily load",
        icon = icon("chart-area"),
        card_body(
          uiOutput(ns("history_hint_plot")),
          plotlyOutput(ns("acwr_plot"), height = "600px")
        )
      ),

      nav_panel(
        title = "Acute and chronic loads",
        icon = icon("chart-line"),
        card_body(
          plotlyOutput(ns("loads_plot"), height = "600px")
        )
      ),

      nav_panel(
        title = "Results table",
        icon = icon("table"),
        card_body(
          uiOutput(ns("history_hint_table")),
          DTOutput(ns("results_table"))
        )
      ),

      nav_panel(
        title = "Detailed calculation",
        icon = icon("calculator"),
        card_body(
          tags$div(
            class = "mb-3",
            radioButtons(
              ns("detail_method"), "Method:",
              choices = c("Coupled" = "coupled",
                          "Uncoupled" = "uncoupled",
                          "EWMA" = "ewma"),
              selected = "coupled",
              inline = TRUE
            )
          ),
          uiOutput(ns("detail_formula_header")),
          zone_warning_box(),
          DTOutput(ns("detail_table"))
        )
      ),

      nav_panel(
        title = "Theory",
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

    # --- Windows: the chronic window must be longer than the acute one ---
    windows_ok <- reactive({
      validate(need(
        input$chronic_window > input$acute_window,
        paste0("The chronic window (", input$chronic_window, " days) must ",
               "be longer than the acute window (", input$acute_window, " days). ",
               "Move the sliders in the side panel.")
      ))
      list(acute = input$acute_window, chronic = input$chronic_window)
    })

    observe({
      if (input$chronic_window <= input$acute_window) {
        showNotification(
          "The chronic window must be longer than the acute window.",
          type = "warning", duration = 5
        )
      }
    })

    # --- Daily series and ACWR package calculations ---
    package_results <- reactive({
      req(current_data())
      w <- windows_ok()
      acwr_daily(daily_series(current_data()), partial = input$partial,
                 acute = w$acute, chronic = w$chronic)
    })

    time_unit <- reactive(attr(current_data(), "time"))

    # --- Hint: not enough history for the rolling-average ratios ---
    history_hint <- reactive({
      res <- package_results()
      cw <- windows_ok()$chronic
      n <- nrow(res)
      if (isTRUE(input$partial) || n > cw) return(NULL)
      when <- if (n < cw) {
        "so they are NA for every day of these data"
      } else {
        paste0("so they only have a value on day ", cw)
      }
      tags$div(
        style = "padding: 6px 12px; border-radius: 8px; border-left: 4px solid var(--bs-info); background: var(--bs-tertiary-bg); font-size: 0.82em; margin-bottom: 8px;",
        icon("circle-info"),
        paste0(" These data cover ", n, " days. The coupled and uncoupled ",
               "ratios (RAC, RAU) need ", cw, " days of history (the chronic ",
               "window), ", when, ". Turn on \"Partial windows\" in the side ",
               "panel to see earlier values.")
      )
    })

    output$history_hint_plot <- renderUI(history_hint())
    output$history_hint_table <- renderUI(history_hint())

    # --- Warning: chronic window shorter than twice the acute window ---
    output$window_ratio_warning <- renderUI({
      aw <- input$acute_window
      cw <- input$chronic_window
      if (cw <= aw || cw >= 2 * aw) return(NULL)
      tags$div(
        style = "padding: 6px 10px; border-radius: 8px; border-left: 4px solid var(--bs-warning); background: var(--bs-tertiary-bg); font-size: 0.8em; margin: -6px 0 10px 0;",
        icon("triangle-exclamation"),
        paste0(" The chronic window (", cw, " days) is shorter than twice the ",
               "acute window (", aw, " days): the chronic load barely differs ",
               "from the acute load (in the uncoupled method it is based on ",
               "only ", cw - aw, " days) and the ratio becomes very noisy.")
      )
    })

    # --- Results (unrounded values; rounded when displayed) ---
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
      result <- add_date_column(result, res$date, time_unit())

      if ("coupled" %in% input$methods)   result$ACWR_COUPLED   <- res$RAC_ACWR
      if ("uncoupled" %in% input$methods) result$ACWR_UNCOUPLED <- res$RAU_ACWR
      if ("ewma" %in% input$methods)      result$ACWR_EWMA      <- res$EWMA_ACWR

      result
    })

    # --- Main plot ---
    output$acwr_plot <- renderPlotly({
      req(acwr_results())
      th <- plot_theme()

      df <- acwr_results()

      acwr_cols <- grep("^ACWR_", names(df), value = TRUE)
      all_acwr <- unlist(df[acwr_cols], use.names = FALSE)
      all_acwr <- all_acwr[!is.na(all_acwr)]

      if (length(all_acwr) == 0) {
        return(plotly_empty() %>% layout(title = "Not enough data to calculate the ACWR"))
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
      df$X <- x_values(df)

      p <- plot_ly(df, x = ~X) %>%
        add_bars(y = ~Load, name = "Daily load",
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
        lbl <- unname(method_labels[col])
        clr <- unname(method_colors[col])
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
        # Right margin for the ACWR axis labels
        margin = list(r = 80),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(title = x_title(df), tickfont = plotly_font, titlefont = plotly_font,
                     gridcolor = th$grid_color),
        yaxis = list(title = "Daily load (AU)", tickfont = plotly_font,
                     titlefont = plotly_font, gridcolor = th$grid_color,
                     rangemode = "tozero"),
        yaxis2 = list(
          title = list(text = "ACWR", standoff = 15),
          overlaying = "y",
          side = "right",
          range = c(0, max_acwr),
          dtick = 0.2,
          tickformat = ".1f",
          automargin = TRUE,
          showgrid = FALSE,
          tickfont = plotly_font,
          titlefont = plotly_font
        )
      )
    })

    # --- Acute and chronic loads plot ---
    output$loads_plot <- renderPlotly({
      req(acwr_results())
      th <- plot_theme()

      df <- acwr_results()

      if (all(is.na(df$Acute)) && all(is.na(df$Chronic))) {
        return(plotly_empty() %>% layout(title = "Select a method to see the loads"))
      }

      first_method <- input$methods[1]
      method_titles <- c(coupled = "Coupled (RA)", uncoupled = "Uncoupled (RA)", ewma = "EWMA")
      title_txt <- paste("Acute and chronic loads -", method_titles[first_method])
      # RAC/RAU: weekly-equivalent sum; EWMA: scale of a daily load
      y_title <- if (first_method == "ewma") "Load (AU/day)" else "Load (AU/7 days)"

      plot_df <- df[, c("Day", "Acute", "Chronic")]
      plot_df$X <- x_values(df)
      pfont <- list(color = th$text_color)

      p <- plot_ly(plot_df, x = ~X) %>%
        add_lines(y = ~Acute, name = "Acute load",
                  line = list(color = "#EF476F", width = 2.5),
                  connectgaps = FALSE) %>%
        add_lines(y = ~Chronic, name = "Chronic load",
                  line = list(color = "#6C63FF", width = 2.5, dash = "dash"),
                  connectgaps = FALSE) %>%
        layout(
          # Title at the top of the figure and legend just above the plot
          # area, with room for both
          title = list(text = title_txt, font = list(size = 14, color = th$text_color),
                       y = 0.98, yanchor = "top"),
          margin = list(t = 90),
          xaxis = list(title = x_title(df), tickfont = pfont, titlefont = pfont,
                       gridcolor = th$grid_color),
          yaxis = list(title = y_title, tickfont = pfont, titlefont = pfont,
                       gridcolor = th$grid_color),
          legend = list(orientation = "h", x = 0.5, xanchor = "center",
                        y = 1.02, yanchor = "bottom", font = pfont),
          hovermode = "x unified",
          paper_bgcolor = "transparent",
          plot_bgcolor = "transparent"
        )

      p
    })

    # --- Results table ---
    output$results_table <- renderDT({
      req(acwr_results())

      df <- acwr_results()
      acwr_col_names <- grep("^ACWR_", names(df), value = TRUE)

      dt <- datatable(
        df,
        options = list(
          pageLength = 30,
          scrollX = TRUE,
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

    # --- Detailed calculation ---
    detail_data <- reactive({
      res <- package_results()
      w <- windows_ok()
      tl <- input$threshold_low
      tm <- input$threshold_mid
      th_val <- input$threshold_high
      partial <- isTRUE(input$partial)

      out <- switch(input$detail_method,
        "coupled"   = detail_rolling(res, "coupled", tl, tm, th_val,
                                     partial, w$acute, w$chronic),
        "uncoupled" = detail_rolling(res, "uncoupled", tl, tm, th_val,
                                     partial, w$acute, w$chronic),
        "ewma"      = detail_ewma(res, tl, tm, th_val, w$acute, w$chronic)
      )
      add_date_column(out, res$date, time_unit())
    })

    output$detail_formula_header <- renderUI({
      w <- windows_ok()
      aw <- w$acute
      cw <- w$chronic
      partial_txt <- if (isTRUE(input$partial) && input$detail_method != "ewma") {
        paste0("  |  Partial windows: at the start, the days available are used ",
               "(sum \u00d7 7 / days)")
      } else ""

      info <- switch(input$detail_method,
        "coupled" = list(
          title = "Coupled method \u2014 ACWR::RAC()",
          desc  = paste0("Acute = sum of the last ", aw, " days \u00d7 7/", aw, "  |  ",
                         "Chronic = sum of the last ", cw, " days \u00d7 7/", cw,
                         " (includes the acute window)  |  ACWR = Acute / Chronic  |  ",
                         "Days without a session count as load 0", partial_txt)
        ),
        "uncoupled" = list(
          title = "Uncoupled method \u2014 ACWR::RAU()",
          desc  = paste0("Acute = sum of the last ", aw, " days \u00d7 7/", aw, "  |  ",
                         "Chronic = sum of days t\u2212", cw - 1, " to t\u2212", aw,
                         " \u00d7 7/", cw - aw,
                         " (excludes the acute window)  |  ACWR = Acute / Chronic  |  ",
                         "Days without a session count as load 0", partial_txt)
        ),
        "ewma" = list(
          title = "EWMA \u2014 ACWR::EWMA()",
          desc  = paste0("\u03bba = 2/(", aw, "+1) = ", round(2 / (aw + 1), 4),
                         "  |  \u03bbc = 2/(", cw, "+1) = ", round(2 / (cw + 1), 4),
                         "  |  EWMA(t) = \u03bb\u00b7Load(t) + (1\u2212\u03bb)\u00b7EWMA(t\u22121)",
                         "  |  Day 1: initialisation = Load")
        )
      )

      ewma_note <- if (input$detail_method == "ewma") {
        tags$div(
          style = "margin-top: 6px; font-size: 0.85em;",
          icon("circle-info"),
          " The first values depend on the initialisation with the day-1 load",
          " (acute EWMA = chronic EWMA = day-1 load). For example, if day 1 is",
          " a rest day (load 0), the chronic load starts at 0 and the day-2",
          " ACWR is very high. Interpret the first weeks with caution."
        )
      }

      tags$div(
        style = "padding: 10px 14px; border-radius: 8px; background: var(--bs-tertiary-bg); border-left: 4px solid var(--bs-primary); margin-bottom: 14px;",
        tags$strong(info$title),
        tags$br(),
        tags$span(style = "font-size: 0.88em; color: var(--bs-secondary);", info$desc),
        ewma_note
      )
    })

    output$detail_table <- renderDT({
      req(detail_data())
      df <- detail_data()
      w <- windows_ok()
      method_fn <- if (input$detail_method == "coupled") "RAC" else "RAU"
      div_txt <- if (input$detail_method == "coupled") {
        paste0("7/", w$chronic)
      } else {
        paste0("7/", w$chronic - w$acute)
      }

      # Label of each column (the Date column only exists with dates)
      labels <- c(
        Day = "Day", Date = "Date", Load = "Load",
        Acute_days = "Acute window (days)",
        Acute_loads = "Loads included (acute)",
        Acute_sum = "Acute sum",
        Acute = paste0("Acute = sum \u00d7 7/", w$acute, " (", method_fn, "_acute)"),
        Chronic_days = if (input$detail_method == "coupled") "Chronic window (days)"
                       else "Chronic window (excl. acute)",
        Chronic_loads = "Loads included (chronic)",
        Chronic_sum = "Chronic sum",
        Chronic = paste0("Chronic = sum \u00d7 ", div_txt, " (", method_fn, "_chronic)"),
        la_x_Load = "\u03bba \u00d7 Load",
        Decay_Acute = "(1\u2212\u03bba) \u00d7 EWMA_a(t\u22121)",
        EWMA_Acute = "EWMA acute",
        lc_x_Load = "\u03bbc \u00d7 Load",
        Decay_Chronic = "(1\u2212\u03bbc) \u00d7 EWMA_c(t\u22121)",
        EWMA_Chronic = "EWMA chronic",
        ACWR = "ACWR",
        Zone = "Zone \u26a0"
      )
      col_labels <- unname(labels[names(df)])

      dt <- datatable(
        df,
        colnames = col_labels,
        options = list(
          pageLength = 50,
          scrollX = TRUE,
          dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE
      )

      # Loads and sums with 2 decimals; acute/chronic loads and ACWR with 4
      num_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))],
                          c("Day", "Acute_days", "Chronic_days"))
      cols_2 <- intersect(num_cols, c("Load", "Acute_sum", "Chronic_sum"))
      dt <- dt %>% formatRound(cols_2, 2) %>% formatRound(setdiff(num_cols, cols_2), 4)

      dt %>% formatStyle(
        columns = "Zone",
        backgroundColor = styleEqual(
          c(ZONES, "No chronic data", "Initialisation"),
          c("#FADBD8", "#D5F5E3", "#FEF9E7", "#FADBD8", "#F2F3F4", "#F2F3F4")
        ),
        color = styleEqual(
          c(ZONES, "No chronic data", "Initialisation"),
          c("#922B21", "#1E8449", "#7D6608", "#922B21", "#7F8C8D", "#7F8C8D")
        )
      )
    })

    output$download_detail_xlsx <- downloadHandler(
      filename = function() {
        paste0("ACWR_detail_", input$detail_method, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(detail_data(), file)
      }
    )

    # --- Downloads ---
    download_data <- reactive({
      df <- acwr_results()
      tl <- input$threshold_low
      tm <- input$threshold_mid
      th_val <- input$threshold_high

      acwr_cols <- grep("^ACWR_", names(df), value = TRUE)
      for (col in acwr_cols) {
        zone_col <- sub("^ACWR_", "Zone_", col)
        df[[zone_col]] <- assign_zone(df[[col]], tl, tm, th_val)
      }
      df
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("ACWR_results_", Sys.Date(), ".csv"),
      content = function(file) write.csv(download_data(), file, row.names = FALSE)
    )

    output$download_xlsx <- downloadHandler(
      filename = function() paste0("ACWR_results_", Sys.Date(), ".xlsx"),
      content = function(file) writexl::write_xlsx(download_data(), file)
    )

    output$download_plot <- downloadHandler(
      filename = function() paste0("ACWR_plot_", Sys.Date(), ".png"),
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

        df$X <- x_values(df)
        x_scale <- if ("Date" %in% names(df)) {
          scale_x_date(name = "Date")
        } else {
          scale_x_continuous(breaks = seq(min(df$Day), max(df$Day), by = max(1, floor(nrow(df) / 20))),
                             name = "Day")
        }

        p <- ggplot(df, aes(x = X)) +
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
          scale_color_manual(values = color_vals, name = "Method") +
          scale_y_continuous(
            name = "Daily load (AU)",
            sec.axis = sec_axis(~ . / scale_factor, name = "ACWR"),
            expand = expansion(mult = c(0, 0.05))
          ) +
          x_scale +
          theme_minimal(base_size = 14) +
          theme(legend.position = "top", panel.grid.minor = element_blank()) +
          labs(title = "ACWR - Acute:Chronic Workload Ratio")

        ggsave(file, p, width = 12, height = 6, dpi = 300, bg = "white")
      }
    )

    output$download_loads_plot <- downloadHandler(
      filename = function() paste0("Acute_chronic_loads_", Sys.Date(), ".png"),
      content = function(file) {
        df <- acwr_results()

        first_method <- input$methods[1]
        method_titles <- c(coupled = "Coupled (RA)", uncoupled = "Uncoupled (RA)", ewma = "EWMA")
        title_txt <- paste("Acute and chronic loads -", method_titles[first_method])
        # RAC/RAU: weekly-equivalent sum; EWMA: scale of a daily load
        y_title <- if (first_method == "ewma") "Load (AU/day)" else "Load (AU/7 days)"

        plot_df <- df[, c("Day", "Acute", "Chronic")]
        plot_df$X <- x_values(df)

        p <- ggplot(plot_df, aes(x = X)) +
          geom_line(aes(y = Acute, color = "Acute load"), linewidth = 1.1, na.rm = TRUE) +
          geom_line(aes(y = Chronic, color = "Chronic load"), linewidth = 1.1, linetype = "dashed", na.rm = TRUE) +
          scale_color_manual(values = c("Acute load" = "#EF476F", "Chronic load" = "#6C63FF"), name = "") +
          (if ("Date" %in% names(df)) scale_x_date(name = "Date")
           else scale_x_continuous(name = "Day")) +
          scale_y_continuous(name = y_title) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "top", panel.grid.minor = element_blank()) +
          labs(title = title_txt)

        ggsave(file, p, width = 12, height = 6, dpi = 300, bg = "white")
      }
    )
  })
}
