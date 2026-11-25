# ============================================================
# Module: data input
# ============================================================

dataInputUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Data",
      width = 350,
      radioButtons(
        ns("data_source"), "Select source:",
        choices = c("Upload file" = "upload",
                    "Preloaded data" = "preloaded"),
        selected = "upload"
      ),
      conditionalPanel(
        condition = paste0("input['", ns("data_source"), "'] == 'upload'"),
        fileInput(ns("file_upload"), "Upload CSV:",
                  accept = c(".csv", ".txt"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("data_source"), "'] == 'preloaded'"),
        selectInput(
          ns("preloaded_choice"), "Dataset:",
          choices = names(preloaded)
        )
      ),
      uiOutput(ns("athlete_ui")),
      uiOutput(ns("calendar_info"))
    ),

    # Main panel with sub-tabs
    navset_card_tab(
      id = ns("data_tabs"),

      nav_panel(
        title = "Preview",
        icon = icon("table"),
        card_body(
          DTOutput(ns("data_preview"))
        )
      ),

      nav_panel(
        title = "Data guide",
        icon = icon("circle-question"),
        card_body(
          fillable = FALSE,
          tags$div(
            style = "max-width: 800px;",

            # --- Intro ---
            tags$p(
              style = "font-size: 0.95em; color: var(--bs-secondary);",
              "The CSV file has one row per training session or match.",
              " Accepted separators: comma (,) or semicolon (;)."
            ),

            # --- ACWR ---
            tags$div(
              style = "padding: 16px; border-radius: 8px; border-left: 4px solid var(--bs-primary); background: var(--bs-tertiary-bg); margin-bottom: 16px;",
              tags$h6(style = "margin-bottom: 8px;", icon("chart-line"), " ACWR (Acute:Chronic Workload Ratio)"),
              tags$p(style = "margin-bottom: 6px; font-size: 0.9em;",
                "Requires ", tags$strong("a time column"), " (date or day) and ",
                tags$strong("a load column"), ". The athlete column is optional:"),
              tags$table(
                class = "table table-sm table-bordered",
                style = "font-size: 0.85em; width: auto;",
                tags$thead(tags$tr(
                  tags$th("Column"), tags$th("Description"), tags$th("Example")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("date")),
                          tags$td("Session date (YYYY-MM-DD or DD/MM/YYYY)"),
                          tags$td("2024-09-02, 02/09/2024...")),
                  tags$tr(tags$td(tags$code("day")),
                          tags$td("Numeric day (if there is no date)"), tags$td("1, 2, 3...")),
                  tags$tr(tags$td(tags$code("load")), tags$td("Load in AU"), tags$td("450, 520, 380...")),
                  tags$tr(tags$td(tags$code("athlete_id"), " (optional)"),
                          tags$td("Athlete identifier"), tags$td("1, 2, Ana..."))
                )
              ),
              tags$pre(
                style = "background: var(--bs-body-bg); padding: 8px; border-radius: 4px; font-size: 0.82em; margin-bottom: 0;",
                "athlete_id,date,load\n1,2024-09-02,450\n1,2024-09-03,520\n1,2024-09-03,180\n1,2024-09-05,380\n2,2024-09-02,410\n..."
              )
            ),

            # --- Calendar ---
            tags$div(
              style = "padding: 16px; border-radius: 8px; border-left: 4px solid var(--bs-info); background: var(--bs-tertiary-bg); margin-bottom: 16px;",
              tags$h6(style = "margin-bottom: 8px;", icon("calendar-days"), " How sessions are placed on the calendar"),
              tags$ul(
                style = "margin-bottom: 0; padding-left: 18px; font-size: 0.9em;",
                tags$li("Calculations use ", tags$strong("calendar days"),
                        ", from the athlete's first to last day."),
                tags$li("Days that are not in the file count as ",
                        tags$strong("rest (load 0)"), "."),
                tags$li("If there are ", tags$strong("several sessions on the same day"),
                        ", their loads are summed."),
                tags$li("The same applies to numeric days: in ",
                        tags$code("1, 2, 4"), ", day 3 is a rest day."),
                tags$li("A session with an empty load is not replaced by 0: the windows",
                        " that include that day are NA (and EWMA from that day on).")
              )
            ),

            # --- FWF ---
            tags$div(
              style = "padding: 16px; border-radius: 8px; border-left: 4px solid var(--bs-success); background: var(--bs-tertiary-bg); margin-bottom: 16px;",
              tags$h6(style = "margin-bottom: 8px;", icon("fingerprint"), " FWF (Footballer Workload Footprint)"),
              tags$p(style = "margin-bottom: 6px; font-size: 0.9em;",
                "Requires the columns above + ",
                tags$strong("additional GPS columns"), " (numeric):"),
              tags$table(
                class = "table table-sm table-bordered",
                style = "font-size: 0.85em; width: auto;",
                tags$thead(tags$tr(
                  tags$th("Column"), tags$th("Description"), tags$th("Unit")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("date"), " or ", tags$code("day")), tags$td("Date or day"), tags$td("-")),
                  tags$tr(tags$td(tags$code("load")), tags$td("Load (sRPE \u00d7 min)"), tags$td("AU")),
                  tags$tr(tags$td(tags$code("sRPE_UA")), tags$td("sRPE"), tags$td("AU")),
                  tags$tr(tags$td(tags$code("distance_m")), tags$td("Total distance"), tags$td("m")),
                  tags$tr(tags$td(tags$code("dist_21_kmh")), tags$td("Distance >21 km/h"), tags$td("m")),
                  tags$tr(tags$td(tags$code("player_load")), tags$td("Player Load"), tags$td("-")),
                  tags$tr(tags$td(tags$code("n_acc_3")), tags$td("Accelerations >3 m/s\u00b2"), tags$td("n")),
                  tags$tr(tags$td(tags$code("dist_hmld")), tags$td("HMLD"), tags$td("m"))
                )
              ),
              tags$pre(
                style = "background: var(--bs-body-bg); padding: 8px; border-radius: 4px; font-size: 0.82em; margin-bottom: 4px;",
                "day,load,sRPE_UA,distance_m,dist_21_kmh,player_load,n_acc_3,dist_hmld\n1,450,450,5500,180,550,25,900\n2,520,520,6200,210,610,28,1050\n3,0,0,0,0,0,0,0\n..."
              ),
              tags$p(
                style = "font-size: 0.82em; margin-bottom: 0;",
                class = "text-muted",
                icon("info-circle"),
                " At least 56 days (28 + 28) are recommended so that the \u03a3 and \u0394 matrices are complete.",
                " If you only have the date (or ", tags$code("day"), ") and ", tags$code("load"),
                ", the FWF works with a single variable."
              )
            ),

            # --- General notes ---
            tags$div(
              style = "padding: 12px; border-radius: 8px; background: var(--bs-tertiary-bg); font-size: 0.85em;",
              tags$h6(style = "margin-bottom: 8px;", icon("lightbulb"), " General notes"),
              tags$ul(
                style = "margin-bottom: 0; padding-left: 18px;",
                tags$li("Column names are ", tags$strong("not case-sensitive"),
                        ". The Spanish names ", tags$code("fecha"), ", ", tags$code("dia"),
                        ", ", tags$code("carga"), " and ", tags$code("atleta"),
                        " are also accepted."),
                tags$li("If the file has no recognised columns",
                        ", the first two are used as ", tags$code("day"), " and ",
                        tags$code("load"), "."),
                tags$li("Rows without a date (or day) are removed."),
                tags$li("With an athlete column, a selector appears in the side panel."),
                tags$li("Additional numeric columns are detected automatically for the FWF."),
                tags$li("The preloaded data of athletes 7 and 8 include 6 GPS variables.",
                        " The \"Team with rest days\" dataset has real dates, rest",
                        " days and double sessions.")
              )
            )
          )
        )
      )
    )
  )
}

dataInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- File or dataset -> sessions (all athletes) ---
    all_data <- reactive({
      if (input$data_source == "preloaded") {
        req(input$preloaded_choice)
        raw <- preloaded[[input$preloaded_choice]]
      } else {
        req(input$file_upload)
        raw <- tryCatch({
          d <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
          if (ncol(d) < 2) {
            d <- read.csv2(input$file_upload$datapath, stringsAsFactors = FALSE)
          }
          d
        }, error = function(e) {
          showNotification("The file could not be read. Check its format.", type = "error")
          NULL
        })
        req(raw)
      }

      df <- tryCatch(prepare_input(raw), error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
        NULL
      })
      req(df)

      if (attr(df, "dropped") > 0) {
        showNotification(
          paste0(attr(df, "dropped"), " row(s) without a date (or day) were removed."),
          type = "warning", duration = 6
        )
      }
      df
    })

    # --- Athlete selector (only with an athlete column) ---
    output$athlete_ui <- renderUI({
      df <- all_data()
      if (!attr(df, "has_id")) return(NULL)
      ids <- unique(df$ID)
      selectInput(session$ns("athlete"), "Athlete:", choices = ids,
                  selected = ids[1])
    })

    # --- Sessions of the selected athlete ---
    current_data <- reactive({
      df <- all_data()
      if (attr(df, "has_id")) {
        req(input$athlete %in% df$ID)
        keep <- df$ID == input$athlete
      } else {
        keep <- rep(TRUE, nrow(df))
      }
      out <- df[keep, setdiff(names(df), "ID"), drop = FALSE]
      rownames(out) <- NULL
      attr(out, "time") <- attr(df, "time")
      out
    })

    # --- Information: rest days and same-day sessions ---
    output$calendar_info <- renderUI({
      s <- calendar_summary(current_data())
      lines <- list(
        tags$strong(paste0(s$sessions, " sessions over ", s$span,
                           " calendar days.")),
        tags$br(),
        if (s$rest_days > 0) tagList(
          paste0(s$rest_days, " day(s) without a record between the first and ",
                 "last day count as rest (load 0)."), tags$br()),
        if (s$multi_days > 0) tagList(
          paste0(s$multi_days, " day(s) with several sessions: their loads are summed."),
          tags$br()),
        if (s$missing_load > 0) tagList(
          paste0(s$missing_load, " session(s) without a load: the windows that ",
                 "include them are NA."), tags$br())
      )
      tags$div(
        style = "margin-top: 6px; padding: 10px; border-radius: 8px; border-left: 4px solid var(--bs-info); background: var(--bs-tertiary-bg); font-size: 0.85em;",
        icon("calendar-days"), " ", lines
      )
    })

    output$data_preview <- renderDT({
      df <- current_data()
      time <- attr(df, "time")
      show <- df
      if (identical(time, "date")) {
        show$date <- format(show$date)
      } else {
        show$date <- date_to_day(show$date)
      }
      names(show)[names(show) == "date"] <- if (identical(time, "date")) "Date" else "Day"
      names(show)[names(show) == "load"] <- "Load (AU)"
      datatable(
        show,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "tip"
        ),
        rownames = FALSE
      )
    })

    # Return the reactive for the other modules
    current_data
  })
}
