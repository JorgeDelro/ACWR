# ============================================================
# Módulo: Entrada de datos
# ============================================================

dataInputUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Base de datos",
      width = 350,
      radioButtons(
        ns("data_source"), "Seleccionar fuente:",
        choices = c("Subir archivo" = "upload",
                    "Datos pre-cargados" = "preloaded"),
        selected = "upload"
      ),
      conditionalPanel(
        condition = paste0("input['", ns("data_source"), "'] == 'upload'"),
        fileInput(ns("file_upload"), "Subir CSV:",
                  accept = c(".csv", ".txt"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("data_source"), "'] == 'preloaded'"),
        selectInput(
          ns("preloaded_choice"), "Dataset:",
          choices = names(preloaded)
        )
      )
    ),

    # Panel principal con sub-pestañas
    navset_card_tab(
      id = ns("data_tabs"),

      nav_panel(
        title = "Vista previa",
        icon = icon("table"),
        card_body(
          DTOutput(ns("data_preview"))
        )
      ),

      nav_panel(
        title = "Gu\u00eda de datos",
        icon = icon("circle-question"),
        card_body(
          fillable = FALSE,
          tags$div(
            style = "max-width: 800px;",

            # --- Intro ---
            tags$p(
              style = "font-size: 0.95em; color: var(--bs-secondary);",
              "El formato del archivo CSV depende del an\u00e1lisis que quieras realizar.",
              " Separador aceptado: coma (,) o punto y coma (;)."
            ),

            # --- ACWR ---
            tags$div(
              style = "padding: 16px; border-radius: 8px; border-left: 4px solid var(--bs-primary); background: var(--bs-tertiary-bg); margin-bottom: 16px;",
              tags$h6(style = "margin-bottom: 8px;", icon("chart-line"), " ACWR (Acute:Chronic Workload Ratio)"),
              tags$p(style = "margin-bottom: 6px; font-size: 0.9em;",
                "Requiere ", tags$strong("2 columnas"), ":"),
              tags$table(
                class = "table table-sm table-bordered",
                style = "font-size: 0.85em; width: auto;",
                tags$thead(tags$tr(
                  tags$th("Columna"), tags$th("Descripci\u00f3n"), tags$th("Ejemplo")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("day")), tags$td("D\u00eda (num\u00e9rico)"), tags$td("1, 2, 3...")),
                  tags$tr(tags$td(tags$code("load")), tags$td("Carga en UA"), tags$td("450, 520, 380..."))
                )
              ),
              tags$pre(
                style = "background: var(--bs-body-bg); padding: 8px; border-radius: 4px; font-size: 0.82em; margin-bottom: 0;",
                "day,load\n1,450\n2,520\n3,380\n4,610\n..."
              )
            ),

            # --- FWF ---
            tags$div(
              style = "padding: 16px; border-radius: 8px; border-left: 4px solid var(--bs-success); background: var(--bs-tertiary-bg); margin-bottom: 16px;",
              tags$h6(style = "margin-bottom: 8px;", icon("fingerprint"), " FWF (Footballer Workload Footprint)"),
              tags$p(style = "margin-bottom: 6px; font-size: 0.9em;",
                "Requiere ", tags$code("day"), " + ", tags$code("load"), " + ",
                tags$strong("columnas GPS adicionales"), " (num\u00e9ricas):"),
              tags$table(
                class = "table table-sm table-bordered",
                style = "font-size: 0.85em; width: auto;",
                tags$thead(tags$tr(
                  tags$th("Columna"), tags$th("Descripci\u00f3n"), tags$th("Unidad")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("day")), tags$td("D\u00eda"), tags$td("-")),
                  tags$tr(tags$td(tags$code("load")), tags$td("Carga (sRPE \u00d7 min)"), tags$td("UA")),
                  tags$tr(tags$td(tags$code("sRPE_UA")), tags$td("sRPE"), tags$td("UA")),
                  tags$tr(tags$td(tags$code("distancia_m")), tags$td("Distancia total"), tags$td("m")),
                  tags$tr(tags$td(tags$code("dist_21_kmh")), tags$td("Distancia >21 km/h"), tags$td("m")),
                  tags$tr(tags$td(tags$code("player_load")), tags$td("Player Load"), tags$td("-")),
                  tags$tr(tags$td(tags$code("n_acc_3")), tags$td("Aceleraciones >3 m/s\u00b2"), tags$td("n")),
                  tags$tr(tags$td(tags$code("dist_hmld")), tags$td("HMLD"), tags$td("m"))
                )
              ),
              tags$pre(
                style = "background: var(--bs-body-bg); padding: 8px; border-radius: 4px; font-size: 0.82em; margin-bottom: 4px;",
                "day,load,sRPE_UA,distancia_m,dist_21_kmh,player_load,n_acc_3,dist_hmld\n1,450,450,5500,180,550,25,900\n2,520,520,6200,210,610,28,1050\n3,0,0,0,0,0,0,0\n..."
              ),
              tags$p(
                style = "font-size: 0.82em; margin-bottom: 0;",
                class = "text-muted",
                icon("info-circle"),
                " Se recomienda un m\u00ednimo de 56 d\u00edas (28 + 28) para que las matrices \u03a3 y \u0394 est\u00e9n completas.",
                " Si solo dispones de ", tags$code("day"), " y ", tags$code("load"),
                ", el FWF funcionar\u00e1 con una \u00fanica variable."
              )
            ),

            # --- Notas generales ---
            tags$div(
              style = "padding: 12px; border-radius: 8px; background: var(--bs-tertiary-bg); font-size: 0.85em;",
              tags$h6(style = "margin-bottom: 8px;", icon("lightbulb"), " Notas generales"),
              tags$ul(
                style = "margin-bottom: 0; padding-left: 18px;",
                tags$li("Los nombres de columna ", tags$strong("no distinguen may\u00fasculas/min\u00fasculas"),
                        " para ", tags$code("day"), " y ", tags$code("load"), "."),
                tags$li("Si el archivo no tiene columnas ", tags$code("day/load"),
                        ", se usar\u00e1n las dos primeras columnas autom\u00e1ticamente."),
                tags$li("Las filas con valores vac\u00edos en ", tags$code("day"), " o ",
                        tags$code("load"), " se eliminan autom\u00e1ticamente."),
                tags$li("Las columnas num\u00e9ricas adicionales se detectan autom\u00e1ticamente para FWF."),
                tags$li("Los datos pre-cargados (Atletas 7 y 8) ya incluyen 6 variables GPS.")
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

    current_data <- reactive({
      if (input$data_source == "preloaded") {
        req(input$preloaded_choice)
        df <- preloaded[[input$preloaded_choice]]
      } else {
        req(input$file_upload)

        df <- tryCatch({
          d <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
          if (ncol(d) < 2) {
            d <- read.csv2(input$file_upload$datapath, stringsAsFactors = FALSE)
          }
          d
        }, error = function(e) {
          showNotification("Error al leer el archivo. Verifica el formato.", type = "error")
          return(NULL)
        })

        req(df)

        names(df) <- trimws(names(df))
        # Normalizar solo day/load a minúsculas (preservar nombres GPS originales)
        names_lower <- tolower(names(df))
        idx_day  <- which(names_lower == "day")[1]
        idx_load <- which(names_lower == "load")[1]

        if (!is.na(idx_day) && !is.na(idx_load)) {
          names(df)[idx_day]  <- "day"
          names(df)[idx_load] <- "load"
        } else if (ncol(df) >= 2) {
          names(df)[1:2] <- c("day", "load")
        } else {
          showNotification("El archivo debe tener columnas 'day' y 'load'.", type = "error")
          return(NULL)
        }

        df$day <- as.numeric(df$day)
        df$load <- as.numeric(df$load)
        df <- df[complete.cases(df[, c("day", "load")]), ]

        if (nrow(df) == 0) {
          showNotification("No se encontraron datos validos.", type = "error")
          return(NULL)
        }

        # Conservar columnas numéricas extra (variables GPS para FWF)
        extra_cols <- setdiff(names(df), c("day", "load"))
        keep_extra <- extra_cols[vapply(extra_cols, function(cn) is.numeric(df[[cn]]), logical(1))]
        df <- df[order(df$day), c("day", "load", keep_extra)]
      }

      df
    })

    output$data_preview <- renderDT({
      req(current_data())
      df <- current_data()
      col_names <- names(df)
      display_names <- col_names
      display_names[col_names == "day"] <- "Day"
      display_names[col_names == "load"] <- "Load (AU)"
      names(display_names) <- col_names
      datatable(
        df,
        colnames = display_names,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"),
          dom = "tip"
        ),
        rownames = FALSE
      )
    })

    # Retornar el reactivo para que otros módulos lo consuman
    current_data
  })
}
