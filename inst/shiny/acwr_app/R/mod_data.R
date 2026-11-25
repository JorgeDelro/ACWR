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
      ),
      uiOutput(ns("athlete_ui")),
      uiOutput(ns("calendar_info"))
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
        title = "Guía de datos",
        icon = icon("circle-question"),
        card_body(
          fillable = FALSE,
          tags$div(
            style = "max-width: 800px;",

            # --- Intro ---
            tags$p(
              style = "font-size: 0.95em; color: var(--bs-secondary);",
              "El archivo CSV tiene una fila por sesión de entrenamiento o partido.",
              " Separador aceptado: coma (,) o punto y coma (;)."
            ),

            # --- ACWR ---
            tags$div(
              style = "padding: 16px; border-radius: 8px; border-left: 4px solid var(--bs-primary); background: var(--bs-tertiary-bg); margin-bottom: 16px;",
              tags$h6(style = "margin-bottom: 8px;", icon("chart-line"), " ACWR (Acute:Chronic Workload Ratio)"),
              tags$p(style = "margin-bottom: 6px; font-size: 0.9em;",
                "Requiere ", tags$strong("una columna de tiempo"), " (fecha o día) y ",
                tags$strong("una columna de carga"), ". La columna de atleta es opcional:"),
              tags$table(
                class = "table table-sm table-bordered",
                style = "font-size: 0.85em; width: auto;",
                tags$thead(tags$tr(
                  tags$th("Columna"), tags$th("Descripción"), tags$th("Ejemplo")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("date"), " o ", tags$code("fecha")),
                          tags$td("Fecha de la sesión (AAAA-MM-DD o DD/MM/AAAA)"),
                          tags$td("2024-09-02, 02/09/2024...")),
                  tags$tr(tags$td(tags$code("day")),
                          tags$td("Día numérico (si no hay fecha)"), tags$td("1, 2, 3...")),
                  tags$tr(tags$td(tags$code("load")), tags$td("Carga en UA"), tags$td("450, 520, 380...")),
                  tags$tr(tags$td(tags$code("athlete_id"), " (opcional)"),
                          tags$td("Identificador del atleta"), tags$td("1, 2, Ana..."))
                )
              ),
              tags$pre(
                style = "background: var(--bs-body-bg); padding: 8px; border-radius: 4px; font-size: 0.82em; margin-bottom: 0;",
                "athlete_id,date,load\n1,2024-09-02,450\n1,2024-09-03,520\n1,2024-09-03,180\n1,2024-09-05,380\n2,2024-09-02,410\n..."
              )
            ),

            # --- Calendario ---
            tags$div(
              style = "padding: 16px; border-radius: 8px; border-left: 4px solid var(--bs-info); background: var(--bs-tertiary-bg); margin-bottom: 16px;",
              tags$h6(style = "margin-bottom: 8px;", icon("calendar-days"), " Cómo se colocan las sesiones en el calendario"),
              tags$ul(
                style = "margin-bottom: 0; padding-left: 18px; font-size: 0.9em;",
                tags$li("Los cálculos usan ", tags$strong("días de calendario"),
                        ", desde el primer hasta el último día del atleta."),
                tags$li("Los días que no aparecen en el archivo cuentan como ",
                        tags$strong("descanso (carga 0)"), "."),
                tags$li("Si hay ", tags$strong("varias sesiones el mismo día"),
                        ", sus cargas se suman."),
                tags$li("Con días numéricos pasa lo mismo: en ",
                        tags$code("1, 2, 4"), " el día 3 es descanso."),
                tags$li("Una sesión con la carga vacía no se cambia por 0: las ventanas",
                        " que incluyen ese día dan NA (y el EWMA, desde ese día en adelante).")
              )
            ),

            # --- FWF ---
            tags$div(
              style = "padding: 16px; border-radius: 8px; border-left: 4px solid var(--bs-success); background: var(--bs-tertiary-bg); margin-bottom: 16px;",
              tags$h6(style = "margin-bottom: 8px;", icon("fingerprint"), " FWF (Footballer Workload Footprint)"),
              tags$p(style = "margin-bottom: 6px; font-size: 0.9em;",
                "Requiere las columnas anteriores + ",
                tags$strong("columnas GPS adicionales"), " (numéricas):"),
              tags$table(
                class = "table table-sm table-bordered",
                style = "font-size: 0.85em; width: auto;",
                tags$thead(tags$tr(
                  tags$th("Columna"), tags$th("Descripción"), tags$th("Unidad")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("date"), " o ", tags$code("day")), tags$td("Fecha o día"), tags$td("-")),
                  tags$tr(tags$td(tags$code("load")), tags$td("Carga (sRPE × min)"), tags$td("UA")),
                  tags$tr(tags$td(tags$code("sRPE_UA")), tags$td("sRPE"), tags$td("UA")),
                  tags$tr(tags$td(tags$code("distancia_m")), tags$td("Distancia total"), tags$td("m")),
                  tags$tr(tags$td(tags$code("dist_21_kmh")), tags$td("Distancia >21 km/h"), tags$td("m")),
                  tags$tr(tags$td(tags$code("player_load")), tags$td("Player Load"), tags$td("-")),
                  tags$tr(tags$td(tags$code("n_acc_3")), tags$td("Aceleraciones >3 m/s²"), tags$td("n")),
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
                " Se recomienda un mínimo de 56 días (28 + 28) para que las matrices Σ y Δ estén completas.",
                " Si solo dispones de la fecha (o ", tags$code("day"), ") y ", tags$code("load"),
                ", el FWF funcionará con una única variable."
              )
            ),

            # --- Notas generales ---
            tags$div(
              style = "padding: 12px; border-radius: 8px; background: var(--bs-tertiary-bg); font-size: 0.85em;",
              tags$h6(style = "margin-bottom: 8px;", icon("lightbulb"), " Notas generales"),
              tags$ul(
                style = "margin-bottom: 0; padding-left: 18px;",
                tags$li("Los nombres de columna ", tags$strong("no distinguen mayúsculas/minúsculas"),
                        ". También se aceptan ", tags$code("fecha"), ", ", tags$code("carga"),
                        " y ", tags$code("atleta"), "."),
                tags$li("Si el archivo no tiene columnas reconocidas",
                        ", se usarán las dos primeras como ", tags$code("day"), " y ",
                        tags$code("load"), "."),
                tags$li("Las filas sin fecha (o día) se eliminan."),
                tags$li("Con una columna de atleta aparece un selector en el panel lateral."),
                tags$li("Las columnas numéricas adicionales se detectan automáticamente para FWF."),
                tags$li("Los datos pre-cargados (Atletas 7 y 8) ya incluyen 6 variables GPS.",
                        " El dataset \"Equipo con descansos\" tiene fechas reales, días de",
                        " descanso y sesiones dobles.")
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

    # --- Archivo o dataset -> sesiones (todas los atletas) ---
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
          showNotification("Error al leer el archivo. Verifica el formato.", type = "error")
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
          paste0(attr(df, "dropped"), " fila(s) sin fecha (o día) se han eliminado."),
          type = "warning", duration = 6
        )
      }
      df
    })

    # --- Selector de atleta (solo si hay columna de atleta) ---
    output$athlete_ui <- renderUI({
      df <- all_data()
      if (!attr(df, "has_id")) return(NULL)
      ids <- unique(df$ID)
      selectInput(session$ns("athlete"), "Atleta:", choices = ids,
                  selected = ids[1])
    })

    # --- Sesiones del atleta seleccionado ---
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

    # --- Aviso: días de descanso y sesiones del mismo día ---
    output$calendar_info <- renderUI({
      s <- calendar_summary(current_data())
      lines <- list(
        tags$strong(paste0(s$sessions, " sesiones en ", s$span,
                           " días de calendario.")),
        tags$br(),
        if (s$rest_days > 0) tagList(
          paste0(s$rest_days, " día(s) sin registro entre el primero y el ",
                 "último cuentan como descanso (carga 0)."), tags$br()),
        if (s$multi_days > 0) tagList(
          paste0(s$multi_days, " día(s) con varias sesiones: sus cargas se suman."),
          tags$br()),
        if (s$missing_load > 0) tagList(
          paste0(s$missing_load, " sesión(es) sin carga: las ventanas que las ",
                 "incluyen dan NA."), tags$br())
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
      names(show)[names(show) == "date"] <- if (identical(time, "date")) "Fecha" else "Día"
      names(show)[names(show) == "load"] <- "Carga (UA)"
      datatable(
        show,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          language = DT_LANGUAGE,
          dom = "tip"
        ),
        rownames = FALSE
      )
    })

    # Retornar el reactivo para que otros módulos lo consuman
    current_data
  })
}
