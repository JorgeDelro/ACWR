# ============================================================
# ACWR & FWF
# Acute:Chronic Workload Ratio + Footballer Workload Footprint
# Todos los calculos usan el paquete ACWR (ACWR::run_app())
#
# Asignatura: Planificación del Entrenamiento Deportivo
# Profesor: Jorge del Rosario Fernández Santos
# ============================================================

# --- Paquetes necesarios ---
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(writexl)
library(shinyWidgets)
library(ACWR)

# Todos los archivos en R/ se cargan automáticamente por Shiny:
#   utils_app.R, utils_fwf_plot.R, utils_plot_theme.R,
#   data_preloaded.R, mod_data.R, mod_acwr.R, mod_fwf.R

# ============================================================
# UI
# ============================================================

ui <- page_navbar(
  title = tags$span(
    tags$strong("ACWR"),
    style = "font-size: 1.15em;"
  ),
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#6C63FF",
    secondary = "#A8A4CE",
    success = "#57CC99",
    info = "#80B3FF",
    warning = "#FFD166",
    danger = "#EF476F",
    "navbar-bg" = "#6C63FF",
    "navbar-dark-color" = "#FFFFFF",
    "navbar-dark-hover-color" = "#E0DDFF",
    "navbar-dark-active-color" = "#FFFFFF",
    "navbar-dark-brand-color" = "#FFFFFF",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  header = tags$head(tags$style(HTML("
    /* Plotly se adapta al dark mode */
    [data-bs-theme='dark'] .plotly .main-svg { background: transparent !important; }
    [data-bs-theme='dark'] .js-plotly-plot .plotly .main-svg { background: transparent !important; }
    [data-bs-theme='dark'] .card { background-color: var(--bs-card-bg) !important; }
  "))),

  # --- Pestaña: DATOS ---
  nav_panel(
    title = "Datos",
    icon = icon("database"),
    dataInputUI("data")
  ),

  # --- Pestaña: ACWR ---
  nav_panel(
    title = "ACWR",
    icon = icon("chart-line"),
    acwrUI("acwr")
  ),

  # --- Pestaña: FWF ---
  nav_panel(
    title = "FWF",
    icon = icon("fingerprint"),
    fwfUI("fwf")
  ),

  # --- Espaciador y toggle de tema ---
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),

  # --- Footer ---
  footer = tags$footer(
    style = "text-align:center; padding: 15px; font-size: 0.85em; color: var(--bs-secondary); border-top: 1px solid var(--bs-border-color);",
    tags$span("ACWR & FWF"),
    tags$span(" | "),
    tags$span("Planificaci\u00f3n del Entrenamiento Deportivo"),
    tags$span(" | "),
    tags$span("Prof. Jorge del Rosario Fern\u00e1ndez Santos"),
    tags$br(),
    tags$span(
      style = "font-size: 0.9em;",
      "Williams S, et al. 2017 | Matas-Bustos JB, et al. 2025"
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # Tema reactivo compartido (depende del dark mode del navbar)
  plot_theme <- reactive({
    get_plot_theme(isTRUE(input$dark_mode == "dark"))
  })

  # Módulo de datos: retorna current_data (reactive)
  current_data <- dataInputServer("data")

  # Módulos de análisis: consumen current_data y plot_theme
  acwrServer("acwr", current_data, plot_theme)
  fwfServer("fwf", current_data, plot_theme)
}

# ============================================================
# EJECUTAR APP
# ============================================================

shinyApp(ui = ui, server = server)
