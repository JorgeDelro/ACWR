# ============================================================
# Utilidades de la app (sin dependencias Shiny)
#
# La app NO calcula cargas por su cuenta: todos los valores vienen del
# paquete ACWR (RAC, RAU, EWMA, fwf_cumulative, fwf_delta). Aquí solo se
# preparan los datos y se da formato a los resultados.
# ============================================================

# --- Entrada de datos ---

# Día numérico -> fecha (el día 1 es 2000-01-01; solo sirve para colocar
# las sesiones en el calendario, la app muestra el número de día)
DAY_ORIGIN <- as.Date("2000-01-01")

day_to_date <- function(day) DAY_ORIGIN + (day - 1)

date_to_day <- function(date) as.numeric(date - DAY_ORIGIN) + 1

# Nombres de columna reconocidos (sin distinguir mayúsculas/minúsculas)
COL_ID   <- c("athlete_id", "id", "athlete", "atleta", "jugador", "player")
COL_DATE <- c("date", "fecha", "training_date")
COL_DAY  <- c("day", "dia", "día")
COL_LOAD <- c("load", "carga", "tl")

DATE_FORMATS <- c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d")

find_col <- function(df, candidates) {
  idx <- which(tolower(names(df)) %in% candidates)
  if (length(idx) == 0) NA_integer_ else idx[1]
}

parse_dates <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(format(x, "%Y-%m-%d")))
  x <- trimws(as.character(x))
  for (f in DATE_FORMATS) {
    d <- as.Date(x, format = f)
    if (all(is.na(d) == (is.na(x) | x == ""))) return(d)
  }
  stop("No se reconoce el formato de la columna de fechas. Usa AAAA-MM-DD ",
       "(p. ej. 2024-09-02) o DD/MM/AAAA (p. ej. 02/09/2024).", call. = FALSE)
}

# Archivo (o dataset pre-cargado) -> sesiones con columnas ID, date, load
# y las variables numéricas extra. Atributo "time": "date" si el archivo
# tiene fechas, "day" si tiene días numéricos. Los errores se dan con stop()
# en español para mostrarlos en la app.
prepare_input <- function(df) {
  names(df) <- trimws(names(df))
  i_id   <- find_col(df, COL_ID)
  i_date <- find_col(df, COL_DATE)
  i_day  <- find_col(df, COL_DAY)
  i_load <- find_col(df, COL_LOAD)

  # Compatibilidad: sin nombres reconocidos, las dos primeras columnas son
  # día y carga
  if (is.na(i_load) && is.na(i_date) && is.na(i_day)) {
    if (ncol(df) < 2) {
      stop("El archivo debe tener una columna de fecha (o 'day') y una ",
           "columna 'load'.", call. = FALSE)
    }
    i_day <- 1L
    i_load <- 2L
  }
  if (is.na(i_load)) {
    stop("No se encuentra la columna de carga ('load').", call. = FALSE)
  }
  if (is.na(i_date) && is.na(i_day)) {
    stop("No se encuentra una columna de fecha ('date' o 'fecha') ni de ",
         "día ('day').", call. = FALSE)
  }

  if (!is.na(i_date)) {
    time <- "date"
    date <- parse_dates(df[[i_date]])
  } else {
    time <- "day"
    day <- suppressWarnings(as.numeric(df[[i_day]]))
    if (any(!is.na(day) & day != round(day))) {
      stop("La columna 'day' debe contener días enteros (1, 2, 3...).",
           call. = FALSE)
    }
    date <- day_to_date(day)
  }

  load <- suppressWarnings(as.numeric(df[[i_load]]))
  if (any(load < 0, na.rm = TRUE)) {
    stop("La carga no puede ser negativa.", call. = FALSE)
  }
  id <- if (is.na(i_id)) rep("1", nrow(df)) else as.character(df[[i_id]])

  used <- c(i_id, i_date, i_day, i_load)
  extra <- setdiff(seq_along(df), used[!is.na(used)])
  extra <- extra[vapply(df[extra], is.numeric, logical(1))]

  out <- data.frame(ID = id, date = date, load = load,
                    stringsAsFactors = FALSE)
  out <- cbind(out, df[, extra, drop = FALSE])

  dropped <- sum(is.na(out$date) | is.na(out$ID))
  out <- out[!is.na(out$date) & !is.na(out$ID), , drop = FALSE]
  if (nrow(out) == 0) {
    stop("No se encontraron datos válidos.", call. = FALSE)
  }
  out <- out[order(out$ID, out$date), , drop = FALSE]
  rownames(out) <- NULL

  attr(out, "time") <- time
  attr(out, "has_id") <- !is.na(i_id)
  attr(out, "dropped") <- dropped
  out
}

# Resumen del calendario de un atleta (para avisar al usuario)
calendar_summary <- function(df) {
  days <- unique(df$date)
  span <- as.numeric(max(days) - min(days)) + 1
  list(
    sessions = nrow(df),
    days_with_sessions = length(days),
    span = span,
    rest_days = span - length(days),
    multi_days = sum(table(df$date) > 1),
    missing_load = sum(is.na(df$load))
  )
}

# Número de día que se muestra: el del archivo (datos con 'day') o el día
# de calendario desde la primera fecha (datos con fechas)
day_number <- function(date, time, first_date = min(date)) {
  if (identical(time, "date")) as.numeric(date - first_date) + 1
  else date_to_day(date)
}

# --- Cálculos (siempre con el paquete ACWR) ---

# Sesiones -> serie diaria del paquete (una fila por día de calendario:
# sesiones del mismo día sumadas, días sin sesión = 0, NA si falta la carga
# de una sesión)
daily_series <- function(df) {
  db <- data.frame(ID = 1, load = df$load, date = df$date)
  long <- suppressWarnings(
    ACWR::fwf_cumulative(db, ID = "ID", vars = "load",
                         training_dates = "date", windows = 1)
  )
  data.frame(day = day_number(long$date, attr(df, "time")),
             date = long$date, load = long$value)
}

# ACWR de cada día de la serie diaria con las funciones del paquete
acwr_daily <- function(daily, partial = FALSE, acute = 7, chronic = 28) {
  suppressWarnings({
    rac  <- ACWR::RAC(TL = daily$load, training_dates = daily$date,
                      partial = partial, acute = acute, chronic = chronic)
    rau  <- ACWR::RAU(TL = daily$load, training_dates = daily$date,
                      partial = partial, acute = acute, chronic = chronic)
    ewma <- ACWR::EWMA(TL = daily$load, training_dates = daily$date,
                       acute = acute, chronic = chronic)
  })
  data.frame(daily, rac, rau, ewma)
}

# Formato ancho de fwf_cumulative()/fwf_delta() para una variable:
# columna day + una columna por ventana tau
fwf_wide <- function(long, var, prefix, time) {
  x <- long[long$variable == var, ]
  windows <- sort(unique(x$window))
  days <- sort(unique(x$date))
  out <- data.frame(day = day_number(days, time))
  for (w in windows) {
    xw <- x[x$window == w, ]
    out[[paste0(prefix, "_tau_", w)]] <- xw$value[match(days, xw$date)]
  }
  out
}

assign_zone <- function(values, t_low, t_mid, t_high) {
  ifelse(is.na(values), NA_character_,
    ifelse(values < t_low, "Infraentrenado",
      ifelse(values <= t_mid, "Sweet spot",
        ifelse(values <= t_high, "Precaución",
          "Alto riesgo"))))
}

# --- Cálculo detallado (paso a paso) ---
# Las columnas Aguda, Crónica y ACWR son las del paquete; las cargas
# incluidas y sus sumas salen de la misma serie diaria del paquete.

# Días de la ventana [t - from, t - to]; NULL si no hay historial
# suficiente (o los días disponibles con ventanas parciales)
window_days <- function(t, from, to, partial) {
  start <- t - from
  end <- t - to
  if (start < 1) {
    if (!partial) return(NULL)
    start <- 1
  }
  if (end < start) return(NULL)
  start:end
}

format_loads <- function(x) paste0("[", paste(x, collapse = ", "), "]")

detail_rolling <- function(res, method, t_low, t_mid, t_high,
                           partial = FALSE, acute = 7, chronic = 28) {
  chronic_to <- if (method == "coupled") 0 else acute
  acute_col   <- if (method == "coupled") "RAC_acute" else "RAU_acute"
  chronic_col <- if (method == "coupled") "RAC_chronic" else "RAU_chronic"
  acwr_col    <- if (method == "coupled") "RAC_ACWR" else "RAU_ACWR"

  rows <- lapply(seq_len(nrow(res)), function(t) {
    a_idx <- window_days(t, acute - 1, 0, partial)
    c_idx <- window_days(t, chronic - 1, chronic_to, partial)
    acwr  <- res[[acwr_col]][t]
    data.frame(
      Dia             = res$day[t],
      Carga           = res$load[t],
      Ventana_Acute   = if (is.null(a_idx)) NA_integer_ else length(a_idx),
      Cargas_Acute    = if (is.null(a_idx)) "—" else format_loads(res$load[a_idx]),
      Suma_Acute      = if (is.null(a_idx)) NA_real_ else sum(res$load[a_idx]),
      Aguda           = res[[acute_col]][t],
      Ventana_Chronic = if (is.null(c_idx)) NA_integer_ else length(c_idx),
      Cargas_Chronic  = if (is.null(c_idx)) "—" else format_loads(res$load[c_idx]),
      Suma_Chronic    = if (is.null(c_idx)) NA_real_ else sum(res$load[c_idx]),
      Cronica         = res[[chronic_col]][t],
      ACWR            = acwr,
      Zona            = if (is.na(acwr)) "Sin datos crónicos" else
                          assign_zone(acwr, t_low, t_mid, t_high),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  # Acoplado: la ventana crónica completa no se lista, como antes
  if (method == "coupled") out$Cargas_Chronic <- NULL
  out
}

detail_ewma <- function(res, t_low, t_mid, t_high, acute = 7, chronic = 28) {
  # Williams et al. (2017): lambda = 2 / (N + 1)
  lambda_a <- 2 / (acute + 1)
  lambda_c <- 2 / (chronic + 1)
  n <- nrow(res)
  prev_a <- c(NA_real_, res$EWMA_acute[-n])
  prev_c <- c(NA_real_, res$EWMA_chronic[-n])
  first <- seq_len(n) == 1
  zona <- ifelse(is.na(res$EWMA_ACWR), "Sin datos crónicos",
                 assign_zone(res$EWMA_ACWR, t_low, t_mid, t_high))
  zona[first] <- "Inicialización"
  data.frame(
    Dia           = res$day,
    Carga         = res$load,
    la_x_Carga    = ifelse(first, NA_real_, res$load * lambda_a),
    Decay_Acute   = (1 - lambda_a) * prev_a,
    EWMA_Acute    = res$EWMA_acute,
    lc_x_Carga    = ifelse(first, NA_real_, res$load * lambda_c),
    Decay_Chronic = (1 - lambda_c) * prev_c,
    EWMA_Chronic  = res$EWMA_chronic,
    ACWR          = res$EWMA_ACWR,
    Zona          = zona,
    stringsAsFactors = FALSE
  )
}

# Fecha real en las tablas (solo si el archivo tenía fechas)
add_date_column <- function(df, dates, time) {
  if (!identical(time, "date")) return(df)
  cbind(df[1], Fecha = format(dates), df[-1])
}
