# ============================================================
# Utilidades de la app (sin dependencias Shiny)
#
# La app NO calcula cargas por su cuenta: todos los valores vienen del
# paquete ACWR (RAC, RAU, EWMA, fwf_cumulative, fwf_delta). Aquí solo se
# preparan los datos y se dan formato a los resultados.
# ============================================================

# Día numérico -> fecha (el día 1 es 2000-01-01; solo sirve para colocar
# las sesiones en el calendario, la app muestra el número de día)
DAY_ORIGIN <- as.Date("2000-01-01")

day_to_date <- function(day) DAY_ORIGIN + (day - 1)

date_to_day <- function(date) as.numeric(date - DAY_ORIGIN) + 1

# Sesiones -> serie diaria del paquete (una fila por día de calendario:
# sesiones del mismo día sumadas, días sin sesión = 0)
daily_series <- function(df) {
  db <- data.frame(ID = 1, load = df$load, date = day_to_date(df$day))
  long <- ACWR::fwf_cumulative(db, ID = "ID", vars = "load",
                               training_dates = "date", windows = 1)
  data.frame(day = date_to_day(long$date), date = long$date,
             load = long$value)
}

# ACWR de cada día de la serie diaria con las funciones del paquete
acwr_daily <- function(daily, partial = FALSE) {
  rac  <- ACWR::RAC(TL = daily$load, training_dates = daily$date,
                    partial = partial)
  rau  <- ACWR::RAU(TL = daily$load, training_dates = daily$date,
                    partial = partial)
  ewma <- ACWR::EWMA(TL = daily$load, training_dates = daily$date)
  data.frame(daily, rac, rau, ewma)
}

# Formato ancho de fwf_cumulative()/fwf_delta() para una variable:
# columna day + una columna por ventana tau
fwf_wide <- function(long, var, prefix) {
  x <- long[long$variable == var, ]
  windows <- sort(unique(x$window))
  days <- sort(unique(x$date))
  out <- data.frame(day = date_to_day(days))
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
                           partial = FALSE) {
  chronic_from_to <- if (method == "coupled") c(27, 0) else c(27, 7)
  acute_col   <- if (method == "coupled") "RAC_acute" else "RAU_acute"
  chronic_col <- if (method == "coupled") "RAC_chronic" else "RAU_chronic"
  acwr_col    <- if (method == "coupled") "RAC_ACWR" else "RAU_ACWR"

  rows <- lapply(seq_len(nrow(res)), function(t) {
    a_idx <- window_days(t, 6, 0, partial)
    c_idx <- window_days(t, chronic_from_to[1], chronic_from_to[2], partial)
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
  # Acoplado: la ventana crónica (28 días) no se lista, como antes
  if (method == "coupled") out$Cargas_Chronic <- NULL
  out
}

detail_ewma <- function(res, t_low, t_mid, t_high) {
  # Williams et al. (2017): lambda = 2 / (N + 1), N = 7 y 28 días
  lambda_a <- 2 / (7 + 1)
  lambda_c <- 2 / (28 + 1)
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
