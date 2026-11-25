# ============================================================
# App helpers (no Shiny dependencies)
#
# The app does NOT compute loads itself: every value comes from the ACWR
# package (RAC, RAU, EWMA, fwf_cumulative, fwf_delta). These helpers only
# prepare the data and format the results.
# ============================================================

# --- Data input ---

# Numeric day -> date (day 1 is 2000-01-01; only used to place the
# sessions on the calendar, the app shows the day number)
DAY_ORIGIN <- as.Date("2000-01-01")

day_to_date <- function(day) DAY_ORIGIN + (day - 1)

date_to_day <- function(date) as.numeric(date - DAY_ORIGIN) + 1

# Accepted column names (case-insensitive); the Spanish names are aliases
COL_ID   <- c("athlete_id", "id", "athlete", "player", "atleta", "jugador")
COL_DATE <- c("date", "training_date", "fecha")
COL_DAY  <- c("day", "dia", "d\u00eda")
COL_LOAD <- c("load", "tl", "carga")

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
  stop("The date column format is not recognised. Use YYYY-MM-DD ",
       "(e.g. 2024-09-02) or DD/MM/YYYY (e.g. 02/09/2024).", call. = FALSE)
}

# File (or preloaded dataset) -> sessions with columns ID, date, load and
# the extra numeric variables. Attribute "time": "date" if the file has
# dates, "day" if it has numeric days. Errors are given with stop() so that
# the app can show them.
prepare_input <- function(df) {
  names(df) <- trimws(names(df))
  i_id   <- find_col(df, COL_ID)
  i_date <- find_col(df, COL_DATE)
  i_day  <- find_col(df, COL_DAY)
  i_load <- find_col(df, COL_LOAD)

  # Backwards compatibility: without recognised names, the first two
  # columns are day and load
  if (is.na(i_load) && is.na(i_date) && is.na(i_day)) {
    if (ncol(df) < 2) {
      stop("The file must have a date (or 'day') column and a 'load' ",
           "column.", call. = FALSE)
    }
    i_day <- 1L
    i_load <- 2L
  }
  if (is.na(i_load)) {
    stop("The load column ('load') was not found.", call. = FALSE)
  }
  if (is.na(i_date) && is.na(i_day)) {
    stop("No date column ('date') or day column ('day') was found.",
         call. = FALSE)
  }

  if (!is.na(i_date)) {
    time <- "date"
    date <- parse_dates(df[[i_date]])
  } else {
    time <- "day"
    day <- suppressWarnings(as.numeric(df[[i_day]]))
    if (any(!is.na(day) & day != round(day))) {
      stop("The 'day' column must contain whole days (1, 2, 3...).",
           call. = FALSE)
    }
    date <- day_to_date(day)
  }

  load <- suppressWarnings(as.numeric(df[[i_load]]))
  if (any(load < 0, na.rm = TRUE)) {
    stop("The load cannot be negative.", call. = FALSE)
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
    stop("No valid data were found.", call. = FALSE)
  }
  out <- out[order(out$ID, out$date), , drop = FALSE]
  rownames(out) <- NULL

  attr(out, "time") <- time
  attr(out, "has_id") <- !is.na(i_id)
  attr(out, "dropped") <- dropped
  out
}

# Calendar summary of one athlete (to inform the user)
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

# Day number shown: the one in the file (data with 'day') or the calendar
# day since the first date (data with dates)
day_number <- function(date, time, first_date = min(date)) {
  if (identical(time, "date")) as.numeric(date - first_date) + 1
  else date_to_day(date)
}

# --- Calculations (always with the ACWR package) ---

# Sessions -> the package's daily series (one row per calendar day:
# same-day sessions summed, days without a session = 0, NA if a session
# has a missing load)
daily_series <- function(df) {
  db <- data.frame(ID = 1, load = df$load, date = df$date)
  long <- suppressWarnings(
    ACWR::fwf_cumulative(db, ID = "ID", vars = "load",
                         training_dates = "date", windows = 1)
  )
  data.frame(day = day_number(long$date, attr(df, "time")),
             date = long$date, load = long$value)
}

# ACWR for each day of the daily series, with the package functions
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

# Wide format of fwf_cumulative()/fwf_delta() for one variable:
# a day column plus one column per window tau
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

# Risk zones (indicative, not validated)
ZONES <- c("Undertrained", "Sweet spot", "Caution", "High risk")

assign_zone <- function(values, t_low, t_mid, t_high) {
  ifelse(is.na(values), NA_character_,
    ifelse(values < t_low, ZONES[1],
      ifelse(values <= t_mid, ZONES[2],
        ifelse(values <= t_high, ZONES[3],
          ZONES[4]))))
}

# --- Detailed calculation (step by step) ---
# The Acute, Chronic and ACWR columns are the package's values; the loads
# included and their sums come from the same daily series of the package.

# Days of the window [t - from, t - to]; NULL if there is not enough
# history (or the days available with partial windows)
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
      Day           = res$day[t],
      Load          = res$load[t],
      Acute_days    = if (is.null(a_idx)) NA_integer_ else length(a_idx),
      Acute_loads   = if (is.null(a_idx)) "\u2014" else format_loads(res$load[a_idx]),
      Acute_sum     = if (is.null(a_idx)) NA_real_ else sum(res$load[a_idx]),
      Acute         = res[[acute_col]][t],
      Chronic_days  = if (is.null(c_idx)) NA_integer_ else length(c_idx),
      Chronic_loads = if (is.null(c_idx)) "\u2014" else format_loads(res$load[c_idx]),
      Chronic_sum   = if (is.null(c_idx)) NA_real_ else sum(res$load[c_idx]),
      Chronic       = res[[chronic_col]][t],
      ACWR          = acwr,
      Zone          = if (is.na(acwr)) "No chronic data" else
                        assign_zone(acwr, t_low, t_mid, t_high),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  # Coupled: the full chronic window is not listed
  if (method == "coupled") out$Chronic_loads <- NULL
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
  zone <- ifelse(is.na(res$EWMA_ACWR), "No chronic data",
                 assign_zone(res$EWMA_ACWR, t_low, t_mid, t_high))
  zone[first] <- "Initialisation"
  data.frame(
    Day           = res$day,
    Load          = res$load,
    la_x_Load     = ifelse(first, NA_real_, res$load * lambda_a),
    Decay_Acute   = (1 - lambda_a) * prev_a,
    EWMA_Acute    = res$EWMA_acute,
    lc_x_Load     = ifelse(first, NA_real_, res$load * lambda_c),
    Decay_Chronic = (1 - lambda_c) * prev_c,
    EWMA_Chronic  = res$EWMA_chronic,
    ACWR          = res$EWMA_ACWR,
    Zone          = zone,
    stringsAsFactors = FALSE
  )
}

# Real date in the tables (only if the file had dates)
add_date_column <- function(df, dates, time) {
  if (!identical(time, "date")) return(df)
  cbind(df[1], Date = format(dates), df[-1])
}
