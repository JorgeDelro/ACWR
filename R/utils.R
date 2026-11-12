
# Internal helpers (not exported) -------------------------------------------

#' Convert training dates to calendar days
#'
#' @param training_dates a Date or POSIXct vector.
#' @param n expected length.
#'
#' @return an integer vector of calendar days (days since 1970-01-01).
#' @noRd
as_calendar_days <- function(training_dates, n) {

  if (!inherits(training_dates, c("Date", "POSIXt"))) {
    stop("`training_dates` must be of class Date or POSIXct, not ",
         class(training_dates)[1], ". Convert it first, e.g. with as.Date().",
         call. = FALSE)
  }
  if (length(training_dates) != n) {
    stop("`training_dates` (length ", length(training_dates),
         ") must have the same length as `TL` (length ", n, ").",
         call. = FALSE)
  }
  if (anyNA(training_dates)) {
    stop("`training_dates` has ", sum(is.na(training_dates)),
         " missing value(s). Every session needs a date.", call. = FALSE)
  }

  # POSIXct: keep the calendar day in the time zone of the object
  if (inherits(training_dates, "POSIXt")) {
    training_dates <- as.Date(format(training_dates, "%Y-%m-%d"))
  }

  as.integer(floor(unclass(training_dates)))
}

#' Check a training load vector
#'
#' @param TL training load.
#'
#' @return `TL` as a double vector.
#' @noRd
check_TL <- function(TL) {

  if (!is.numeric(TL)) {
    stop("`TL` must be numeric, not ", class(TL)[1], ".", call. = FALSE)
  }
  if (length(TL) == 0) {
    stop("`TL` has no sessions.", call. = FALSE)
  }
  if (any(TL < 0, na.rm = TRUE)) {
    stop("`TL` has ", sum(TL < 0, na.rm = TRUE),
         " negative value(s). Training loads must be >= 0.", call. = FALSE)
  }

  as.double(TL)
}

#' Turn one athlete's sessions into a daily series
#'
#' Sessions on the same day are summed, days without a session get a load of
#' 0, and a day is NA only if one of its recorded sessions has a missing load.
#'
#' @param TL training load of each session.
#' @param day calendar day of each session (integer, any order).
#'
#' @return a list with `load` (one value per calendar day, from the first to
#'         the last day) and `index` (position of each session in `load`).
#' @noRd
daily_series <- function(TL, day) {

  first_day <- min(day)
  index <- day - first_day + 1L
  n_days <- max(index)

  load <- numeric(n_days)
  for (i in seq_along(TL)) {
    load[index[i]] <- load[index[i]] + TL[i]
  }

  list(load = load, index = index)
}

#' Weekly-equivalent load over a window of past days
#'
#' For each day t, sums the daily load over days t - lag_start to t - lag_end
#' and expresses it per 7 days (sum * 7 / number of days in the window).
#'
#' @param load daily load.
#' @param lag_start,lag_end window limits in days before t (lag_start >= lag_end).
#' @param partial if FALSE, NA when the window starts before the first day;
#'        if TRUE, use the days available.
#'
#' @return one value per day.
#' @noRd
weekly_load <- function(load, lag_start, lag_end, partial = FALSE) {

  vapply(seq_along(load), function(t) {
    from <- t - lag_start
    to <- t - lag_end
    if (from < 1) {
      if (!partial) return(NA_real_)
      from <- 1
    }
    if (to < from) return(NA_real_)
    sum(load[from:to]) * 7 / (to - from + 1)
  }, numeric(1))
}

#' Ratio of acute to chronic load
#'
#' Undefined (NA) when the chronic load is 0.
#' @noRd
safe_ratio <- function(acute, chronic) {
  ratio <- acute / chronic
  ratio[!is.na(chronic) & chronic == 0] <- NA_real_
  ratio
}

#' Acute, chronic and ACWR on a daily series
#'
#' @param load daily load.
#' @param method "EWMA", "RAC" or "RAU".
#' @param partial passed to weekly_load() (RAC and RAU only).
#'
#' @return a list with acute, chronic and ACWR (one value per day).
#' @noRd
daily_ACWR <- function(load, method, partial = FALSE) {

  if (method == "EWMA") {
    # Williams et al. (2017): lambda = 2 / (N + 1), N = 7 and 28 days
    lambda_acute <- 2 / (7 + 1)
    lambda_chronic <- 2 / (28 + 1)
    acute <- chronic <- numeric(length(load))
    acute[1] <- chronic[1] <- load[1]
    for (t in seq_along(load)[-1]) {
      acute[t] <- load[t] * lambda_acute + (1 - lambda_acute) * acute[t - 1]
      chronic[t] <- load[t] * lambda_chronic + (1 - lambda_chronic) * chronic[t - 1]
    }
  }

  if (method == "RAC") {
    acute <- weekly_load(load, 6, 0, partial)    # days t-6..t
    chronic <- weekly_load(load, 27, 0, partial) # days t-27..t
  }

  if (method == "RAU") {
    acute <- weekly_load(load, 6, 0, partial)    # days t-6..t
    chronic <- weekly_load(load, 27, 7, partial) # days t-27..t-7
  }

  list(acute = acute, chronic = chronic, ACWR = safe_ratio(acute, chronic))
}

#' Compute one method for one athlete, per session
#'
#' @return a list with `values` (acute, chronic, ACWR; one value per input
#'         session, in the input order) and `missing` (information on missing
#'         loads, or NULL).
#' @noRd
session_ACWR <- function(TL, day, method, partial = FALSE) {

  daily <- daily_series(TL, day)
  res <- daily_ACWR(daily$load, method, partial)
  values <- lapply(res, function(x) x[daily$index])

  missing <- NULL
  if (anyNA(daily$load)) {
    # Days that are NA only because of the missing load(s)
    filled <- daily$load
    filled[is.na(filled)] <- 0
    res_filled <- daily_ACWR(filled, method, partial)
    missing <- list(
      sessions = sum(is.na(TL)),
      days_na = sum(is.na(res$ACWR) & !is.na(res_filled$ACWR))
    )
  }

  list(values = values, missing = missing)
}

#' Warn once about missing training loads
#'
#' @param info a named list (one element per method), each a list of
#'        `missing` results from session_ACWR() (one per athlete); NULL
#'        results are ignored.
#' @param sessions number of sessions with a missing load.
#' @noRd
warn_missing_TL <- function(info, sessions) {

  info <- lapply(info, function(x) Filter(Negate(is.null), x))
  info <- info[lengths(info) > 0]
  if (length(info) == 0) return(invisible())

  days <- vapply(names(info), function(m) {
    paste0(m, ": ", sum(vapply(info[[m]], `[[`, numeric(1), "days_na")),
           " day(s)")
  }, character(1))

  msg <- paste0(
    sessions, " session(s) have a missing training load (TL). Every ",
    "window containing those days is NA. Days with NA ACWR because of ",
    "this: ", paste(days, collapse = "; "), ".")
  if ("EWMA" %in% names(info)) {
    msg <- paste0(msg, " EWMA is recursive, so every day after a missing ",
                  "load is NA.")
  }
  warning(msg, " Impute the missing load(s) before calling this function.",
          call. = FALSE)
}

#' Check the `partial` argument
#' @noRd
check_partial <- function(partial) {
  if (!is.logical(partial) || length(partial) != 1 || is.na(partial)) {
    stop("`partial` must be TRUE or FALSE.", call. = FALSE)
  }
}
