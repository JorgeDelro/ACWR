#' Footballer Workload Footprint: cumulative and delta variations
#'
#' Descriptive features of the Footballer Workload Footprint (FWF) of
#' Matas-Bustos et al. (2025), computed on a daily calendar for one or several
#' athletes and one or several load variables.
#'
#' Each athlete's sessions are first turned into a daily series per variable,
#' from their first to their last date: sessions on the same day are summed
#' and days without a session count as 0. Then, for each calendar day
#' \eqn{t} and each window \eqn{\tau}:
#' \itemize{
#' \item `fwf_cumulative()` (cumulative variations, Eqs 9-10 of the article):
#'   the sum of the daily values over days \eqn{t - \tau + 1} to \eqn{t},
#'   \eqn{x(t) + x(t-1) + \dots + x(t-\tau+1)}. NA until \eqn{\tau} days
#'   (day \eqn{t} included) exist.
#' \item `fwf_delta()` (temporary variations, Eqs 17-18 of the article):
#'   the change of that sum from the previous day, which simplifies to
#'   \eqn{x(t) - x(t - \tau)}. NA until day \eqn{t - \tau} exists, i.e. until
#'   \eqn{\tau} days of history before day \eqn{t} exist.
#' }
#' Values are not rounded. With `windows = 7` and `windows = 28`,
#' `fwf_cumulative()` gives the acute and (x 4) chronic loads of [RAC()].
#'
#' @param db a data frame with one row per session.
#' @param ID name of the column with the ID of the subjects.
#' @param vars name(s) of the numeric column(s) with the load variables
#'        (e.g. total distance, high-speed running).
#' @param training_dates name of the column with the training dates (Date or
#'        POSIXct). Rows do not need to be sorted.
#' @param windows window sizes \eqn{\tau}, in calendar days (positive
#'        integers). Default `1:28`.
#'
#' @return a tibble in long format with one row per athlete, calendar day
#'         (rest days included), variable and window, and the columns `ID`,
#'         `date`, `variable`, `window` and `value`.
#'
#' @details A missing value (NA) makes every window that contains its day NA,
#'          and a warning is given.
#'
#' @section Package decisions:
#' The following choices are made by this package; they are not taken from
#' the cited source:
#' \itemize{
#' \item Windows are calendar days. The article defines \eqn{\tau} in
#'   \emph{events} (training sessions and matches): its \eqn{x(e_{j-1})} is
#'   the previous session, wherever it falls. Here \eqn{x(t-1)} is the
#'   previous calendar day, and a day without a session counts as 0, as in
#'   [RAC()], [RAU()] and [EWMA()]. With rest days, both definitions give
#'   different values; for `fwf_delta()`, \eqn{x(t - \tau)} is often a rest
#'   day, so the value is then just \eqn{x(t)}.
#' \item There are no partial windows: values are NA until enough history
#'   exists (see above).
#' }
#'
#' @section Interpretation:
#' These functions are descriptive feature engineering. They summarise the
#' recent load history in many windows at once; they are not a validated
#' injury-prediction tool. The article tested them in a single team over one
#' season, and its results should not be generalised to other athletes.
#'
#' @references Matas-Bustos JB, Mora-Garcia AM, de Hoyo Lora M, Nieto-Alarcon A,
#'   Gonzalez-Fernandez FT (2025).
#'   Advanced feature engineering in Acute:Chronic Workload Ratio (ACWR)
#'   calculation for injury forecasting in elite soccer. PLoS One, 20(7),
#'   e0327960. \doi{10.1371/journal.pone.0327960}
#'
#' @seealso [plot_fwf()] to draw one athlete and one day as a heatmap.
#'
#' @export
#'
#' @examples
#' data("training_load", package = "ACWR")
#'
#' # Cumulative variations, windows of 1 to 28 days
#' fwf_sum <- fwf_cumulative(db = training_load,
#'                           ID = "ID",
#'                           vars = "TL",
#'                           training_dates = "Training_Date")
#'
#' # Delta variations for a few windows
#' fwf_dif <- fwf_delta(db = training_load,
#'                      ID = "ID",
#'                      vars = "TL",
#'                      training_dates = "Training_Date",
#'                      windows = c(1, 7, 14, 28))
#'
#' # windows = 7 compares each day with the same weekday of the previous
#' # week: Monday 8 January 2024 vs Monday 1 January 2024
#' two_weeks <- data.frame(ID = 1,
#'                         TL = c(300, 200, 450),
#'                         Training_Date = as.Date(c("2024-01-01",
#'                                                   "2024-01-03",
#'                                                   "2024-01-08")))
#' d7 <- fwf_delta(two_weeks, "ID", "TL", "Training_Date", windows = 7)
#' d7[d7$date == as.Date("2024-01-08"), ] # 450 - 300 = 150
#'
fwf_cumulative <- function(db,
                           ID,
                           vars,
                           training_dates,
                           windows = 1:28) {

  fwf_compute(db, ID, vars, training_dates, windows, "cumulative")

} # end fwf_cumulative function

#' @rdname fwf_cumulative
#' @export
fwf_delta <- function(db,
                      ID,
                      vars,
                      training_dates,
                      windows = 1:28) {

  fwf_compute(db, ID, vars, training_dates, windows, "delta")

} # end fwf_delta function

#' Rolling sum over the last tau days
#'
#' @param load daily values.
#' @param tau window size in days.
#' @return one value per day; NA until tau days exist.
#' @noRd
rolling_sum <- function(load, tau) {
  vapply(seq_along(load), function(t) {
    if (t < tau) return(NA_real_)
    sum(load[(t - tau + 1):t])
  }, numeric(1))
}

#' Difference with the value tau days before
#'
#' @param load daily values.
#' @param tau lag in days.
#' @return one value per day; NA until day t - tau exists.
#' @noRd
lag_difference <- function(load, tau) {
  n <- length(load)
  out <- rep(NA_real_, n)
  if (n > tau) {
    t <- (tau + 1):n
    out[t] <- load[t] - load[t - tau]
  }
  out
}

#' Check the `windows` argument
#' @noRd
check_windows <- function(windows) {
  if (!is.numeric(windows) || length(windows) == 0 || anyNA(windows) ||
      any(windows < 1) || any(windows != round(windows))) {
    stop("`windows` must be positive whole numbers (days), e.g. 1:28.",
         call. = FALSE)
  }
  sort(unique(as.integer(windows)))
}

#' Shared work of fwf_cumulative() and fwf_delta()
#' @noRd
fwf_compute <- function(db, ID, vars, training_dates, windows, type) {

  ## Checks
  if (missing(db) || is.null(db) || !is.data.frame(db)) {
    stop("you must provide `db` as a data frame", call. = FALSE)
  }
  if (missing(ID) || is.null(ID)) {
    stop("you must provide the column name of ID variable", call. = FALSE)
  }
  if (missing(vars) || is.null(vars) || length(vars) == 0) {
    stop("you must provide the column name(s) of the load variable(s)",
         call. = FALSE)
  }
  if (missing(training_dates) || is.null(training_dates)) {
    stop("you must provide the column name of training dates variable",
         call. = FALSE)
  }
  columns <- c(ID, vars, training_dates)
  if (!is.character(columns)) {
    stop("column names must be given as character strings", call. = FALSE)
  }
  if (length(ID) != 1 || length(training_dates) != 1) {
    stop("`ID` and `training_dates` must be a single column name each",
         call. = FALSE)
  }
  vars <- unique(vars)
  absent <- setdiff(columns, names(db))
  if (length(absent) > 0) {
    stop("column(s) not found in `db`: ",
         paste0("\"", absent, "\"", collapse = ", "), call. = FALSE)
  }
  not_numeric <- vars[!vapply(db[vars], is.numeric, logical(1))]
  if (length(not_numeric) > 0) {
    stop("load variable(s) must be numeric: ",
         paste0("\"", not_numeric, "\"", collapse = ", "), call. = FALSE)
  }
  if (nrow(db) == 0) {
    stop("`db` has no sessions.", call. = FALSE)
  }
  if (anyNA(db[[ID]])) {
    stop("the ID column has missing values", call. = FALSE)
  }
  windows <- check_windows(windows)
  day <- as_calendar_days(db[[training_dates]], nrow(db))

  fun <- if (type == "cumulative") rolling_sum else lag_difference
  ids <- unique(db[[ID]])
  out <- vector("list", length(ids))

  # Loop over the subjects
  for (i in seq_along(ids)) {

    rows <- which(db[[ID]] == ids[i])
    first_day <- min(day[rows])
    n_days <- max(day[rows]) - first_day + 1L

    # One block of n_days x length(windows) values per variable
    values <- unlist(lapply(vars, function(v) {
      load <- daily_series(as.double(db[[v]][rows]), day[rows])$load
      unlist(lapply(windows, function(tau) fun(load, tau)))
    }))

    # Long format, sorted by date, variable and window
    grid <- expand.grid(d = seq_len(n_days), w = seq_along(windows),
                        v = seq_along(vars))
    ord <- order(grid$d, grid$v, grid$w)
    out[[i]] <- data.frame(
      ID = rep(ids[i], length(ord)),
      date = as.Date(first_day + grid$d[ord] - 1L, origin = "1970-01-01"),
      variable = vars[grid$v[ord]],
      window = windows[grid$w[ord]],
      value = values[ord],
      stringsAsFactors = FALSE
    )

  } # end loop over individuals

  res <- tibble::as_tibble(do.call(rbind, out))

  n_missing <- vapply(db[vars], function(x) sum(is.na(x)), numeric(1))
  if (any(n_missing > 0)) {
    warning(sum(n_missing), " session(s) have a missing value (",
            paste0(names(n_missing)[n_missing > 0], ": ",
                   n_missing[n_missing > 0], collapse = "; "),
            "). Every window containing those days is NA. Impute the ",
            "missing value(s) before calling this function.", call. = FALSE)
  }

  attr(res, "fwf_type") <- type
  res

} # end fwf_compute function

#' Heatmap of the Footballer Workload Footprint
#'
#' Draws the FWF of one athlete on one day as a heatmap, with the load
#' variables on the x-axis and the windows on the y-axis, as in Figs 5 and 6
#' of Matas-Bustos et al. (2025).
#'
#' @param fwf output of [fwf_cumulative()] or [fwf_delta()].
#' @param ID the athlete to draw. Can be omitted if `fwf` has one athlete.
#' @param date the day to draw (Date, or a character string such as
#'        `"2024-02-15"`).
#' @param standardise if `FALSE` (default), tiles show the values in the
#'        units of each variable. If `TRUE`, each variable and window is
#'        expressed as a z-score over all the athlete's days, so variables in
#'        different units can be compared on one colour scale. The mean and
#'        standard deviation use the athlete's whole history, including days
#'        after `date`, so the z-scores are descriptive only: they are not
#'        what could have been known on that day.
#' @param title plot title. By default, it says which FWF matrix is shown,
#'        the athlete and the day.
#'
#' @return a ggplot2 object.
#'
#' @details Warm colours are high values and cool colours low values. For
#'          [fwf_delta()] (and with `standardise = TRUE`) the scale is
#'          centred at 0. Grey tiles are NA (not enough history). The plot is
#'          descriptive; see [fwf_cumulative()] for its limitations.
#'          It needs the \pkg{ggplot2} package.
#'
#' @references Matas-Bustos JB, Mora-Garcia AM, de Hoyo Lora M, Nieto-Alarcon A,
#'   Gonzalez-Fernandez FT (2025).
#'   Advanced feature engineering in Acute:Chronic Workload Ratio (ACWR)
#'   calculation for injury forecasting in elite soccer. PLoS One, 20(7),
#'   e0327960. \doi{10.1371/journal.pone.0327960}
#'
#' @export
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data("training_load", package = "ACWR")
#'
#' fwf_sum <- fwf_cumulative(db = training_load,
#'                           ID = "ID",
#'                           vars = "TL",
#'                           training_dates = "Training_Date")
#'
#' plot_fwf(fwf_sum, ID = 1, date = max(training_load$Training_Date))
#'
plot_fwf <- function(fwf,
                     ID = NULL,
                     date,
                     standardise = FALSE,
                     title = NULL) {

  ## Checks
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_fwf() needs the ggplot2 package. Install it with ",
         "install.packages(\"ggplot2\").", call. = FALSE)
  }
  needed <- c("ID", "date", "variable", "window", "value")
  if (!is.data.frame(fwf) || !all(needed %in% names(fwf))) {
    stop("`fwf` must be the output of fwf_cumulative() or fwf_delta()",
         call. = FALSE)
  }
  if (!is.logical(standardise) || length(standardise) != 1 ||
      is.na(standardise)) {
    stop("`standardise` must be TRUE or FALSE.", call. = FALSE)
  }
  ids <- unique(fwf$ID)
  if (is.null(ID)) {
    if (length(ids) != 1) {
      stop("`fwf` has ", length(ids), " athletes; choose one with `ID`.",
           call. = FALSE)
    }
    ID <- ids
  }
  if (length(ID) != 1 || !ID %in% ids) {
    stop("`ID` must be one of the athletes in `fwf`.", call. = FALSE)
  }
  if (missing(date) || length(date) != 1) {
    stop("you must provide one `date`", call. = FALSE)
  }
  date <- tryCatch(as.Date(date), error = function(e) NA)
  athlete <- fwf[fwf$ID == ID, , drop = FALSE]
  if (is.na(date) || !date %in% athlete$date) {
    stop("`date` must be a day between the first and the last session ",
         "of the athlete (", min(athlete$date), " to ", max(athlete$date),
         ").", call. = FALSE)
  }

  type <- attr(fwf, "fwf_type")
  if (is.null(type)) type <- "unknown"

  # z-score of each variable and window over all the athlete's days
  if (standardise) {
    key <- paste(athlete$variable, athlete$window)
    athlete$value <- stats::ave(athlete$value, key, FUN = function(x) {
      s <- stats::sd(x, na.rm = TRUE)
      if (is.na(s) || s == 0) return(rep(0, length(x)) * x)
      (x - mean(x, na.rm = TRUE)) / s
    })
  }

  plot_df <- athlete[athlete$date == date, , drop = FALSE]
  plot_df$variable <- factor(plot_df$variable,
                             levels = unique(fwf$variable))
  # Window 1 at the top
  plot_df$window <- factor(plot_df$window,
                           levels = rev(sort(unique(fwf$window))))

  if (is.null(title)) {
    matrix_name <- switch(type,
                          cumulative = "FWF cumulative variations",
                          delta = "FWF delta variations",
                          "FWF")
    title <- paste0(matrix_name, " - ID ", ID, " - ", format(date))
  }
  fill_name <- if (standardise) "z-score" else "value"

  p <- ggplot2::ggplot(plot_df,
                       ggplot2::aes(x = .data$variable, y = .data$window,
                                    fill = .data$value)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::labs(x = "Variable", y = "Window (days)", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  if (standardise || type == "delta") {
    p + ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7",
                                      high = "#B2182B", midpoint = 0,
                                      na.value = "grey80", name = fill_name)
  } else {
    p + ggplot2::scale_fill_gradient(low = "#F7F7F7", high = "#B2182B",
                                     na.value = "grey80", name = fill_name)
  }

} # end plot_fwf function

# `.data` is the ggplot2 data pronoun used in plot_fwf() (ggplot2 is in
# Suggests, so it is not imported)
utils::globalVariables(".data")
