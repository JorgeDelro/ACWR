
#' Acute Chronic Workload Ratio
#'
#' Calculates the acute chronic workload ratio for one or several athletes
#' with one or more methods (see [EWMA()], [RAC()] and [RAU()]).
#'
#' Each athlete's sessions are turned into a daily series from their first to
#' their last date: sessions on the same day are summed and days without a
#' session count as a load of 0.
#'
#' @param db a data frame with one row per session.
#' @param ID name of the column with the ID of the subjects.
#' @param TL name of the column with the training load (numeric, >= 0).
#' @param weeks deprecated and ignored in the calculation. If given, the
#'        column is kept in the output.
#' @param days deprecated and ignored in the calculation. If given, the
#'        column is kept in the output (e.g. for [plot_ACWR()]).
#' @param training_dates name of the column with the training dates (Date or
#'        POSIXct). Rows do not need to be sorted.
#' @param ACWR_method method(s) to calculate ACWR: any of "EWMA", "RAC" and
#'        "RAU".
#' @param partial passed to [RAC()] and [RAU()]. If `FALSE` (default), values
#'        are NA until enough history exists; if `TRUE`, the days available
#'        are used.
#'
#' @return a data frame with the ID, TL, weeks, days and training dates
#'         columns, followed by the acute & chronic training load and ACWR of
#'         the selected method/s. Rows are in the same order as in `db`.
#'
#' @details A missing load (NA) makes every window that contains its day NA,
#'          and a single warning is given for the whole data frame.
#'          The ratio is NA when the chronic load is 0 (e.g. no training
#'          in the chronic window): a ratio to zero load is undefined, and
#'          returning NA avoids infinite values in summaries and plots.
#'
#' @export
#'
#' @examples
#' data("training_load", package = "ACWR")
#'
#' # Calculate ACWR
#' result_ACWR <- ACWR(db = training_load,
#'                     ID = "ID",
#'                     TL = "TL",
#'                     training_dates = "Training_Date",
#'                     ACWR_method = c("EWMA", "RAC", "RAU"))
#'
#' # Athletes with rest days
#' data("training_load_rest", package = "ACWR")
#' result_rest <- ACWR(db = training_load_rest,
#'                     ID = "ID",
#'                     TL = "TL",
#'                     training_dates = "Training_Date",
#'                     ACWR_method = c("EWMA", "RAC", "RAU"))
#'
ACWR <- function(db,
                 ID,
                 TL,
                 weeks = NULL,
                 days = NULL,
                 training_dates,
                 ACWR_method = c("EWMA", "RAC", "RAU"),
                 partial = FALSE) {

  ## Checks
  if (missing(db) || is.null(db) || !is.data.frame(db)) {
    stop("you must provide `db` as a data frame", call. = FALSE)
  }
  if (missing(ID) || is.null(ID)) {
    stop("you must provide the column name of ID variable", call. = FALSE)
  }
  if (missing(TL) || is.null(TL)) {
    stop("you must provide the column name of training load variable",
         call. = FALSE)
  }
  if (missing(training_dates) || is.null(training_dates)) {
    stop("you must provide the column name of training dates variable",
         call. = FALSE)
  }
  ACWR_method <- match.arg(ACWR_method, several.ok = TRUE)
  check_partial(partial)

  columns <- c(ID, TL, weeks, days, training_dates)
  if (!is.character(columns)) {
    stop("column names must be given as character strings", call. = FALSE)
  }
  absent <- setdiff(columns, names(db))
  if (length(absent) > 0) {
    stop("column(s) not found in `db`: ",
         paste0("\"", absent, "\"", collapse = ", "), call. = FALSE)
  }
  if (anyNA(db[[ID]])) {
    stop("the ID column has missing values", call. = FALSE)
  }

  load <- check_TL(db[[TL]])
  day <- as_calendar_days(db[[training_dates]], nrow(db))

  db_final <- db[, unique(columns), drop = FALSE]
  missing_info <- list()

  # Loop over the methods
  for (j in ACWR_method) {

    acute <- chronic <- ratio <- rep(NA_real_, nrow(db))
    missing_info[[j]] <- list()

    # Loop over the subjects
    for (i in unique(db[[ID]])) {

      rows <- which(db[[ID]] == i)
      res <- session_ACWR(load[rows], day[rows], j, partial)

      acute[rows] <- res$values$acute
      chronic[rows] <- res$values$chronic
      ratio[rows] <- res$values$ACWR
      missing_info[[j]] <- c(missing_info[[j]], list(res$missing))

    } # end loop over individuals

    if (j == "EWMA") {
      db_final$EWMA_chronic <- chronic
      db_final$EWMA_acute <- acute
      db_final$EWMA_ACWR <- ratio
    } else {
      db_final[[paste0(j, "_acute")]] <- acute
      db_final[[paste0(j, "_chronic")]] <- chronic
      db_final[[paste0(j, "_ACWR")]] <- ratio
    }

  } # end loop over methods

  warn_missing_TL(missing_info, sum(is.na(load)))

  return(db_final)

} # end ACWR function
