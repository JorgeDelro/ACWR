
#' Rolling Average Uncoupled
#'
#' Calculates the acute chronic workload ratio with rolling averages, where
#' the acute week is excluded from the chronic period (uncoupled ratio).
#'
#' Sessions are first turned into a daily series from the first to the last
#' date: sessions on the same day are summed and days without a session count
#' as a load of 0. Then, for each day \eqn{t}:
#' \itemize{
#' \item Acute load: sum of the daily load over days \eqn{t-6} to \eqn{t}.
#' \item Chronic load: sum of the daily load over days \eqn{t-27} to
#'       \eqn{t-7}, divided by 3 (weekly-equivalent).
#' \item ACWR: acute / chronic.
#' }
#' Each session gets the value of its day. Values are not rounded.
#'
#' @inheritParams RAC
#'
#' @return {A list with one value per session, in the input order:
#' \itemize{
#' \item RAU_acute: RAU - acute training load.
#' \item RAU_chronic: RAU - chronic training load.
#' \item RAU_ACWR: RAU - Acute-Chronic Workload Ratio (NA when the chronic
#'       load is 0).
#' }}
#'
#' @details A missing load (NA) makes every window that contains its day NA,
#'          and a warning is given.
#'
#' @section Package decisions:
#' The following choices are made by this package; they are not taken from
#' the cited sources:
#' \itemize{
#' \item Windows move day by day. The sources define acute and chronic
#'   loads by weeks (the most recent week, and the average of the last 4
#'   weeks or of the 3 preceding weeks). This package interprets them as
#'   rolling windows of 7, 28 and 21 calendar days that end on each day, so
#'   every day has its own value.
#' \item With `partial = TRUE`, a window that starts before the first day
#'   uses only the days available and is rescaled to 7 days (sum x 7 /
#'   number of days available). The sources do not cover incomplete windows.
#' \item The ratio is NA when the chronic load is 0 (e.g. no training in the
#'   chronic window): a ratio to zero load is undefined, and returning NA
#'   avoids infinite values in summaries and plots.
#' }
#'
#' @references Windt J, Gabbett TJ (2019). Is it all for naught? What does
#'   mathematical coupling mean for acute:chronic workload ratios? British
#'   Journal of Sports Medicine, 53(16), 988-990. \doi{10.1136/bjsports-2017-098925}
#'
#' @export
#'
#' @examples
#' data("training_load", package = "ACWR")
#'
#' # Select the first subject
#' training_load_1 <- training_load[training_load[["ID"]] == 1, ]
#'
#' # Calculate ACWR
#' result_RAU <- RAU(TL = training_load_1$TL,
#'                   training_dates = training_load_1$Training_Date)
#'
RAU <- function(TL,
                weeks = NULL,
                training_dates,
                partial = FALSE) {

  if (missing(training_dates)) {
    stop("you must provide `training_dates`", call. = FALSE)
  }
  check_partial(partial)
  TL <- check_TL(TL)
  day <- as_calendar_days(training_dates, length(TL))

  res <- session_ACWR(TL, day, "RAU", partial)
  warn_missing_TL(list(RAU = list(res$missing)), sum(is.na(TL)))

  list(RAU_acute = res$values$acute,
       RAU_chronic = res$values$chronic,
       RAU_ACWR = res$values$ACWR)

} # end RAU function
