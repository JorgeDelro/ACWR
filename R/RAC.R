
#' Rolling Average Coupled
#'
#' Calculates the acute chronic workload ratio with rolling averages, where
#' the acute week is included in the chronic period (coupled ratio).
#'
#' Sessions are first turned into a daily series from the first to the last
#' date: sessions on the same day are summed and days without a session count
#' as a load of 0. Then, for each day \eqn{t}:
#' \itemize{
#' \item Acute load: sum of the daily load over days \eqn{t-6} to \eqn{t}.
#' \item Chronic load: sum of the daily load over days \eqn{t-27} to \eqn{t},
#'       divided by 4 (weekly-equivalent).
#' \item ACWR: acute / chronic.
#' }
#' Each session gets the value of its day. Values are not rounded.
#'
#' @param TL training load of each session (numeric, >= 0).
#' @param weeks deprecated and ignored. Kept for backwards compatibility;
#'        the calculation now uses `training_dates` only.
#' @param training_dates date of each session (Date or POSIXct). Sessions do
#'        not need to be sorted.
#' @param partial if `FALSE` (default), acute values are NA until 7 days of
#'        history exist, and chronic values and ACWR until 28 days exist.
#'        If `TRUE`, the days available are used and expressed per 7 days.
#'
#' @return {A list with one value per session, in the input order:
#' \itemize{
#' \item RAC_acute: RAC - acute training load.
#' \item RAC_chronic: RAC - chronic training load.
#' \item RAC_ACWR: RAC - Acute-Chronic Workload Ratio (NA when the chronic
#'       load is 0).
#' }}
#'
#' @details A missing load (NA) makes every window that contains its day NA,
#'          and a warning is given.
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
#' result_RAC <- RAC(TL = training_load_1$TL,
#'                   training_dates = training_load_1$Training_Date)
#'
RAC <- function(TL,
                weeks = NULL,
                training_dates,
                partial = FALSE) {

  if (missing(training_dates)) {
    stop("you must provide `training_dates`", call. = FALSE)
  }
  check_partial(partial)
  TL <- check_TL(TL)
  day <- as_calendar_days(training_dates, length(TL))

  res <- session_ACWR(TL, day, "RAC", partial)
  warn_missing_TL(list(RAC = list(res$missing)), sum(is.na(TL)))

  list(RAC_acute = res$values$acute,
       RAC_chronic = res$values$chronic,
       RAC_ACWR = res$values$ACWR)

} # end RAC function
