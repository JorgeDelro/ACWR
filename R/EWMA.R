
#' Exponentially Weighted Moving Average
#'
#' Calculates the acute chronic workload ratio with exponentially weighted
#' moving averages (Williams et al., 2017).
#'
#' Sessions are first turned into a daily series from the first to the last
#' date: sessions on the same day are summed and days without a session count
#' as a load of 0. Then, for every calendar day \eqn{t} with daily load
#' \eqn{w_t}:
#' \deqn{E_t = w_t \lambda + (1 - \lambda) E_{t-1}, \quad \lambda = 2 / (N + 1)}
#' with \eqn{N = 7} (acute) and \eqn{N = 28} (chronic), and \eqn{E_1 = w_1}.
#' Acute and chronic loads are on the scale of a daily load. Each session gets
#' the value of its day. Values are not rounded.
#'
#' @param TL training load of each session (numeric, >= 0).
#' @param training_dates date of each session (Date or POSIXct). Sessions do
#'        not need to be sorted. If `NULL` (default), sessions are assumed to
#'        be on consecutive days, in the order given, and a message is shown.
#'
#' @return {A list with one value per session, in the input order:
#' \itemize{
#' \item EWMA_acute: EWMA - acute training load.
#' \item EWMA_chronic: EWMA - chronic training load.
#' \item EWMA_ACWR: EWMA - Acute-Chronic Workload Ratio (NA when the chronic
#'       load is 0).
#' }}
#'
#' @details EWMA is recursive: a missing load (NA) makes that day and every
#'          later day NA. A warning is given; impute missing loads before
#'          calling the function.
#'
#' @references Williams S, West S, Cross MJ, Stokes KA (2017). Better way to
#'   determine the acute:chronic workload ratio? British Journal of Sports
#'   Medicine, 51(3), 209-210. \doi{10.1136/bjsports-2016-096589}
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
#' result_EWMA <- EWMA(TL = training_load_1$TL,
#'                     training_dates = training_load_1$Training_Date)
#'
EWMA <- function(TL,
                 training_dates = NULL) {

  TL <- check_TL(TL)
  if (is.null(training_dates)) {
    message("`training_dates` not supplied: assuming one session per day ",
            "on consecutive days, in the order given.")
    day <- seq_along(TL)
  } else {
    day <- as_calendar_days(training_dates, length(TL))
  }

  res <- session_ACWR(TL, day, "EWMA")
  warn_missing_TL(list(EWMA = list(res$missing)), sum(is.na(TL)))

  list(EWMA_acute = res$values$acute,
       EWMA_chronic = res$values$chronic,
       EWMA_ACWR = res$values$ACWR)

} # end EWMA
