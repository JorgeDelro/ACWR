#' @title Training load dataframe with rest days
#'
#' @description A dataframe with the training load of 3 subjects over 8 weeks,
#'   with rest days. Subject 1 trains 4 days per week; subject 2 trains 5 days
#'   per week with a double session on Wednesdays and does not train in week 5;
#'   subject 3 trains 3 days per week and 6 days in the last week. Created with
#'   simulated data (see `data-raw/training_load_rest.R` in the source code).
#'
#' @docType data
#'
#' @usage data("training_load_rest", package = "ACWR")
#'
#' @format A data frame with 101 rows (one per session) and 5 variables:
#' \describe{
#'   \item{ID}{ID of the subjects}
#'   \item{Week}{training week}
#'   \item{Day}{calendar day since the start (1 = Monday 3 January 2022)}
#'   \item{TL}{training load (arbitrary units)}
#'   \item{Training_Date}{training date (Date)}
#' }
"training_load_rest"
