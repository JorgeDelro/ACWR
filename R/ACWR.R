
#' Acute Chronic Workload Ratio
#'
#' @param db a data frame
#' @param ID ID of the subjects
#' @param TL training load
#' @param weeks training weeks
#' @param days training days
#' @param training_dates training dates
#' @param ACWR_method method to calculate ACWR
#'
#' @return a data frame with the acute & chronic training load and ACWR calculated
#'         with the selected method/s and added on the left side of the data frame

#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get old working directory
#' oldwd <- getwd()
#'
#' # Set temporary directory
#' setwd(tempdir())
#'
#' # Read dfs
#' data("training_load", package = "ACWR")
#'
#' # Convert to data.frame
#' training_load <- data.frame(training_load)
#'
#' # Calculate ACWR
#' result_ACWR <- ACWR(db = training_load,
#'                  ID = "ID",
#'                  TL = "TL",
#'                  weeks = "Week",
#'                  days = "Day",
#'                  training_dates = "Training_Date",
#'                  ACWR_method = c("EWMA", "RAC", "RAU"))
#'
#' # set user working directory
#' setwd(oldwd)
#' }
#'
ACWR <- function(db,
                 ID,
                 TL,
                 weeks,
                 days,
                 training_dates,
                 ACWR_method = c("EWMA", "RAC", "RAU")) {

  ## Checks
  # NULL
  if(is.null(db)){
    stop("you must provide a dataframe")
  }
  if(is.null(ID)){
    stop("you must provide the column name of ID variable")
  }
  if(is.null(TL)){
    stop("you must provide the column name of training load variable")
  }
  if(is.null(weeks)){
    stop("you must provide the column name of week variable")
  }
  if(is.null(days)){
    stop("you must provide the column name of day variable")
  }
  if(is.null(training_dates)){
    stop("you must provide the column name of training dates variable")
  }

  # Loop over the subjects
  for (i in unique(db[[ID]])) {

    # Create individual dfs
    db_ind <- db[db[[ID]] == i,  c(ID, TL, weeks, days, training_dates) ]

    # Loop over the methods
    for (j in ACWR_method) {

      # EWMA method
      if(j == "EWMA") {

        res_EWMA <- EWMA(TL = db_ind[[TL]])

        db_ind$EWMA_chronic <- res_EWMA$EWMA_chronic
        db_ind$EWMA_acute <- res_EWMA$EWMA_acute
        db_ind$EWMA_ACWR <- res_EWMA$EWMA_ACWR

      }

      # Rolling Average Coupled
      if(j == "RAC") {

        res_RAC <- RAC(TL = db_ind[[TL]],
                       weeks = db_ind[[weeks]],
                       training_dates = db_ind[[training_dates]])

        db_ind$RAC_acute = res_RAC$RAC_acute
        db_ind$RAC_chronic = res_RAC$RAC_chronic
        db_ind$RAC_ACWR = res_RAC$RAC_ACWR

      }

      # Rolling Average Uncoupled
      if(j == "RAU") {

        res_RAU <- RAU(TL = db_ind[[TL]],
                       weeks = db_ind[[weeks]],
                       training_dates = db_ind[[training_dates]])

        db_ind$RAU_acute = res_RAU$RAU_acute
        db_ind$RAU_chronic = res_RAU$RAU_chronic
        db_ind$RAU_ACWR = res_RAU$RAU_ACWR

      }

    } # end loop over methods

    if(i == 1){
      db_final <- db_ind
    } else {
      db_final <- rbind(db_final, db_ind)
    }

  } # end loop over individuals

  return(db_final)

} # end ACWR function
