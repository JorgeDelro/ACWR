
#' Rolling Average Coupled
#'
#' @param TL training load
#' @param weeks training weeks
#' @param training_dates training dates
#'
#' @return {This function returns the following variables:
#' \itemize{
#' \item RAC_chronic: RAC - chronic training load.
#' \item RAC_acute: RAC - acute training load.
#' \item RAC_ACWR: RAC - Acute-Chronic Workload Ratio.
#' }}
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
#' # Read db
#' data("training_load", package = "ACWR")
#'
#' # Convert to data.frame
#' training_load <- data.frame(training_load)
#'
#' # Select the first subject
#' training_load_1 <- training_load[training_load[["ID"]] == 1,  ]
#'
#' # Calculate ACWR
#' result_RAC <- RAC(TL = training_load_1$TL,
#'                    weeks = training_load_1$Week,
#'                    training_dates = training_load_1$Training_Date)
#'
#' # set user working directory
#' setwd(oldwd)
#' }
#'
RAC <- function(TL,
                weeks,
                training_dates) {

  # Count number of sessions / week
  sessions_week <- as.data.frame(table(weeks))

  # Initialize variables
  RAC_chronic <- c()
  RAC_acute <- c()
  RAC_ACWR <- c()
  # Initialize number of training sessions
  n_sessions_total <- 0

  # Loop over the total days of training
  for (i in unique(weeks)) {

    # First training week: RAC_chronic = RAC_acute
    if(i == 1){

      # loop over number of sessions / week
      for (j in 1:sessions_week$Freq[unique(weeks)[i]]) {

        # First training day: RAC_chronic = TL / RAC_acute = TL
        if(j == 1){
          # Count number of training sessions
          n_sessions_total <- n_sessions_total + 1
          RAC_chronic[n_sessions_total] = TL[n_sessions_total]
          RAC_acute[n_sessions_total] = TL[n_sessions_total]
        }

        # Rest of the week
        else if(j >= 2){
          # Count number of training sessions
          n_sessions_total <- n_sessions_total + 1
          RAC_chronic[n_sessions_total] = (sum(TL[1:n_sessions_total]))/n_sessions_total
          RAC_acute[n_sessions_total] = (sum(TL[1:n_sessions_total]))/n_sessions_total
        }

      }
    } # end first week

    # from second week to end of first month
    else if(i >= 2 && i < 5){

      # loop over number of sessions / week
      for (j in 1:sessions_week$Freq[unique(weeks)[i]]) {

        # Count number of training sessions
        n_sessions_total <- n_sessions_total + 1
        RAC_chronic[n_sessions_total] = (sum(TL[1:n_sessions_total]))/n_sessions_total

        # RAC acute each 7 CALENDAR days
        # Calculate 7 days training blocks
        # Returns:
        # n_sessions_acute = Number of training sessions include in the acute block
        # previous_TL_acute = Position of the first session of the acute training block
        acute_TB <- training_blocks(training_dates = training_dates,
                                    actual_TL = n_sessions_total,
                                    diff_dates = 6)

        RAC_acute[n_sessions_total] = (sum(TL[acute_TB$previous_TL:n_sessions_total]))/acute_TB$n_sessions
      }

    } # end first month

    # from second moth to end of data
    else if(i >= 5){

      # loop over number of sessions / week
      for (j in 1:sessions_week$Freq[unique(weeks)[i]]) {
        # Count number of training sessions
        n_sessions_total <- n_sessions_total + 1

        # RAC chronic each 28 CALENDAR days
        # Calculate 28 days training blocks
        chronic_TB <- training_blocks(training_dates = training_dates,
                                      actual_TL = n_sessions_total,
                                      diff_dates = 27)
        RAC_chronic[n_sessions_total] = (sum(TL[chronic_TB$previous_TL:n_sessions_total]))/chronic_TB$n_sessions

        # RAC acute each 7 CALENDAR days
        # Calculate 7 days training blocks
        acute_TB <- training_blocks(training_dates = training_dates,
                                    actual_TL = n_sessions_total,
                                    diff_dates = 6)
        RAC_acute[n_sessions_total] = (sum(TL[acute_TB$previous_TL:n_sessions_total]))/acute_TB$n_sessions
      }

    } # end >= second moth

  } # end of loop over the data

  # Calculate ACWR
  RAC_ACWR <- RAC_acute / RAC_chronic

  return(list(RAC_acute = round(RAC_acute, 2),
              RAC_chronic = round(RAC_chronic, 2),
              RAC_ACWR = round(RAC_ACWR, 2)))

}# end RAC function
