
#' Rolling Average Uncoupled
#'
#' @param TL training load
#' @param weeks training weeks
#' @param training_dates training dates
#'
#' @return {This function returns the following variables:
#' \itemize{
#' \item RAU_chronic: RAU - chronic training load.
#' \item RAU_acute: RAU - acute training load.
#' \item RAU_ACWR: RAU - Acute-Chronic Workload Ratio.
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
#' result_RAU <- RAU(TL = training_load_1$TL,
#'                    weeks = training_load_1$Week,
#'                    training_dates = training_load_1$Training_Date)
#'
#' # set user working directory
#' setwd(oldwd)
#' }
#'
RAU <- function (TL,
                 weeks,
                 training_dates) {

  # Count number of sessions / week
  sessions_week <- as.data.frame(table(weeks))

  # Initialize variables
  RAU_chronic <- c()
  RAU_acute <- c()
  RAU_ACWR <- c()
  # Initialize number of training sessions
  n_sessions_total <- 0
  # We also need a new counter for the number of training sessions
  n_sessions_chronic <- 1

  # Loop over the total days of training
  for (i in unique(weeks)) {

    # First training week: RAC_chronic = NA / RAC_acute = Training load
    if(i == 1){

      # loop over number of sessions / week
      for (j in 1:sessions_week$Freq[unique(weeks)[i]]) {

        # First training day: RAC_chronic = TL / RAC_acute = TL
        if(j == 1){
          # Count number of training sessions
          n_sessions_total <- n_sessions_total + 1
          # RAU_chronic[n_sessions_total] = TL[n_sessions_total]
          RAU_chronic[n_sessions_total] = NA
          RAU_acute[n_sessions_total] = TL[n_sessions_total]
        }

        # Rest of the week
        else if(j >= 2){
          # Count number of training sessions
          n_sessions_total <- n_sessions_total + 1
          # RAU_chronic[n_sessions_total] = (sum(TL[1:n_sessions_total]))/n_sessions_total
          RAU_chronic[n_sessions_total] = NA
          RAU_acute[n_sessions_total] = (sum(TL[1:n_sessions_total]))/n_sessions_total
        }

        # During first week of RAU ACWR = 0
        RAU_ACWR[n_sessions_total] <- NA

      }
    } # end first week

    # from the beginning of the second week to end of third week
    else if(i >= 2 && i < 5){

      # loop over number of sessions / week
      for (j in 1:sessions_week$Freq[unique(weeks)[i]]) {

        # Count number of training sessions
        n_sessions_total <- n_sessions_total + 1

        # RAU acute each 7 CALENDAR days
        # Calculate 7 days training blocks
        # Returns:
        # n_sessions = Number of training sessions include in the acute block
        # previous_TL = Position of the first session of the acute training block
        acute_TB <- training_blocks(training_dates = training_dates,
                                    actual_TL = n_sessions_total,
                                    diff_dates = 6)

        RAU_acute[n_sessions_total] = (sum(TL[acute_TB$previous_TL:n_sessions_total]))/acute_TB$n_sessions


        # RAU chronic
        # (acute_TB$previous_TL-1) indicates the position of the first session
        # We are going to reuse this value to indicate the first value of the RAU chronic block
        RAU_chronic[n_sessions_total] = (sum(TL[(acute_TB$previous_TL-1):1]))/n_sessions_chronic
        n_sessions_chronic <- n_sessions_chronic + 1
      }
    } # end first 3 weeks

    # from fourth week to end of data
    else if(i >= 5){

      # loop over number of sessions / week
      for (j in 1:sessions_week$Freq[unique(weeks)[i]]) {

        # Count number of training sessions
        n_sessions_total <- n_sessions_total + 1

        # RAU acute each 7 CALENDAR days
        # Calculate 7 days training blocks
        # Returns:
        # n_sessions = Number of training sessions include in the acute block
        # previous_TL = Position of the first session of the acute training block
        acute_TB <- training_blocks(training_dates = training_dates,
                                    actual_TL = n_sessions_total,
                                    diff_dates = 6)

        RAU_acute[n_sessions_total] = (sum(TL[acute_TB$previous_TL:n_sessions_total]))/acute_TB$n_sessions


        # RAU chronic
        #
        chronic_TB <- training_blocks(training_dates = training_dates,
                                      actual_TL = n_sessions_total,
                                      diff_dates = 20)
        # Number of sessions include in the chronic training block =
        # Number of sessions in chronic - number of sessions in acute
        RAU_chronic[n_sessions_total] = (sum(TL[acute_TB$previous_TL:chronic_TB$previous_TL]))/chronic_TB$n_session
      }


    }
  } # end loop

  # Calculate ACWR
  RAU_ACWR <- RAU_acute / RAU_chronic

  return(list(RAU_acute = round(RAU_acute, 2),
              RAU_chronic = round(RAU_chronic, 2),
              RAU_ACWR = round(RAU_ACWR, 2)))

} # end RAU function
