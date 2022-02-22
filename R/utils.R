
#' Create Training Blocks
#'
#' @param training_dates training dates
#' @param actual_TL position of the actual training load
#' @param diff_dates difference in days
#'
#'
training_blocks <- function(training_dates,
                            actual_TL,
                            diff_dates
){

  # Initialize variables
  n_sessions <- 0

  # Create blocks BACKWARDS

  # loop over the training days backwards, starting from actual training day - 1
  for (previous_TL in rev(1:actual_TL-1)) {

    # Count total number of sessions include in a acute training block
    n_sessions <- n_sessions + 1

    # Calculate the difference in days between 2 dates (as integer)
    diff_dates_calculated <- as.integer(training_dates[actual_TL] - training_dates[previous_TL])

    # If the difference between 2 dates are X days or more more then stop the loop
    if(diff_dates_calculated >= diff_dates) {
      break
    }


  } # end loop

  # Number of training sessions include in the training block
  return(list(n_sessions = n_sessions +1,
              # Position of the first session of the training block
              previous_TL = previous_TL)
  )


} # end training blocks
