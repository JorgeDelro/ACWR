
#' Exponentially Weighted Moving Average
#'
#' @param TL training load
#'
#' @return {This function returns the following variables:
#' \itemize{
#' \item EWMA_chronic: EWMA - chronic training load.
#' \item EWMA_acute: EWMA - acute training load.
#' \item EWMA_ACWR: EWMA - Acute-Chronic Workload Ratio.
#' }}
#'
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
#' result_EWMA <- EWMA(TL = training_load_1$TL)
#'
#' # set user working directory
#' setwd(oldwd)
#' }
#'
EWMA <- function(TL) {


  # lambda <- 2/(N + 1)

  # Initialize variables
  EWMA_chronic <- c()
  EWMA_acute <- c()
  EWMA_ACWR <- c()
  lambda_acute <- 2/(7+1)
  lambda_chronic <- 2/(28+1)

  # Loop over the TL
  for (i in seq_along(TL)) {

    # First training day: EWMA_chronic = TL / EWMA_acute = TL
    if(i == 1){
      EWMA_chronic[i] = TL[i]
      EWMA_acute[i] = TL[i]
    }

    if(i > 1){
      EWMA_chronic[i] = TL[i] * lambda_chronic + ((1- lambda_chronic)* EWMA_chronic[i-1])
      EWMA_acute[i] = TL[i] * lambda_acute + ((1- lambda_acute)* EWMA_acute[i-1])
    }

    EWMA_ACWR <- EWMA_acute / EWMA_chronic

  }

  return(list(EWMA_acute = EWMA_acute,
              EWMA_chronic = EWMA_chronic,
              EWMA_ACWR = EWMA_ACWR))

} # end EWMA
