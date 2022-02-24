
#' ACWR plots using d3.js
#'
#' @param db a data frame
#' @param TL training load
#' @param ACWR Acute Chronic Workload Ratio
#' @param day training days
#' @param ID ID of the subjects
#' @param colour colour of the bars. By default "#87CEEB" (skyblue)
#' @param xLabel x-axis label. By default "Days"
#' @param y0Label left y-axis label. By default "Load [AU]"
#' @param y1Label right y-axis label. By default "Acute:chronic worload ratio"
#' @param plotTitle Title of the plot. By default "ACWR"
#'
#' @import r2d3
#'
#' @return This function returns a d3.js object for a single subject.
#'          For several subjects it returns a list of d3.js objects.
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
#' training_load_db <- data.frame(training_load)
#'
#' # Calculate ACWR
#' result_ACWR <- ACWR(db = training_load_db,
#'                  ID = "ID",
#'                  TL = "TL",
#'                  weeks = "Week",
#'                  days = "Day",
#'                  training_dates = "Training_Date",
#'                  ACWR_method = c("EWMA", "RAC", "RAU"))
#'
#' # Plot for 1 subject
#' # Select the first subject
#' result_ACWR_1 <- result_ACWR[result_ACWR[["ID"]] == 1,  ]
#'
#' # plot ACWR (e.g. EWMA)
#' ACWR_plot_1 <- plot_ACWR(db = result_ACWR_1,
#'                          TL = "TL",
#'                          ACWR = "EWMA_ACWR",
#'                          day = "Day")
#'
#' # Plot for several subjects
#' # plot ACWR (e.g. RAC)
#' ACWR_plot <- plot_ACWR(db = result_ACWR,
#'                          TL = "TL",
#'                          ACWR = "RAC_ACWR",
#'                          day = "Day",
#'                          ID = "ID")
#'
#' # set user working directory
#' setwd(oldwd)
#' }
#'
plot_ACWR <- function(db,
                      TL,
                      ACWR,
                      day,
                      ID = NULL,
                      colour = NULL,
                      xLabel = NULL,
                      y0Label = NULL,
                      y1Label = NULL,
                      plotTitle = NULL) {

  # Check variables
  if(is.null(db)){
    stop("you must provide a db")
  }
  if(is.null(TL)){
    stop("you must provide the name of the training load column in the database")
  }
  if(is.null(ACWR)){
    stop("you must provide the name of the ACWR column in the database")
  }
  if(is.null(day)){
    stop("you must provide the name of the day training column in the database")
  }
  if(is.null(colour)){
    colour <- "#87CEEB"
  }
  if(is.null(xLabel)){
    xLabel <- "Days"
  }
  if(is.null(y0Label)){
    y0Label <- "Load [AU]"
  }
  if(is.null(y1Label)){
    y1Label <- "Acute:chronic worload ratio"
  }
  if(is.null(plotTitle)){
    plotTitle <- "ACWR"
  }

  # Rename db columns
  # TL
  names(db)[names(db) == TL] <- "TL"
  # ACWR
  names(db)[names(db) == ACWR] <- "ACWR"
  # day
  names(db)[names(db) == day] <- "day"

  # Single plot
  if(is.null(ID)){

  # Day / TL / EWMA_ACWR
  d3_ACWR <- r2d3(data = db,
             script = system.file("ACWR_plot.js", package = "ACWR"),
             options = list(margin = 50,
                            #barPadding = 0.1,
                            colour = colour,
                            xLabel = xLabel,
                            y0Label = y0Label,
                            y1Label = y1Label,
                            plotTitle = plotTitle
             ),
  )

  # return d3 object
  return(d3_ACWR)

  } # end if is.null(ID)

  # Multiple plots
  if(!is.null(ID)){

    # Create an empty list to store the plots
    d3_ACWR_list <- list()

    # Loop over the subjects
    for (i in unique(db[[ID]])) {

      # Create individual dfs
      db_ind <- db[db[[ID]] == i,  c("TL", "ACWR", "day") ]

      # Individual plots
      d3_ACWR <- r2d3(data = db_ind,
                      script = system.file("ACWR_plot.js", package = "ACWR"),
                      options = list(margin = 50,
                                     #barPadding = 0.1,
                                     colour = colour,
                                     xLabel = xLabel,
                                     y0Label = y0Label,
                                     y1Label = y1Label,
                                     plotTitle = plotTitle
                      ),
      )

    # Store the plots inside the list
    d3_ACWR_list[[paste0("ID = ", i)]] <- d3_ACWR

    }

    # return a list
    return(d3_ACWR_list)
  } # end if !is.null(ID)


}
