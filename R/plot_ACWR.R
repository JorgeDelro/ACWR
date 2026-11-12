
#' ACWR plots using d3.js
#'
#' @param db a data frame
#' @param TL training load
#' @param ACWR Acute Chronic Workload Ratio
#' @param day training days: day numbers (whole numbers) or dates (Date or
#'        POSIXct). The x-axis is a continuous calendar from the first to the
#'        last day.
#' @param ID ID of the subjects
#' @param colour colour of the bars. By default "#87CEEB" (skyblue)
#' @param xLabel x-axis label. By default "Days"
#' @param y0Label left y-axis label. By default `"Load [AU]"`
#' @param y1Label right y-axis label. By default "Acute:chronic workload ratio"
#' @param plotTitle Title of the plot. By default "ACWR"
#'
#' @import r2d3
#'
#' @return This function returns a d3.js object for a single subject.
#'          For several subjects it returns a list of d3.js objects.
#'          The x-axis has one slot per calendar day between the first and
#'          the last day, labelled every 7 days; days without a session have
#'          a bar of height 0. Sessions on the same day are shown as one bar
#'          (summed load). The ACWR line joins the sessions that have a value,
#'          across rest days, and is not drawn where the ACWR is NA.
#' @export
#'
#' @examples
#' data("training_load", package = "ACWR")
#'
#' # Calculate ACWR
#' result_ACWR <- ACWR(db = training_load,
#'                     ID = "ID",
#'                     TL = "TL",
#'                     days = "Day",
#'                     training_dates = "Training_Date",
#'                     ACWR_method = c("EWMA", "RAC", "RAU"))
#'
#' # Plot for 1 subject
#' # Select the first subject
#' result_ACWR_1 <- result_ACWR[result_ACWR[["ID"]] == 1, ]
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
#'                        TL = "TL",
#'                        ACWR = "RAC_ACWR",
#'                        day = "Day",
#'                        ID = "ID")
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
    y1Label <- "Acute:chronic workload ratio"
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
  d3_ACWR <- r2d3(data = one_bar_per_day(db),
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
      db_ind <- one_bar_per_day(db[db[[ID]] == i, ])

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

#' One bar per calendar day
#'
#' Several sessions on one day become one bar (summed load); the ACWR of a
#' day is the same for all its sessions, so the first one is kept. Days
#' without a session between the first and the last day are added with a
#' load of 0 and no ACWR (`session = FALSE`).
#' @noRd
one_bar_per_day <- function(db) {

  day <- db$day
  if (anyNA(day)) {
    stop("the day column has missing values", call. = FALSE)
  }
  if (inherits(day, "POSIXt")) {
    day <- as.Date(format(day, "%Y-%m-%d"))
  }
  if (inherits(day, "Date")) {
    all_days <- seq(min(day), max(day), by = "day")
  } else if (is.numeric(day) && all(day == round(day))) {
    all_days <- seq(min(day), max(day))
  } else {
    stop("the day column must hold day numbers (whole numbers) or dates ",
         "(Date or POSIXct) to draw a calendar axis", call. = FALSE)
  }

  first <- !duplicated(day)
  session_TL <- as.vector(rowsum(db$TL, match(day, day[first]),
                                 reorder = TRUE))
  idx <- match(all_days, day[first])

  data.frame(TL = ifelse(is.na(idx), 0, session_TL[idx]),
             ACWR = db$ACWR[first][idx],
             day = all_days,
             session = !is.na(idx))
}
