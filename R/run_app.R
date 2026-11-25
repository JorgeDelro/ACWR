#' Run the ACWR Shiny app
#'
#' Launches an interactive Shiny app (in Spanish, for teaching) to compute and
#' plot the ACWR and the Footballer Workload Footprint of an athlete, from an
#' uploaded file or from preloaded datasets.
#'
#' All the values shown by the app are computed with the functions of this
#' package ([RAC()], [RAU()], [EWMA()], [fwf_cumulative()] and
#' [fwf_delta()]), on the same daily calendar.
#'
#' @param ... arguments passed to [shiny::runApp()], e.g. `launch.browser`
#'        or `port`.
#'
#' @return called for its side effect (it starts the app); does not return
#'         until the app is closed.
#'
#' @details The app needs the packages shiny, bslib, ggplot2, plotly, DT
#'          and writexl. They are not installed with ACWR;
#'          `run_app()` says which ones are missing.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
run_app <- function(...) {

  needed <- c("shiny", "bslib", "ggplot2", "plotly", "DT", "writexl")
  missing_pkgs <- needed[!vapply(needed, is_installed, logical(1))]
  if (length(missing_pkgs) > 0) {
    stop("run_app() needs the following package(s), which are not ",
         "installed: ", paste(missing_pkgs, collapse = ", "), ".\n",
         "Install them with:\n  install.packages(c(",
         paste0("\"", missing_pkgs, "\"", collapse = ", "), "))",
         call. = FALSE)
  }

  app_dir <- system.file("shiny", "acwr_app", package = "ACWR")
  if (app_dir == "") {
    stop("could not find the app directory. Try re-installing ACWR.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, ...)

} # end run_app function

#' Is a package installed? (wrapper so tests can mock it)
#' @noRd
is_installed <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}
