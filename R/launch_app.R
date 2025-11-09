#' Launch the aussiefirewx Shiny app
#'
#' @return Runs the Shiny application.
#' @export
launch_app <- function() {
  app_dir <- system.file("app", package = "aussiefirewx")
  if (app_dir == "") stop("App directory not found. Reinstall the package.", call. = FALSE)
  shiny::runApp(app_dir, display.mode = "normal")
}
