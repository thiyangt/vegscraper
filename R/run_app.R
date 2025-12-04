#' Run the Shiny App
#' @export
run_app <- function(...) {

  appDir <- system.file("app", package = "vegscrapr")

  if (appDir == "") {
    stop("Could not find the Shiny app directory.
Make sure inst/app/ exists and reinstall vegscrapr.")
  }

  shiny::runApp(appDir, ...)
}
