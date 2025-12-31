#' Launch Shiny Application
#'
#' This function launches the Shiny application included in the
#' **vegscraper** package. The app must be located in
#' `inst/app/` within the package source. When installed, this
#' directory is available via `system.file()`.
#'
#' @param ... Additional arguments passed to `shiny::runApp()`.
#'
#' @return The Shiny app is launched; no R object is returned.
#' Run the Shiny App
#' @examples
#' \dontrun{
#'   run_app()
#' }
#' @export
run_app <- function(...) {

  appDir <- system.file("app", package = "vegscrapr")

  if (appDir == "") {
    stop("Could not find the Shiny app directory.
Make sure inst/app/ exists and reinstall vegscrapr.")
  }

  shiny::runApp(appDir, ...)
}
