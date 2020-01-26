#' Function to start Shiny app
#'
#'
#' @export
run_ShinyApp <- function() {
  appDir <- system.file("shiny-examples", "PL_app", package = "footballR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `footballR`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
