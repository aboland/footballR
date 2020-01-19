#' Function to start Shiny app
#'
#'
#' @export
run_ShinyApp <- function() {
  appDir <- system.file("shiny-examples", "PL_app", package = "PremieRLeague")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `PremieRLeague`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
