#' Launch Shiny App for LivTransplantEvolution
#'
#' A function that launches the Shiny app for LivTransplantEvolution.
#' This app is designed to facilitate the analysis of liver transplant
#' relevant risk scores and biomarkers, using the data and functionalities
#' provided by the LivTransplantEvolution package. The code for the Shiny
#' app is located in \code{./inst/shiny-scripts}.
#'
#' @return No return value, but opens up a Shiny page.
#'
#' @examples
#' \dontrun{
#'
#' LivTransplantEvolution::runLivTransplantEvolution()
#' }
#'
#'
#' @export
#' @importFrom shiny runApp

runLivTransplantEvolution <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "LivTransplantEvolution")
  actionShiny <- shiny::runApp(appDir, display.mode = "normal")
  return(actionShiny)
}
