#' photoR
#'
#' Run the photoR Shiny App
#'
#'
#' @usage photoR()
#' @export
photoR<-function() {
  appDir <- system.file("shiny", package = "photoR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `photoR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
