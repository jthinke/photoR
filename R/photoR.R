#' photoR
#'
#' Run the photoR Shiny App
#'
#' @return
#' @export
#'
#' @examples
photoR<-function() {
  appDir <- system.file("shiny-examples", "photoR", package = "photoR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
