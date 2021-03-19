#' photoR
#'
#' photoR runs the ShinyApp to interactively process data on adult attendance at their nest and nest contents to estimate phenological events and reproductive success of breeding birds.
#' Running the photoR app will spawn an interactive web page that allows the user to upload two data sets, correctly formatted and in a .csv format.
#' The attendance data should have the following headers (case sensitive):SPLIT_YEAR, ROOKERY, COLONY, CAMERA,	SPP, NEST,	DATE, and	MAXN. Note that DATE can be specified in any way (default is for m/d/y), or as separate columns named DAY, MON, YR.
#' The nest content data should have the following headers (case sensitive):SPLIT_YEAR,  ROOKERY,  SPP,  COLONY,  CAMERA,  NEST, DATE,  LAY,  MAXE,  HATCH,  MAXC, and  CRECHE. Note that DATE can be specified in anyway (default is for m/d/y), or as separate columns named DAY, MON, YR.
#'
#' The app will iteratively check the input data before processing. It first calls 'import_validation()' to address errors in the headers supplied in the data. Once error free, it then calls 'error_checking' to assess potential errors in the data. The user must fix any identified errors before processing can proceed.
#'
#' Protocols for data preparation are in progress, but available from the author upon request.
#'
#' @return A dataframe for download as a .csv file. The data are summarized  and formatted to fit specific CEMP Protocol data forms (www.ccamlr.org).
#' @usage photoR()
#' @export
photoR<-function() {
  appDir <- system.file("shiny", package = "photoR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `photoR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
