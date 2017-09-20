
#' @export
runMosaic <- function() {
  appDir <- system.file("MosaicApp", "main", package = "Mosaic")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Mosaic`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}