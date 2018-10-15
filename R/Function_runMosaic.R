#' Run Mosaic
#' 
#' Run Mosaic in your webbrowser.
#' 
#' @param ... Additional arguments passed to shiny::runApp, e.g. to set up the port used or to make Mosaic reachable from other computers (with host = getOption("shiny.host", "0.0.0.0"))
#'
#'
#' @export
runMosaic <- function(...) {
  
  #assign("devel__mode", devel, envir = "package:Mosaic")
  
  appDir <- system.file("app", package = "Mosaic")
  if (appDir == "") {
    stop("Could not find Mosaic directory. Try re-installing `Mosaic`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, ...)
}


