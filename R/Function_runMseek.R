#' Run Mseek
#' 
#' Run Mseek in your webbrowser.
#' 
#' @param ... Additional arguments passed to shiny::runApp, e.g. to set up the port used or to make Mseek reachable from other computers (with host = getOption("shiny.host", "0.0.0.0"))
#'
#'
#' @export
runMseek <- function(...) {
  
  #assign("devel__mode", devel, envir = "package:Mseek")
  
  appDir <- system.file("app", package = "METABOseek")
  if (appDir == "") {
    stop("Could not find Mseek directory. Try re-installing `Mseek`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, ...)
}


