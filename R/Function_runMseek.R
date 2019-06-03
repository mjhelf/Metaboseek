#' RunMseek
#' 
#' Run Mseek in your webbrowser.
#' 
#' @param ... Additional arguments passed to shiny::runApp, e.g. to set up the port used or to make Mseek reachable from other computers (with host = getOption("shiny.host", "0.0.0.0"))
#'
#'
#' @export
runMseek <- function(...) {
  
  #assign("devel__mode", devel, envir = "package:METABOseek")
  
  appDir <- system.file("app", package = "METABOseek")
  if (appDir == "") {
    stop("Could not find METABOseek directory. Try re-installing `METABOseek`.", call. = FALSE)
  }
  

  
  shiny::runApp(appDir, ...)
}


#' runPeptideApp
#' 
#' Run Mseek in your webbrowser.
#' 
#' @param ... Additional arguments passed to shiny::runApp, e.g. to set up the port used or to make Mseek reachable from other computers (with host = getOption("shiny.host", "0.0.0.0"))
#'
#'
#' @export
runPeptideApp <- function(...) {
    
    #assign("devel__mode", devel, envir = "package:METABOseek")
    
    appDir <- system.file("PeptideApp", package = "METABOseek")
    if (appDir == "") {
        stop("Could not find METABOseek directory. Try re-installing `METABOseek`.", call. = FALSE)
    }
    
    
    
    shiny::runApp(appDir, ..., launch.browser = TRUE)
}