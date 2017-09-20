#' Run Mosaic
#' 
#' Run Mosaic in your webbrowser.
#' 
#' @param devel Enable troubleshooting tools (default: FALSE). May constitute security risk if run on a publicly available server.
#' @param server Enable server mode (users can not access file paths on the server directly), default is TRUE.
#' @param ... Additional arguments passed to shiny::runApp, e.g. to set up the port used or to make Mosaic reachable from other computers (with host = getOption("shiny.host", "0.0.0.0"))
#'
#'
#' @export
runMosaic <- function(devel = F, server = T, ...) {
  appDir <- system.file("MosaicApp", "main", package = "Mosaic")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Mosaic`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, ...)
}