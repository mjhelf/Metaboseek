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
  
  #assign("devel__mode", devel, envir = "package:Mosaic")
  
  appDir <- system.file("MosaicApp", "main", package = "Mosaic")
  if (appDir == "") {
    stop("Could not find Mosaic directory. Try re-installing `Mosaic`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, ...)
}

#' devel__mode
#'
#' @export
devel__mode <- FALSE

#' default__root
#'
#' @export
default__root <- getwd()

#' Run XCMS interface
#' 
#' Run the XCMS runner in your webbrowser. Currently only intended for use on a local computer (file system access required).
#' the XCMS runner will also be integrated into MOSAiC eventually.
#' 
#' @param ... Additional arguments passed to shiny::runApp.
#'
#'
#' @export
runXcms <- function(...) {
  appDir <- system.file("MosaicApp", "xcmsRunner", package = "Mosaic")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Mosaic`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, ...)
}

#' Make filenames for exported .csv or .pdf files
#' 
#' Generate a filename from project name and filter criteria
#' 
#' @param projectName ProjectName used as prefix
#' @param FT Mosaic's featureTable reactiveValues (or a list with same structure)
#'
#'
#' @export
filenamemaker <- function(projectName,
                          FT){
  
  titleout <- paste(projectName, 
                    names(FT$index[which(FT$index == FT$active)]),
                    sep = "_")
  
  
  for(f in FT$tables[[FT$active]]$filters$filters){
    
    if(!is.atomic(f) && f$active){
      titleout <- paste(titleout,f$column,f$minSel, f$maxSel, f$txtSel, sep = "_")
    }
    
    
  }
  return(gsub(".csv$","",gsub("_$","",titleout)))
  
}


#' Get common root folder of file paths
#' 
#' From: https://rosettacode.org/wiki/Find_common_directory_path#R
#' 
#' @param paths vector of paths
#' @param delim folder delimiter
#'
#'
#' @export
get_common_dir <- function(paths, delim = "/")
{
  if(length(unique(dirname(paths))) == 1){
    dirname(paths)
  }else{
  
  path_chunks <- strsplit(paths, delim)
  
  i <- 1
  repeat({
    current_chunk <- sapply(path_chunks, function(x) x[i])
    if(any(current_chunk != current_chunk[1])) break
    i <- i + 1
  })
  paste(path_chunks[[1]][seq_len(i - 1)], collapse = delim)
  }
}