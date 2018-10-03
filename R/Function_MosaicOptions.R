#' MosaicOptions
#'
#' Load and/or change .MosaicOptions
#' 
#' @param defaults if TRUE, default MosaicOptions are loaded
#' @param ... parameters to be modified
#' @importFrom jsonlite serializeJSON unserializeJSON
#'
#' @export
MosaicOptions <- function(..., defaults = F){
  
  if(!file.exists(file.path(system.file("config", package = "Mosaic"), "MosaicOptions.json")) || defaults){
    .MosaicOptions <<- list( activateLocalFiles = T,
                             activateXCMS = T,
                             develMode = FALSE,
                             loadExampleData = FALSE,
                             loadExampleTable = FALSE,
                             enabledCores = 4,
                             filePaths = c(examples = system.file("data", package = "Mosaic"),  if(Sys.info()['sysname'] == "Windows"){checkFolders()}else{c(root ="/")}),
                             filePattern = paste(
                               paste("\\.", 
                                     c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]",
                                       "[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]"),
                                     "$",
                                     sep = ""), 
                               collapse = "|"),
                             perPage = as.integer(100),
                             serverMode = F)
    
  }
  else{
    .MosaicOptions <<- unserializeJSON(readChar(system.file("config", "MosaicOptions.json", package = "Mosaic"), file.info(system.file("config", "MosaicOptions.json", package = "Mosaic"))$size))
  }
  
  if(!.MosaicOptions$serverMode && Sys.info()['sysname'] == "Windows"){
    .MosaicOptions$filePaths <<- c("User folders" = Sys.getenv("USERPROFILE"), examples = system.file("data", package = "Mosaic"), checkFolders())
  }  
  
  
  
  newSettings <- list(...)
  
  for(i in names(newSettings)){
    
    .MosaicOptions[[i]] <<- newSettings[[i]]
    
  }
  
  if(gsub("/Mosaic","",system.file(package = "Mosaic")) %in% .libPaths()){
    write(jsonlite::serializeJSON(.MosaicOptions, pretty = T), file.path(system.file("config", package = "Mosaic"), "MosaicOptions.json"))
  }
  
}