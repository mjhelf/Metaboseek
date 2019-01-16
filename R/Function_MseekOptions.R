#' MseekOptions
#'
#' Load and/or change .MseekOptions
#' 
#' @param defaults if TRUE, default MseekOptions are loaded
#' @param ... parameters to be modified
#' @importFrom jsonlite serializeJSON unserializeJSON
#'
#' @export
MseekOptions <- function(..., defaults = F){
  
  if(!file.exists(file.path(system.file("config", package = "METABOseek"), "MseekOptions.json")) || defaults){
    .MseekOptions <<- list( activateLocalFiles = T,
                             activateXCMS = T,
                             develMode = FALSE,
                             loadExampleData = FALSE,
                             loadExampleTable = FALSE,
                             enabledCores = 4,
                             filePaths = c(examples = system.file("extdata","examples", package = "METABOseek"),  if(Sys.info()['sysname'] == "Windows"){checkFolders()}else{c(root ="/")}),
                             filePattern = paste(
                               paste("\\.", 
                                     c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]",
                                       "[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]"),
                                     "$",
                                     sep = ""), 
                               collapse = "|"),
                             perPage = as.integer(100),
                             serverMode = F,
                             PPMwindow = 5,
                             RTtoggle = F,
                             plotYzoom = 1,
                             plotCols = 5,
                             plotLw = 1,
                             MLtoggle = T,
                             RTwindow = 30,
                             plotCx = 1,
                             TICtoggle = F,
                             colorscheme = "Mseek.colors",
                             plotTransparency = 0.8)
    
  }
  else{
    .MseekOptions <<- unserializeJSON(readChar(system.file("config", "MseekOptions.json", package = "Mseek"), file.info(system.file("config", "MseekOptions.json", package = "Mseek"))$size))
  }
  
  if(!.MseekOptions$serverMode && Sys.info()['sysname'] == "Windows"){
    .MseekOptions$filePaths <<- c("User folders" = Sys.getenv("USERPROFILE"), examples = system.file("extdata","examples", package = "Mseek"), checkFolders())
  }  
  
  
  
  newSettings <- list(...)
  
  for(i in names(newSettings)){
    
    .MseekOptions[[i]] <<- newSettings[[i]]
    
  }
  
  if(dirname(system.file(package = "Mseek")) %in% .libPaths()){
    write(jsonlite::serializeJSON(.MseekOptions, pretty = T), file.path(system.file("config", package = "Mseek"), "MseekOptions.json"))
  }
  
}