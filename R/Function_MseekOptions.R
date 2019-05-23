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
  
  if(!file.exists(file.path(system.file("config", package = "METABOseek"), "MseekOptions.json")) 
     || defaults){
    .MseekOptions <<- list( activateLocalFiles = T,
                             activateXCMS = T,
                             develMode = FALSE,
                             loadExampleData = FALSE,
                             loadExampleTable = FALSE,
                             enabledCores = 4,
                            keyinput.down = "NO",
                            
                             filePaths = c(examples = system.file("extdata","examples", package = "METABOseek"),
                                           if(Sys.info()['sysname'] == "Windows"){checkFolders()}else{c(root ="/")}),
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
                             plotCx = 1.6,
                             TICtoggle = F,
                             colorscheme = "Mseek.colors",
                             plotTransparency = 0.8,
                            groupBy = "grouping",
                            colorBy = "file",
                            relPlotToggle = F,
                            raiseToggle = F,
                           recentProjects = system.file("extdata","examples", "example projectfolder", package = "METABOseek"),
                           SiriusSelIon = "[M+?]+",
                           SiriusCheckFinger = T,
                           SiriusSelInstrument = "orbitrap",
                           SiriusElements = "CHNOP[5]S[5]",
                           SiriusUseMS1 = T,
                           SiriusDBoptions = c("bio", "PubChem", "hmdb", "kegg", "knapsack", "biocyc"),
                           SiriusDBselected = "bio",
                           testMode = F,
                           
                           mzquery.elements = c("C","H","N","O","P","S"),
                           mzquery.mzppm = 5,
                           mzquery.mzcharge = 1,
                           mzquery.minUnsat = -5,
                           mzquery.maxUnsat = 40,
                           mzquery.minElements = "C1H2",
                           mzquery.maxElements = "P3S3",
                           mzquery.parity = "either",
                           mzquery.maxcounts = TRUE,
                           mzquery.valencefilter = TRUE,
                           mzquery.hcratio = TRUE,
                           mzquery.moreratios = TRUE,
                           mzquery.elementheuristic = TRUE
                           )
    
  }
  else{
    .MseekOptions <<- unserializeJSON(readChar(system.file("config", "MseekOptions.json", package = "METABOseek"), file.info(system.file("config", "MseekOptions.json", package = "METABOseek"))$size))
  }
  
  if(!.MseekOptions$serverMode && Sys.info()['sysname'] == "Windows"){
    .MseekOptions$filePaths <<- c("User folders" = Sys.getenv("USERPROFILE"), examples = system.file("extdata","examples", package = "METABOseek"), checkFolders())
   
    
    
  }  
  
  checkexfolder <- grep("extdata/examples/example projectfolder", .MseekOptions$recentProjects, fixed = T)
  
  if(!.MseekOptions$serverMode && length(checkexfolder) >0 && !file.exists(.MseekOptions$recentProjects[checkexfolder])){
  
    .MseekOptions$recentProjects[checkexfolder] <<-  system.file("extdata","examples", "example projectfolder", package = "METABOseek")
    rawgroups <- read.csv(system.file("extdata", "examples", "example projectfolder", "filegroups_base.csv", package = "METABOseek"), stringsAsFactors = F, row.names = 1)
    rawgroups$File <- file.path(system.file("extdata", "examples", package = "METABOseek"), rawgroups$File)
    write.csv(rawgroups, file.path(system.file("extdata", "examples", "example projectfolder", package = "METABOseek"), "filegroups.csv"))
    
  }
  
  #additional checks:
  .MseekOptions$rcdk.installed <<- "rcdk" %in% rownames(installed.packages())
    

  
  
  newSettings <- list(...)

  
  for(i in names(newSettings)){
    
    .MseekOptions[[i]] <<- newSettings[[i]]
    
  }
  
  #prevent saving config while building project
  if(dirname(system.file(package = "METABOseek")) %in% .libPaths() && !.MseekOptions$testMode){
    write(jsonlite::serializeJSON(.MseekOptions, pretty = T), file.path(system.file("config", package = "METABOseek"), "MseekOptions.json"))
  }
  
}