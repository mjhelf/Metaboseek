#' MseekOptions
#'
#' Load and/or change .MseekOptions. 
#' Call MseekOptions() without arguments to just load the config file 
#' (MseekOptions.json) into the R session as an object called \code{.MseekOptions}.
#' Any argument provided will be added to \code{.MseekOptions} as a named list item,
#' and will also be saved in the (MseekOptions.json) file so that the current settings
#' can be loaded in future R sessions.
#' 
#' @details
#' Some arguments have special behavior:
#' \itemize{
#' \item \code{defaults} setting \code{MseekOptions(default = T)} will reset all
#'  items in \code{.MseekOptions} to the defaults and save them to the comfig file
#' \item \code{testMode} setting \code{MseekOptions(testMode = T)} will 
#' disable saving the settings to the config file in this session
#' }
#' 
#' @section Usage in Metaboseek:
#' \code{.MseekOptions} itself is a list object, and therefore does not have reactive properties.
#' However, upon startup of Metaboseek, \code{.MseekOptions} will be loaded into the session as
#' \code{values$GlobalOpts}, so that the values can be used in a reactive context where they get invalidated when changed.
#' Note that not all items in \code{values$GlobalOpts} are in \code{.MseekOptions},
#'  but all items in \code{.MseekOptions} are in \code{values$GlobalOpts}. 
#'  To save changes to input fields so that they are loaded back the next time the app starts,
#'  set up an observer that assigns the input value via \code{MseekOptions(...)}.
#' 
#' @return generates a list called \code{.MseekOptions} in the global environment,
#'  and can add new items to that list.
#' 
#' @param defaults if TRUE, default MseekOptions are loaded
#' @param ... parameters to be modified
#' @importFrom jsonlite serializeJSON unserializeJSON
#'
#' @export
MseekOptions <- function(..., defaults = F){
  
  if(!file.exists(file.path(system.file("config", package = "Metaboseek"), "MseekOptions.json")) 
     || defaults){
    .MseekOptions <<- list( activateLocalFiles = T,
                             activateXCMS = T,
                             develMode = FALSE,
                             loadExampleData = FALSE,
                             loadExampleTable = FALSE,
                             enabledCores = 4,
                            keyinput.down = "NO",
                            
                             filePaths = c(examples = system.file("extdata","examples", package = "Metaboseek"),
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
                            #preferMseekIntensities = TRUE,
                            relPlotToggle = F,
                            raiseToggle = F,
                           recentProjects = system.file("extdata","examples", "example_projectfolder", package = "Metaboseek"),
                           SiriusSelIon = "[M+?]+",
                           SiriusCheckFinger = T,
                           SiriusSelInstrument = "orbitrap",
                           SiriusElements = "CHNOP[5]S[5]",
                           SiriusUseMS1 = T,
                           SiriusDBoptions = c('ALL_BUT_INSILICO','ALL','BIO',
                                               'METACYC','CHEBI','COCONUT',
                                               'ECOCYCMINE','GNPS','HMDB',
                                               'HSDB','KEGG','KEGGMINE',
                                               'KNAPSACK','MACONDA','MESH',
                                               'NORMAN','UNDP','PLANTCYC',
                                               'PUBCHEM','PUBMED','YMDB',
                                               'YMDBMINE','ZINCBIO'),
                           SiriusDBselected = "BIO",
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
                           mzquery.elementheuristic = TRUE,
                           mzquery.source = NULL,
                           mzquery.customMZ = 0,
                           mzquery.autoCalc = FALSE,
                           
                           graph.layouts.available = c("qgraph.layout.fruchtermanreingold",
                                                       "layout_nicely",
                                                       "layout_with_kk",
                                                       "layout_with_fr",
                                                       "layout_with_dh",
                                                       "layout_with_drl",
                                                       "layout_components"),
                           graph.layouts.selected = "qgraph.layout.fruchtermanreingold",
                           
                           p.adjust.method = "fdr"
                           )
    
  }
  else{
    .MseekOptions <<- unserializeJSON(readChar(system.file("config", "MseekOptions.json", package = "Metaboseek"), file.info(system.file("config", "MseekOptions.json", package = "Metaboseek"))$size))
  }
  
  if(!.MseekOptions$serverMode && Sys.info()['sysname'] == "Windows"){
    .MseekOptions$filePaths <<- c("User folders" = Sys.getenv("USERPROFILE"), examples = system.file("extdata","examples", package = "Metaboseek"), checkFolders())
   
    
    
  }  
  
  checkexfolder <- grep("extdata/examples/example_projectfolder", .MseekOptions$recentProjects, fixed = T)
  
  if(!.MseekOptions$serverMode && length(checkexfolder) >0 && !file.exists(.MseekOptions$recentProjects[checkexfolder])){
  
    .MseekOptions$recentProjects[checkexfolder] <<-  system.file("extdata","examples", "example_projectfolder", package = "Metaboseek")
    rawgroups <- read.csv(system.file("extdata", "examples", "example_projectfolder", "filegroups_base.csv", package = "Metaboseek"), stringsAsFactors = F, row.names = 1)
    rawgroups$File <- file.path(system.file("extdata", "examples", package = "Metaboseek"), rawgroups$File)
    write.csv(rawgroups, file.path(system.file("extdata", "examples", "example_projectfolder", package = "Metaboseek"), "filegroups.csv"))
    
  }
  
  #additional checks:
  .MseekOptions$rcdk.installed <<- "rcdk" %in% rownames(utils::installed.packages())
    

  
  
  newSettings <- list(...)

  
  for(i in names(newSettings)){
    
    .MseekOptions[[i]] <<- newSettings[[i]]
    
  }
  
  #prevent saving config while building project
  if(dirname(system.file(package = "Metaboseek")) %in% .libPaths() && !.MseekOptions$testMode){
    write(jsonlite::serializeJSON(.MseekOptions, pretty = T), file.path(system.file("config", package = "Metaboseek"), "MseekOptions.json"))
  }
  
}