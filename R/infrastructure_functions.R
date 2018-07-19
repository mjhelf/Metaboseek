#' ListToReactiveValues
#' 
#' recursively converts lists to reactiveValues
#' 
#' @param ls a list object
#' 
#' @import shiny
#' 
#' @export 
ListToReactiveValues <- function(ls){
  
  #note: 
  if(!is.list(ls) 
     || is.data.frame(ls)
     || (is.list(ls) & length(names(ls)[names(ls)!=""] ) != length(ls) & !is.reactivevalues(ls)) ){
    return(ls)
  }
  
  re <- reactiveValues()
  for (i in names(ls)){
    re[[i]] <- ListToReactiveValues(ls[[i]])
  }
  return(re)
  
}

#' MosaicExamplePreload
#'
#' Loads example data into the parent environment for testing and development purposes
#'
#' @import shiny
#' @export
MosaicExamplePreload <- function(){
  eval.parent(quote({
    tab1 <- constructFeatureTable (df= read.csv(system.file("data", "tables", "mini_example_features.csv", package = "Mosaic"), stringsAsFactors = F),# data frame 
                                   mzcol= "mz", #
                                   rtcol= "rt", #column in df with mz values (columnname)
                                   commentcol = "comments",
                                   fragmentcol = "fragments",
                                   rtFormat = "sec", # "sec" or "min" 
                                   anagrouptable = read.csv(system.file("data", "tables", "analysis_groups.csv", package = "Mosaic"), stringsAsFactors = F),
                                   tablename = "Custom Table",
                                   editable = T)
    
    tab2 <- constructFeatureTable (df= read.csv(system.file("data", "tables", "large_example_features.csv", package = "Mosaic"), stringsAsFactors = F),# data frame 
                                   mzcol= "mz", #
                                   rtcol= "rt", #column in df with mz values (columnname)
                                   commentcol = "comments",
                                   fragmentcol = "fragments",
                                   rtFormat = "sec", # "sec" or "min" 
                                   anagrouptable = read.csv(system.file("data", "tables", "analysis_groups.csv", package = "Mosaic"), stringsAsFactors = F),
                                   tablename = "Custom Table",
                                   editable = T)
    
    rawgroups <- read.csv(system.file("data", "tables", "filegroups_all.csv", package = "Mosaic"), stringsAsFactors = F)
    rawgroups$File <- file.path(system.file("data", package = "Mosaic"), rawgroups$File)
    
    MSD <- list(layouts = list(Group1 = constructRawLayout(rawgrouptable = rawgroups)), #List of rawfile paths (unsorted)
                rawgrouptable = NULL,
                index = NULL,
                rootfolder = getwd(),
                localfolders = character(0),
                RTcorr = NULL,
                active = "Group1",
                filelist = rawgroups$File,
                data = loadRawM(rawgroups$File))
  }))
}




# Define UI for dataset viewer app ----
#' MosaicMinimalUi
#'
#' A minimal UI for Mosaic that can be extended with additional objects for testing and development purposes
#'
#' @importFrom shinyjs runcodeUI 
#' @export
MosaicMinimalUi <- function(..., diagnostics = T){
  fluidPage(
    tags$script('
                $(document).on("keydown", function (e) {
                Shiny.onInputChange("keyd", e.which);
                });
                $(document).on("keyup", function (e) {
                Shiny.onInputChange("keyd", "NO");
                });
                '),
    if(diagnostics){
    fluidPage(...,
              runcodeUI(code = "", type = c("text", "textarea", "ace"), width = NULL,
              height = NULL, includeShinyjs = FALSE),
    verbatimTextOutput('diag'))}
    else{
     ... 
    }
    )}

#' MosaicMinimalServer
#'
#' A minimal server logic for Mosaic. Function has to be called in a shiny server function context. Will load example data if it is not present in the parent environment.
#' Save time by providing the parent environment with objects \code{tab1}, \code{tab2} and \code{MSD} by calling MosaicExamplePreload()
#'
#' @importFrom shinyjs runcodeServer 
#' @export
MosaicMinimalServer <- function(exampleData = T, diagnostics = T){
  eval.parent(quote({
    options(shiny.maxRequestSize=10*1024*1024^2) 
    keyin <- reactiveValues(keyd = "NO")
    
    observeEvent(input$keyd,{keyin$keyd <- input$keyd})
    
    projectData <- reactiveValues(filegroupfiles =NULL,
                                  csvfiles = NULL,
                                  filegroups = NULL,
                                  projectName = paste0("MOSAiC_session_",strftime(Sys.time(),"%Y%m%d_%H%M%S")))
  }))
  if(diagnostics){
    eval.parent(quote({
      runcodeServer()
      
     
    }))
  }
  if(exampleData){
    eval.parent(quote({
      if(!(exists("tab1") && exists("tab2") && exists("MSD")
           && class(tab1) == class(tab2) && class(tab2) == "MosaicFT"
           && class(MSD) == "list" && class(MSD$layouts$Group1) == "rawLayout")){
        MosaicExamplePreload()}
      
      
      featureTables <- reactiveValues(tables = reactiveValues(table0 = constructFeatureTable(),
                                                              table1 = ListToReactiveValues(tab1),
                                                              table2 = ListToReactiveValues(tab2)),
                                      index = c("Custom Table" = "table0",
                                                "mini_example_features.csv" = "table1",
                                                "large_example_features.csv" = "table2"),
                                      active = "table1",
                                      activerow = 1)
      
      
      MSData <- reactiveValues(layouts = list(Group1 = MSD$layouts$Group1), #List of rawfile paths (unsorted)
                               rawgrouptable = MSD$rawgrouptable,
                               index = NULL,
                               rootfolder = getwd(),
                               localfolders = character(0),
                               RTcorr = NULL,
                               active = "Group1",
                               filelist = MSD$filelist,
                               data = MSD$data,
                               selectedFeats = NULL) #rawfs
    }))
  }else{
    eval.parent(quote({
      
      featureTables <- reactiveValues(tables = list(table0 = constructFeatureTable()),
                                      index = c("Custom Table" = "table0"),
                                      active = "table0"
      )
      
      MSData <- reactiveValues(layouts = NULL, #List of rawLayouts (unsorted)
                               rawgrouptable = NULL,
                               index = NULL,
                               rootfolder = c(root = default__root),
                               localfolders = character(0),
                               RTcorr = NULL,
                               active = NULL,
                               filelist = NULL,
                               data = NULL,
                               selectedFeats = NULL) #rawfs
    }))
  }
}