#' MosaicMinimalServer
#'
#' A minimal server logic for Mosaic. Function has to be called in a shiny server function context. Will load example data if it is not present in the parent environment.
#' Save time by providing the parent environment with objects \code{tab1}, \code{tab2} and \code{MSD} by calling MosaicExamplePreload()
#'
#' @param data load example MS data?
#' @param tables load example tables?
#' @param diagnostics run diagnostics (shinyjs::runcodeServer()) code?
#'   
#' @importFrom shinyjs runcodeServer 
#' 
#' @export
MosaicMinimalServer <- function(data = T, tables = T, diagnostics = T){
  eval.parent(quote({
    options(shiny.maxRequestSize=10*1024*1024^2) 
    keyin <- reactiveValues(keyd = "NO")
    
    observeEvent(input$keyd,{keyin$keyd <- input$keyd})
    
    projectData <- reactiveValues(filegroupfiles =NULL,
                                  csvfiles = NULL,
                                  filegroups = NULL,
                                  projectName = paste0("MOSAiC_session_",strftime(Sys.time(),"%Y%m%d_%H%M%S")))
    
    GlobalOpts <- ListToReactiveValues(.MosaicOptions)
    
    
  }))
  if(diagnostics){
    eval.parent(quote({
      runcodeServer()
      
      
    }))
  }
  if(data){
    eval.parent(quote({
      if(!(exists("MSD")
           && class(MSD) == "list" && class(MSD$layouts$Group1) == "rawLayout")){
        MosaicExamplePreload(data = T, tables = F)}
      
      MSData <- reactiveValues(layouts = list(Group1 = MSD$layouts$Group1), #List of rawfile paths (unsorted)
                               rawgrouptable = MSD$rawgrouptable,
                               index = "Group1",
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
      MSData <- reactiveValues(layouts = NULL, #List of rawLayouts (unsorted)
                               rawgrouptable = NULL,
                               index = NULL,
                               rootfolder = getwd(),
                               localfolders = character(0),
                               RTcorr = NULL,
                               active = NULL,
                               filelist = NULL,
                               data = NULL,
                               selectedFeats = NULL) #rawfs
    }))
  }
  if(tables){
    eval.parent(quote({
      if(!(exists("tab1") && exists("tab2")
           && class(tab1) == class(tab2) && class(tab2) == "MosaicFT")){
        MosaicExamplePreload(tables = T, data = F)}
      
      featureTables <- reactiveValues(tables = #ListToReactiveValues(
                                        list(table0 = constructFeatureTable(),
                                             table1 = tab1,
                                             table2 = tab2)
                                      #    )
                                      ,
                                      index = c("Custom Table" = "table0",
                                                "mini_example_features.csv" = "table1",
                                                "large_example_features.csv" = "table2"),
                                      active = "table1",
                                      activerow = 1)
    }))
  }else{
    eval.parent(quote({
      featureTables <- reactiveValues(tables = list(table0 = constructFeatureTable()),
                                      index = c("Custom Table" = "table0"),
                                      active = "table0"
      )
    }))
  }
}

#' MosaicExamplePreload
#'
#' Loads example data into the parent environment for testing and development purposes
#'
#' @param tables load example tables?
#' @param data load example MS data?
#' 
#' @import shiny
#' @export
MosaicExamplePreload <- function(tables = T, data = T){
  if(tables){
    eval.parent(quote({
      tab1 <- constructFeatureTable (df= read.csv(system.file("extdata","examples", "example projectfolder", "mini_example_features.csv", package = "Mosaic"), stringsAsFactors = F),# data frame 
                                     mzcol= "mz", #
                                     rtcol= "rt", #column in df with mz values (columnname)
                                     commentcol = "comments",
                                     fragmentcol = "fragments",
                                     rtFormat = "sec", # "sec" or "min" 
                                     anagrouptable = read.csv(system.file("extdata","examples", "example projectfolder", "analysis_groups.csv", package = "Mosaic"), stringsAsFactors = F),
                                     tablename = "mini_example_features.csv",
                                     editable = F)
      
      tab2 <- constructFeatureTable (df= read.csv(system.file("extdata","examples", "example projectfolder", "large_example_features.csv", package = "Mosaic"), stringsAsFactors = F),# data frame 
                                     mzcol= "mz", #
                                     rtcol= "rt", #column in df with mz values (columnname)
                                     commentcol = "comments",
                                     fragmentcol = "fragments",
                                     rtFormat = "sec", # "sec" or "min" 
                                     anagrouptable = read.csv(system.file("extdata","examples", "example projectfolder", "analysis_groups.csv", package = "Mosaic"), stringsAsFactors = F),
                                     tablename = "large_example_features.csv",
                                     editable = F)
    }))}
  
  if(data){
    eval.parent(quote({
      rawgroups <- read.csv(system.file("extdata", "examples", "example projectfolder", "filegroups.csv", package = "Mosaic"), stringsAsFactors = F)
      
      MSD <- list(layouts = list(Group1 = constructRawLayout(rawgrouptable = rawgroups)), #List of rawfile paths (unsorted)
                  rawgrouptable = NULL,
                  index = "Group1",
                  rootfolder = getwd(),
                  localfolders = character(0),
                  RTcorr = NULL,
                  active = "Group1",
                  filelist = rawgroups$File,
                  data = loadRawM(rawgroups$File))
    }))
  }
}
