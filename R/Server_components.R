#' MseekMinimalServer
#'
#' A minimal server logic for Mseek. Function has to be called in a shiny server
#'  function context. Will load example data if it is not present in the 
#'  parent environment. Save time by providing the parent environment with
#'   objects \code{tab1}, \code{tab2} and \code{MSD} by calling 
#'   \code{\link{MseekExamplePreload}()}
#'
#' @param data load example MS data?
#' @param tables load example tables?
#' @param diagnostics if TRUE, run diagnostics (shinyjs::runcodeServer())
#' 
#' @return provides a shiny session with basic elements needed for Metaboseek.
#'   
#' @importFrom shinyjs runcodeServer 
#' @importFrom BiocParallel register bpstart SnowParam MulticoreParam
#' 
#' @export
MseekMinimalServer <- function(data = T, tables = T, diagnostics = T){
  eval.parent(quote({
  
     #upload size limit
    options(shiny.maxRequestSize=10*1024*1024^2) 
  #  keyin <- reactiveValues(keyd = "NO")
        GlobalOpts <- ListToReactiveValues(c(.MseekOptions,
                                             list(project.filegroupfiles =NULL,
                                                  project.csvfiles = NULL,
                                                  project.filegroups = NULL,
                                                  project.projectName = paste0("Metaboseek_session_",
                                                                               strftime(Sys.time(),
                                                                                        "%Y%m%d_%H%M%S")))))

    observeEvent(input$keyd,{
        
        GlobalOpts$keyinput.keydown <- input$keyd
        
    })
    
    observe({})
    
   # observeEvent(input$keyd,{keyin$keyd <- input$keyd}, ignoreNULL = FALSE)
    
    projectData <- reactiveValues(filegroupfiles =NULL,
                                  csvfiles = NULL,
                                  filegroups = NULL,
                                  projectName = paste0("Metaboseek_session_",strftime(Sys.time(),"%Y%m%d_%H%M%S")))
    
    
    BiocParallel::register(
        BiocParallel::bpstart(
            if(Sys.info()['sysname'] == "Windows"){
                BiocParallel::SnowParam(.MseekOptions$enabledCores)
            }else{
                BiocParallel::MulticoreParam(.MseekOptions$enabledCores)
            }
        )
    )
   
    
  }))
  if(diagnostics){
    eval.parent(quote({
       
        shinyjs::runcodeServer()
  
    }))
  }
  if(data){
    eval.parent(quote({
      if(!(exists("MSD")
           && class(MSD) == "list" && class(MSD$layouts$Group1) == "rawLayout")){
        MseekExamplePreload(data = T, tables = F)}
      
      MSData <- reactiveValues(layouts = list(Group1 = MSD$layouts$Group1), #List of rawfile paths (unsorted)
                               rawgrouptable = MSD$rawgrouptable,
                               index = "Group1",
                               rootfolder = getwd(),
                               localfolders = character(0),
                               RTcorr = NULL,
                               active = "Group1",
                               filelist = MSD$filelist,
                               MSnExp = MSD$MSnExp,
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
           && class(tab1) == class(tab2) && class(tab2) == "MseekFT")){
        MseekExamplePreload(tables = T, data = F)}
      
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
                                      row_filters = TRUE,
                                      activerow = 1)
    }))
  }else{
    eval.parent(quote({
      featureTables <- reactiveValues(tables = list(table0 = constructFeatureTable()),
                                      index = c("Custom Table" = "table0"),
                                      active = "table0",
                                      row_filters = TRUE
      )
    }))
  }
    
    eval.parent(quote({
    
        values <- reactiveValues(featureTables = featureTables,
                                 MSData = MSData,
                                 GlobalOpts = GlobalOpts,
                                 projectData = projectData)
    
    }))
}

#' MseekExamplePreload
#'
#' Loads example data into the parent environment for 
#' testing and development purposes
#'
#' @return Generates objects \code{tab1}, \code{tab2} and \code{MSD} in the 
#' parent environment of the function call
#'
#' @param tables load example tables?
#' @param data load example MS data?
#' 
#' @import shiny
#' @export
MseekExamplePreload <- function(tables = T, data = T){
  if(tables){
    eval.parent(quote({
      tab1 <- constructFeatureTable (df= read.csv(system.file("extdata","examples", "example_projectfolder", "mini_example_features.csv", package = "Metaboseek"), stringsAsFactors = F),# data frame 
                                     mzcol= "mz", #
                                     rtcol= "rt", #column in df with mz values (columnname)
                                     commentcol = "comments",
                                     fragmentcol = "fragments",
                                     rtFormat = "sec", # "sec" or "min" 
                                     anagrouptable = read.csv(system.file("extdata","examples", "example_projectfolder", "analysis_groups.csv", package = "Metaboseek"), stringsAsFactors = F),
                                     tablename = "mini_example_features.csv",
                                     editable = F)
      
      tab2 <- constructFeatureTable (df= read.csv(system.file("extdata","examples", "example_projectfolder", "large_example_features.csv", package = "Metaboseek"), stringsAsFactors = F),# data frame 
                                     mzcol= "mz", #
                                     rtcol= "rt", #column in df with mz values (columnname)
                                     commentcol = "comments",
                                     fragmentcol = "fragments",
                                     rtFormat = "sec", # "sec" or "min" 
                                     anagrouptable = read.csv(system.file("extdata","examples", "example_projectfolder", "analysis_groups.csv", package = "Metaboseek"), stringsAsFactors = F),
                                     tablename = "large_example_features.csv",
                                     editable = F)
    }))}
  
  if(data){
    eval.parent(quote({
      rawgroups <- read.csv(system.file("extdata", "examples", "example_projectfolder", "filegroups.csv", package = "Metaboseek"), stringsAsFactors = F)
      

      MSD <- list(rawgrouptable = NULL,
                  index = "Group1",
                  rootfolder = getwd(),
                  localfolders = character(0),
                  RTcorr = NULL,
                  active = "Group1",
                  filelist = rawgroups$File,
                  data = loadRawM(rawgroups$File))
      
      MSD$MSnExp <- MSnbase::readMSData(MSD$filelist, pdata = NULL, verbose = F,
                                           centroided. = T,
                                           smoothed. = NA, mode = "onDisk")
      
      MSD$layouts <- list(Group1 = constructRawLayout(rawgrouptable = rawgroups, msnExp = MSD$MSnExp))
    }))
  }
}
