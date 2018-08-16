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
#' @param tables load example tables?
#' @param data load example MS data?
#' 
#' @import shiny
#' @export
MosaicExamplePreload <- function(tables = T, data = T){
  if(tables){
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
  }))}
    
  if(data){
    eval.parent(quote({
    rawgroups <- read.csv(system.file("data", "tables", "filegroups.csv", package = "Mosaic"), stringsAsFactors = F)

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




# Define UI for dataset viewer app ----
#' MosaicMinimalUi
#'
#' A minimal UI for Mosaic that can be extended with additional objects for testing and development purposes
#'
#' @importFrom shinyjs runcodeUI 
#' @importFrom shinydashboard dashboardPage 
#' @importFrom shiny fluidPage verbatimTextOutput

#' 
#' @export
MosaicMinimalUI <- function(..., diagnostics = T, dashboard = F){
  
    if(!dashboard){
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
    fluidPage(
           ...,
              runcodeUI(code = "", type = c("text", "textarea", "ace"), width = NULL,
              height = NULL, includeShinyjs = FALSE),
    verbatimTextOutput('diag'))}
    else{
     ... 
    }
  )}
  else{
  if(diagnostics){
    fluidPage(
      dashboardPage(...),
      runcodeUI(code = "", type = c("text", "textarea", "ace"), width = NULL,
                height = NULL, includeShinyjs = FALSE),
      verbatimTextOutput('diag')
      )
    }
  else{
    dashboardPage(...) 
  }
  }
  
  }

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
                               rootfolder = c(root = default__root),
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
    
    featureTables <- reactiveValues(tables = list(table0 = constructFeatureTable(),
                                                            table1 = tab1,
                                                            table2 = tab2),
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


#' checkFolders
#'
#' Looks for folders 
#' 
#' @param query character vector with folders to search for, by default looks for drives in Windows file system
#'
#' @export
checkFolders <- function(query = paste0(LETTERS,":/")){
  
  out <- character(0)
  
  for(i in query){
    if(file.exists(i)){
      out[[gsub(":/","",i)]] = i
    }
  }
  
  return(out)
}

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
    
    if(!.MosaicOptions$serverMode && Sys.info()['sysname'] == "Windows"){
      .MosaicOptions$filePaths <<- c(examples = system.file("data", package = "Mosaic"), checkFolders())
    }  
    
  }
  
  newSettings <- list(...)
  
  for(i in names(newSettings)){
    
    .MosaicOptions[[i]] <<- newSettings[[i]]
    
  }
  
  if(gsub("/Mosaic","",system.file(package = "Mosaic")) %in% .libPaths()){
  write(jsonlite::serializeJSON(.MosaicOptions, pretty = T), file.path(system.file("config", package = "Mosaic"), "MosaicOptions.json"))
  }
  
}