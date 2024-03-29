#' UploadTableModule
#' 
#' Module for loading Tables into the app via \code{\link[shiny]{fileInput}()}
#' 
#' @inherit MseekModules
#' @describeIn UploadTableModule Server logic
#' 
#' @return Returns its internalValues
#' 
#' @export 
UploadTableModule <- function(input,output, session,
                              values = reactiveValues(featureTables = NULL),
                              static = list(title =  NULL,
                                            filetypes = NULL,
                                            format = list(header = T,
                                                          sep = "\t",
                                                          quote = '"',
                                                          stringsAsFactors = F)
                              )
){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(df = NULL,
                                   filename = NULL
  )
  
  #When Load Table button is pressed, initialize internalValues with defaults
  observeEvent(input$file1$datapath,
               {
                 tryCatch({
                   
                   #handling loading of a featureTable
                   if(!is.null(values$featureTables)){
                     
                     withProgress(message = 'Please wait!', detail = "Importing Feature Table", value = 0.6, {
                     
                         
                     # values$featureTables$tables[[values$featureTables$active]] <- updateFTgrouping(values$featureTables$tables[[values$featureTables$active]],
                     #                                                                                NULL)
                                          tabid <- paste0("table",length(values$featureTables$tables))
                            
                                          if(grepl('\\.[Mm][Ss][Kk][Ff][Tt]$',input$file1$datapath)[1]){
                                              
                                              values$featureTables$tables[[tabid]] <- loadMseekFT(input$file1$datapath)
                                              
                                              }else{
                                          
                                          
                                          
                                          feats <- as.data.frame(data.table::fread(input$file1$datapath,
                                                                     header = static$format$header,
                                                                     stringsAsFactors = static$format$stringsAsFactors,
                                                                     quote = static$format$quote,
                                                                     sep = if(!is.null(static$format$sep)){static$format$sep}else{input$sep}),
                                                                 stringsAsFactors = static$format$stringsAsFactors)
                                          
                                          # for all columns that are of type logical and only contain NAs, assume they are mutilated empty character strings
                                          # and a victim of type.convert - make them character vectors again
                                          charCols <- sapply(feats,typeof) == "logical" & sapply(lapply(feats,is.na),all)
                                          if(any(charCols)){
                                          feats[,charCols] <- character(nrow(feats))
                                          }
                                          
                                          intColRange <- grep("__XIC$",colnames(feats))
                                          
                                          if(length(intColRange)==0){
                                            
                                            anagroup <- NULL
                                            
                                            
                                          }else{
                                            
                                            anagroup <- data.frame(Column=colnames(feats)[intColRange],
                                                                   Group = rep("G1",(length(intColRange))),
                                                                   stringsAsFactors = F)
                                          }
                                          incProgress(0.3, detail = "Formatting Feature Table")
                                          
                     values$featureTables$tables[[tabid]] <- buildMseekFT(feats,
                                                                            mzcol= "mz", #column in df with mz values (columnname)
                                                                            rtcol= "rt", #column in df with mz values (columnname)
                                                                            commentcol = "comments",
                                                                            fragmentcol = "fragments",
                                                                            rtFormat = "sec", # "sec" or "min" 
                                                                            anagrouptable = anagroup,
                                                                            tablename = input$file1$name,
                                                                            editable = F)
                     
                                              }
                     
                     values$featureTables$index <- updateFTIndex(values$featureTables$tables)
                     values$featureTables$active <- tabid
                     })
                     removeModal()
                   }
                   #loading other Tables (expected to be smaller files)
                   else{
                   
                   internalValues$df <- read.delim(input$file1$datapath,
                                                   header = if(is.null(static$format$header)){input$header}else{static$format$header},
                                                   sep = if(is.null(static$format$sep)){input$sep}else{static$format$sep}, 
                                                   quote = if(is.null(static$format$quote)){input$quote}else{static$format$quote},
                                                   stringsAsFactors = if(is.null(static$format$stringsAsFactors)){F}else{static$format$stringsAsFactors})
                   
                   internalValues$filename <- input$file1$name #gsub("\\.[^.]*$","",input$file1$name)
                   }
                 },
                 error = function(e){
                   showNotification(paste("ERROR: Make sure you selected the correct Table Options (tab or comma separated?) for your input table."), type = "error", duration = 10)
                   
                 })
                 
                 
                 
               })  
  
 
  output$tableLoad <- renderUI({
    
    fluidRow(
      # checkboxInput(ns('toggleTabOpts'), "Show table Options", value = F),
      
      if(is.null(static$format$header)){
        checkboxInput(ns('header'), 'Header', TRUE)},
      
      if(is.null(static$format$sep)){
        radioButtons(ns('sep'), 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ',')},
      
      if(is.null(static$format$quote)){
        radioButtons(ns('quote'), 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')},
      
      
      fileInput(ns('file1'), if(is.null(static$title)){'Choose File'}
                else{static$title},
                accept=static$filetypes)
      
    )
    
  })
  
  output$tableInfo <- renderUI({
    fluidRow(
      if(!is.null(internalValues$df)){
        p("Loaded a table with", nrow(internalValues$df), "rows and", ncol(internalValues$df), "columns.")
      }else{NULL}
    )
  })
  
  return(internalValues)
  
}

#' @describeIn UploadTableModule UI elements
#' @export
UploadTableModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns('tableLoad')),
    htmlOutput(ns('tableInfo'))
  )
}