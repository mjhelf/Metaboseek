#' LoadTableModule
#' 
#' Module for loading Tables
#' 
#' @inherit MseekModules
#' 
#' @return returns its internalValues, most notably element \code{df} that contains 
#' the table once it is loaded, and \code{filename}, which contains the basename 
#' of the loaded table file
#' 
#' @param static see \code{Details}
#' @param ... arguments passed to UI elements for the "Load" button.
#' 
#' @details elements in \code{static} that can be set:
#' \itemize{
#' \item \code{tooltip} tooltip on button opening the load table menu
#' \item \code{label} label on button opening the load table menu
#' \item \code{format} loading options \code{header}, \code{sep},
#'  \code{quote}, \code{stringsAsFactors} to be passed to \code{data.table::fread}
#' \item \code{pattern} file name pattern to restrict file display to
#' }
#' 
#' @describeIn LoadTableModule server logic
#' 
#' @export 
LoadTableModule <- function(input,output, session,
                            values = reactiveValues(projectData = NULL,
                                                    featureTables = NULL),
                            static = list(tooltip = "Load",
                                          label = "Load",
                                          format = list(header = T,
                                                        sep = "\t",
                                                        quote = '"',
                                                        stringsAsFactors = F),
                                          pattern = "\\.tGrouping$"),
                            ...
){
  ns <- NS(session$ns(NULL))
  
  internalValues  <- reactiveValues(df = NULL,
                                    filename = NULL)
  
  output$loadTableButton <- renderUI({
    actionButton(ns("loadtable"), static$label, icon = icon("folder-open", lib = "font-awesome"), ...)
  })
  
  Uploader <- callModule(UploadTableModule, "modalUpload",
                         values = reactiveValues(featureTables = values$featureTables),
                         static = list(title =  NULL,
                                       filetypes = NULL,
                                       format = list(header = static$format$header,
                                                     sep = static$format$sep,
                                                     quote = static$format$quote,
                                                     stringsAsFactors = static$format$stringsAsFactors))
  )
  
  observeEvent(input$loadtable,{
    
    if(!is.null(values$projectData$projectFolder)){
      
      temp <-  list.files(values$projectData$projectFolder, pattern = static$pattern, recursive =  T, full.names = T)
      
      tl <- list()
      
      for(i in unique(dirname(temp))){
        tl[[i]] <- as.list(temp[dirname(temp) == i])
        names(tl[[i]]) <- basename(temp[dirname(temp) == i])
      }
      
      names(tl) <- file.path(basename(values$projectData$projectFolder), gsub(values$projectData$projectFolder, "", names(tl)))
      
      internalValues$fileSelection <- tl
      
    }
    removeModal()
    
    
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            p(strong("You load a table from your project folder, or upload it through your browser"))
          ),
          hr(),
          fluidRow(
            column(6, div(title = "Upload table through Browser", UploadTableModuleUI(ns("modalUpload")))),
            column(6, div( title = "Load directly from the current projectFolder (only works if you are working in a project folder)",
                           if(is.null(values$projectData$projectFolder)){
                             p("No Project Folder selected")
                           }
                           else if(length(internalValues$fileSelection) == 0){p("No tables of this type in the Project Folder.")}
                           else{
                             fluidRow(
                               selectizeInput( ns("modalSelect"), "Select table to load:", choices = internalValues$fileSelection),
                               actionButton(ns("modalProjectFolder"), "Load table")
                             )
                           }
            ))
          )),
        title = "Load a table",
        easyClose = T,
        fade = F,
        size = "m",
        footer = modalButton("Cancel") 
      ))
    
  })
  
  
  
  
  observeEvent(Uploader$df,{
    internalValues$df <- Uploader$df
    internalValues$filename <- Uploader$filename
    
    
    removeModal()
    showNotification(paste("Table loaded: ", internalValues$filename), duration = 10)
    
    
  })
  
  observeEvent(input$modalProjectFolder,{
    tryCatch({   
      if(!is.null(input$modalSelect)){

    if(!is.null(values$featureTables)){
      
      withProgress(message = 'Please wait!', detail = "Importing Feature Table", value = 0.6, {
        
        tabid <- paste0("table",length(values$featureTables$tables))
        
        if(grepl('\\.[Mm][Ss][Kk][Ff][Tt]$',input$modalSelect)[1]){
            
            values$featureTables$tables[[tabid]] <- loadMseekFT(input$modalSelect)
            
        }else{
        
        feats <- as.data.frame(data.table::fread(input$modalSelect,
                                                 header = static$format$header,
                                                 stringsAsFactors = static$format$stringsAsFactors,
                                                 quote = static$format$quote,
                                                 sep = ","),
                               stringsAsFactors = static$format$stringsAsFactors)
        # for all columns that are of type logical and only contain NAs, assume they are mutilated empty character strings
        # and a victim of type.convert - make them character vectors again
        charCols <- sapply(feats,typeof) == "logical" & sapply(lapply(feats,is.na),all)
        if(any(charCols)){
          feats[,charCols] <- character(nrow(feats))
        }
        
        intColRange <- grep("__XIC$",colnames(feats))
        #look for a .tGrouping file in same folder as table and load it
         if(file.exists(gsub("\\.csv$",".tGrouping",input$modalSelect))){
           anagroup <-read.delim(gsub("\\.csv$",".tGrouping",input$modalSelect), stringsAsFactors = F, header = T, sep = "\t")
         }
        #look for a .tGrouping file for this table in the entire project folder and load the first match
        else if(length(list.files(values$projectData$projectFolder, pattern = gsub("\\.csv$",".tGrouping",basename(input$modalSelect)), recursive = T, full.names = T)) > 0){
          anagroup <- read.delim(list.files(values$projectData$projectFolder, pattern = gsub("\\.csv$",".tGrouping",basename(input$modalSelect)), recursive = T, full.names = T)[1], stringsAsFactors = F, header = T, sep = "\t")
        }
        #no __XIC columns? No automatic grouping suggestion
        else if(length(intColRange)==0){
          
          anagroup <- NULL
        
        }
        
        else{
          
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
                                                                      tablename = basename(input$modalSelect),
                                                                      editable = F)
        }
        
        values$featureTables$index <- updateFTIndex(values$featureTables$tables)
        values$featureTables$active <- tabid
      })
      removeModal()
    }
    #loading other Tables (expected to be smaller files)
    else{
    
      withProgress({
        
        internalValues$df <- read.delim(input$modalSelect,
                                        header = static$format$header,
                                        sep = static$format$sep, 
                                        quote = static$format$quote,
                                        stringsAsFactors = static$format$stringsAsFactors)
        
        internalValues$filename <- basename(input$modalSelect) #gsub("\\.[^.]*$","",input$file1$name)
        removeModal()  
        
      }, message = "loading Table", value = 0.5)
      showNotification(paste("Table loaded: ", internalValues$filename), duration = 10)
      
      
    }

    } 
    else{
      showNotification(paste("No Table selected!"), type = "error", duration = 10)
    }
    },
    error = function(e){
      showNotification(paste("ERROR: Make sure you selected the correct Table Options (tab or comma separated?) for your input table."), type = "error", duration = 10)
      
    })
    
    
  })
  
  return(internalValues)
  
}

#' @describeIn LoadTableModule UI elements
#' @export
LoadTableModuleUI <- function(id, ...)
{
  ns <- NS(id)
  
  htmlOutput(ns("loadTableButton"), ...)
  
}