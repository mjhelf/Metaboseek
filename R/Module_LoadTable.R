#' LoadTableModule
#' 
#' 
#' server module for loading Tables
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
LoadTableModule <- function(input,output, session,
                            values = reactiveValues(projectData = projectData,
                                                    featureTables = NULL),
                            static = list(tooltip = "Load",
                                          label = "Load",
                                          format = list(header = T,
                                                        sep = "\t",
                                                        quote = '"',
                                                        stringsAsFactors = F),
                                          pattern = "\\.tGrouping$")
){
  ns <- NS(session$ns(NULL))
  
  internalValues  <- reactiveValues(df = NULL,
                                    filename = NULL)
  
  output$loadTableButton <- renderUI({
    actionButton(ns("loadtable"), static$label, icon = icon("folder-open", lib = "font-awesome"))
  })
  
  Uploader <- callModule(UploadTableModule, "modalUpload",
                         static = list(title =  NULL,
                                       filetypes = NULL,
                                       format = list(header = T,
                                                     sep = "\t",
                                                     quote = '"',
                                                     stringsAsFactors = F))
  )
  
  observeEvent(input$loadtable,{
    
    if(!is.null(values$projectData$projectFolder)){
      
      temp <-  list.files(values$projectData$projectFolder, pattern = static$pattern, recursive =  T, full.names = T)
      
      print(temp)
      
      tl <- list()
      
      for(i in unique(dirname(temp))){
        tl[[i]] <- as.list(temp[dirname(temp) == i])
        names(tl[[i]]) <- basename(temp[dirname(temp) == i])
      }
      
      names(tl) <- file.path(basename(values$projectData$projectFolder), gsub(values$projectData$projectFolder, "", names(tl)))
      
      internalValues$fileSelection <- tl
      
    }
    
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
    
    if(!is.null(input$modalSelect)){
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
    else{
      showNotification(paste("Something went wrong!"), type = "error", duration = 10)
    }
  })
  
  return(internalValues)
  
}

#' simpleTableModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
LoadTableModuleUI <- function(id)
{
  ns <- NS(id)
  
  htmlOutput(ns("loadTableButton"))
  
}