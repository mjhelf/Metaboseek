#' SaveTableModule
#' 
#' 
#' server module for saving Tables
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @importFrom data.table fwrite
#' 
#' @export 
SaveTableModule <- function(input,output, session,
                            reactives = reactive({list(df = NULL,
                                                       filename = "table.csv")}),
                            values = reactiveValues(projectData = projectData,
                                                    featureTables = NULL,
                                                    MainTable = NULL),
                            static = list(tooltip = "Save",
                                          label = "Save",
                                          format = c("tsv", "csv"))
){
  ns <- NS(session$ns(NULL))
  
  output$saveTableButton <- renderUI({
    actionButton(ns("savetable"), static$label, icon = icon("floppy-save", lib = "glyphicon"))
  })
  
  observeEvent(input$savetable,{
    
    if(!is.null(values$featureTables)){
      TableUpdateChunk()}
    
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            p(strong("You can save this table in your project folder, or download it through your browser"))
          ),
          hr(),
          fluidRow(
            column(6, div(title = "Download table through Browser",     downloadButton(ns("modalDownload"),"Download"))),
            column(6, div( title = "Save directly to current projectFolder (only works if you are working in a project folder)", actionButton(ns("modalProjectFolder"), "Save locally")))
          )),
        title = "Save table",
        easyClose = T,
        fade = F,
        size = "s",
        footer = modalButton("Cancel") 
      ))
    
  })
  
  output$modalDownload <- downloadHandler(filename= function(){paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))}, 
                                          content = function(file){
                                            fwrite(if(is.null(values$featureTables)){reactives()$df}
                                                                               else{values$featureTables$tables[[values$featureTables$active]]$df[values$MainTable$order,]},
                                                                               file,
                                                                               sep = if(static$format =="tsv"){"\t"}else{","},
                                                                               quote = T,
                                                                               row.names = F
                                          )
                                            showNotification(paste("Downloading file: ", paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))), duration = 10)
                                            
                                            removeModal()
                                          },
                                          contentType = if(static$format =="tsv"){"text/tab-separated-values"}else{"text/comma-separated-values"})
  
  
  # observeEvent(output$modalDownload,{
  #   print(output$modalDownload)
  # 
  # })
  
  observeEvent(input$modalProjectFolder,{
    
    if(!is.null(values$projectData$projectFolder)){
      
      if(!dir.exists(dirname(file.path(values$projectData$projectFolder, file.path(dirname(reactives()$filename),paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))))))){
        dir.create(dirname(file.path(values$projectData$projectFolder, file.path(dirname(reactives()$filename),paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))))), recursive = T)
      }
      if(is.null(values$featureTables)){TableUpdateChunk()}
      
      fwrite(if(is.null(values$featureTables)){reactives()$df}
                  else{values$featureTables$tables[[values$featureTables$active]]$df[values$MainTable$order,]},
                  file.path(values$projectData$projectFolder, file.path(dirname(reactives()$filename),paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename)))),
                  sep = if(static$format =="tsv"){"\t"}else{","},
                  quote = T,
                  row.names = F
      )
      showNotification(paste("Table saved as: ", file.path(values$projectData$projectFolder, file.path(dirname(reactives()$filename),paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))))), duration = 10)
      removeModal()
      
    }
    else{
      showNotification(paste("You have to work in a Project Folder to save files this way!"), type = "error", duration = 10)
    }
  })
  
  
  
}

#' SaveTableModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
SaveTableModuleUI <- function(id)
{
  ns <- NS(id)
  
  htmlOutput(ns("saveTableButton"))
  
}