#' SelectActiveTableModule
#' 
#' 
#' server module for loading a Project Folder
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
SelectActiveTableModule <- function(input,output, session,
                           values = reactiveValues(featureTables = featureTables,
                                                   MainTable = MainTable)
){
  
  ns <- NS(session$ns(NULL))
  
  
  output$activeTable <- renderUI({
    selectizeInput(ns('activeTable'), 'Active Table', 
                   selected = values$featureTables$active, 
                   choices = values$featureTables$index,
                   multiple = FALSE)
  })  
  
  observeEvent(input$activeTable, { 
    
    TableUpdateChunk()
    
    values$featureTables$tableSwitch <- T
    values$featureTables$active <- input$activeTable
    values$featureTables$row_filters <- TRUE
    
  })
  
}

#' SelectActiveTableModuleUI
#' 
#' @param id id of the Module
#' 
#' @export 
SelectActiveTableModuleUI <- function(id){
  
  ns <- NS(id)
 
  htmlOutput(ns("activeTable")) 
  
}