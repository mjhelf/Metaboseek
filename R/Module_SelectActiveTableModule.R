#' SelectActiveTableModule
#' 
#' Module to select the active Feature Table
#' 
#' @inherit MseekModules
#' 
#' @return Returns nothing
#' 
#' @describeIn SelectActiveTableModule Server logic
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

#' @describeIn SelectActiveTableModule UI elements
#' @export 
SelectActiveTableModuleUI <- function(id){
  
  ns <- NS(id)
 
  htmlOutput(ns("activeTable")) 
  
}