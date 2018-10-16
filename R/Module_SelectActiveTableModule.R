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
                           values = reactiveValues(featureTables = featureTables)
){
  
  ns <- NS(session$ns(NULL))
  
  
  output$activeTable <- renderUI({
    selectizeInput(ns('activeTable'), 'Active Table', 
                   selected = values$featureTables$active, 
                   choices = values$featureTables$index,
                   multiple = FALSE)
  })  
  
  observeEvent(input$activeTable, { 
    
        if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
            if(featureTables$tables[[featureTables$active]]$editable){
                featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
            }else{
                featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"]
            }
        }
    
    
    values$featureTables$active <- input$activeTable
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