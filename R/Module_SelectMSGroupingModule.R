#' SelectMSGroupingModule
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
SelectMSGroupingModule <- function(input,output, session,
                           values = reactiveValues(MSData = MSData),
                           static = list(editOnly = F)
){
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(MSGrouptable = NULL,
                                   active = NULL)
  
  output$activeTable <- renderUI({
    if(is.null(values$MSData$layouts)){
      p("No MS data loaded")
    }else{
    selectizeInput(ns('activegrouping'), 'Active MS Grouping', 
                   selected = if(static$editOnly){internalValues$active}else{values$MSData$active}, 
                   choices = names(values$MSData$layouts),
                   multiple = FALSE)
    }
  })  
  
  observeEvent(input$activegrouping, { 
    if(static$editOnly){
    internalValues$active <- input$activegrouping
    }else{
    values$MSData$active <- input$activegrouping
}
  })
  
  return(internalValues)
}

#' SelectMSGroupingModuleUI
#' 
#' @param id id of the Module
#' 
#' @export 
SelectMSGroupingModuleUI <- function(id){
  
  ns <- NS(id)
 
  htmlOutput(ns("activeTable")) 
  
}