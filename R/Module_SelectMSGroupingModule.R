#' SelectMSGroupingModule
#' 
#' Module to set up MS data file grouping for plotting
#' 
#' @inherit MseekModules
#' 
#' @return Returns its internalValues
#' 
#' @describeIn SelectMSGroupingModule Server logic
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

#' @describeIn SelectMSGroupingModule UI elements
#' @export 
SelectMSGroupingModuleUI <- function(id){
  
  ns <- NS(id)
 
  htmlOutput(ns("activeTable")) 
  
}