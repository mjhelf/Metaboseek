#' PlotModule
#' 
#' 
#' server module for ggplot2 handler
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' 
#' @import shiny ggplot2
#' @importFrom shinyjs hideElement showElement toggle
#' @importFrom plotly renderPlotly
#' 
#' @export 
PlotModule <- function(input,output, session,
                       reactives = reactive({list(reactiveValues(plot = p,
                                                                 interactive = F))})
){
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(interactive = F)
  
  # observeEvent(c(reactives()$interactive,
  #                reactives()$plot),{
  #    toggle(id = 'fplot', condition = !is.null(reactives()$plot) && !reactives()$interactive)
  #   toggle(id = 'iplot', condition = !is.null(reactives()$plot) && reactives()$interactive)
  #   
  # }, ignoreNULL = F)
  
  output$fplot <- renderPlot({
    
    
    if(!is.null(reactives()$plot) && !is.null(reactives()$interactive) && !reactives()$interactive){
      hideElement(id = 'iplot')
      
      reactives()$plot
    }
    else{
      showElement(id = 'iplot')
      NULL
    }
    
  })
  
  output$iplot <- renderPlotly({
    if(!is.null(reactives()$plot) && !is.null(reactives()$interactive) && reactives()$interactive){
      hideElement(id = 'fplot')
      
      reactives()$plot
      
    }
    else{
      showElement(id = 'fplot')
      
      NULL
    }
  })
  
  observe({
    
    
    
  })
  
  
  
}

#' PCABrowserModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @import shiny
#' @importFrom plotly plotlyOutput
#' 
#' @export
PlotModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      plotOutput(ns('fplot'), height = "550px"),
      plotlyOutput(ns('iplot'), height = "550px")
      
    )
  )
  
  
}