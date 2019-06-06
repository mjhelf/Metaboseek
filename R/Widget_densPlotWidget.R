#' densplotWidget
#' 
#' Plot density of a vector or matrix
#' 
#' @inherit MseekWidget
#' @param reactives list of arguments passed to \code{\link{densplot}}
#' 
#' @describeIn densPlotWidget server logic for densplotWidget
#' @export
densplotWidget <- function(input, output, session,
                           reactives = reactive({list()})){
  
    internalValues <- reactiveValues(plotArgs = NULL)
    
  observe({
      if (input$log){
          tempargs <- reactives()
          tempargs$densin <- log10(reactives()$densin)
          internalValues$plotArgs <- tempargs
          }else{
              internalValues$plotArgs <- reactives()
              }
  })
  
  
  output$dplot <- renderPlot({
      if(length(internalValues$plotArgs)){
    do.call(densplot, internalValues$plotArgs)
          }
  })
  output$info <- renderPrint({
      if(length(internalValues$plotArgs)){
          summary(as.vector(internalValues$plotArgs$densin))
          }   
  })
}

#' @describeIn densPlotWidget UI elements for densplotWidget
#' @export
densplotModuleUI <- function(id){
  ns <- NS(id)
  tagList(
    checkboxInput(ns('log'),'log10 scale'),
    plotOutput(ns('dplot')),
    verbatimTextOutput(ns('info'))
  )
  
}