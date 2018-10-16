#' densplotModule
#' 
#' Plot density of a vector or matrix
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param mx 
#' @param heading
#' 
#' @export
densplotModule <- function(input, output, session, mx, heading = "Default"){
  
  mx2 <- reactive({if (input$log){return(log10(mx()))}else{
    return(mx())}
  })
  
  
  output$dplot <- renderPlot({if(!is.null(mx2())){
    densplot(mx2(), main = heading)}
  })
  output$info <- renderPrint({if(!is.null(mx2())){
    summary(as.vector(mx2()))}   
  })
}

#' densplotModuleUI
#' 
#' Plot density of a vector or matrix (UI)
#' 
#' @param id 
#' 
#' @export
densplotModuleUI <- function(id){
  ns <- NS(id)
  tagList(
    checkboxInput(ns('log'),'log10 scale'),
    plotOutput(ns('dplot')),
    verbatimTextOutput(ns('info'))
  )
  
}