#' RtCorrViewModule
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
RtCorrViewModule <- function(input,output, session,
                             values = reactiveValues(MSData = MSData,
                                                     GlobalOpts = GlobalOpts)
){
  
  ns <- NS(session$ns(NULL))
  
  observeEvent(input$RtCorrLoad$datapath,{
    values$MSData$RTcorr <- attach(input$RtCorrLoad$datapath)$rtx
    
    for(i in 1:length(values$MSData$RTcorr$noncorr)){
      
      values$MSData$RTcorr[["rtdiff"]][[i]] <- values$MSData$RTcorr$noncorr[[i]]-values$MSData$RTcorr$corr[[i]]
      
    }
    
  })
  
  output$rtcorr <- renderPlot({
    if(!is.null(values$MSData$RTcorr)){
      RTplot(values$MSData$RTcorr,
             colscheme = values$GlobalOpts$colorscheme,
             liwi =  2* values$GlobalOpts$plotLw,
             cx = values$GlobalOpts$plotCx)
    }
    
  })
  
}

#' RtCorrViewModuleUI
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param id
#' 
#' @export
RtCorrViewModuleUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
  fluidRow(
    
    column(4,
           fileInput(ns('RtCorrLoad'), "Load Retention time correction data",
                     accept= NULL
           )
    ),
    
    column(8,
           plotOutput(ns("rtcorr"))
    ))
  )
}