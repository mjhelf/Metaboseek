#' RtCorrViewModule
#' 
#' Module to plot retention time correction results
#' 
#' @inherit MseekModules
#' 
#' @return Returns nothing
#' 
#' @describeIn RtCorrViewModule Server logic
#' 
#' @export 
RtCorrViewModule <- function(input,output, session, values){
  
  ns <- NS(session$ns(NULL))
  
  observeEvent(input$RtCorrLoad$datapath,{
  
      tryCatch({    
      if(grepl("\\.[Rr][Dd][Ss]$", input$RtCorrLoad$datapath)){
      values$MSData$RTcorr <- readRDS(file.path(values$projectData$projectFolder, 
                                                "RTcorr_data.Rds"))
      }else{
    values$MSData$RTcorr <- attach(input$RtCorrLoad$datapath)$rtx
      }
    for(i in 1:length(values$MSData$RTcorr$noncorr)){
      
      values$MSData$RTcorr[["rtdiff"]][[i]] <- values$MSData$RTcorr$noncorr[[i]]-values$MSData$RTcorr$corr[[i]]
      
    }
    },
  error = function(e){
      
      showNotification(paste("ERROR: Loading of RT correction information failed:", e), type = "error", duration = NULL)
      
      
      })
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

#' @describeIn RtCorrViewModule UI elements
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