#' FTHistoryWidget
#' 
#' One-button module to display and export the process history of MseekFT objects.
#' 
#' @inherit MseekWidgets
#' @param FT an \code{\link{MseekFT}} object inside a \code{reactive()}
#' 
#' @describeIn FTHistoryWidget server logic
#' 
#' @export 
FTHistoryWidget <- function(input,output, session,
                           FT = reactive({NULL})){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  

  output$historyPrint <- renderPrint({if(!is.null(FT())){
      print(processHistory(FT()))
      
  }else{
      print("No Feature Table loaded")
      }
      
      })
  
  dialog <- callModule(ModalWidget, "getbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                         
                           fluidRow(
                             verbatimTextOutput(ns("historyPrint"))                           
                             )#,
                           
                           # fluidRow(actionButton(ns("getIntensities"), "Go")
                           #   )
                           )
                           
                         )}),
                       static = list(tooltip = "Show process History for this Feature Table",
                                     title = "Process History", 
                                     label = "History",
                                     icon = icon("history", lib = "font-awesome")))
  
  
  
  
  # observeEvent(input$getIntensities,{
  #   tryCatch({
  #     }
  #     ,
  #     error = function(e){
  #       
  #       showNotification(paste("An error occured: ", e), duration = 0, type = "error")
  #       
  #       
  #     })
  #   
  # })
  
  
  return(internalValues)
}

#' @describeIn GetIntensitiesModule server logic
#' @export
FTHistoryWidgetUI <- function(id)
{
  ns <- NS(id)
  
  ModalWidgetUI(ns("getbutton"))
  
}