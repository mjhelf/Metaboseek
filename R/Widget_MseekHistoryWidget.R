#' MseekHistoryWidget
#' 
#' One-button module to display and export the process history of MseekFT objects.
#' 
#' @inherit MseekWidgets
#' @param FT an \code{\link{MseekFT}} object inside a \code{reactive()}
#' @param buttonLabel text on the button that opens the history modal dialog
#' 
#' @describeIn MseekHistoryWidget server logic
#' 
#' @export 
MseekHistoryWidget <- function(input,output, session,
                           FT = reactive({NULL}),
                           buttonLabel = "History"){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  

  output$historyPrint <- renderPrint({
      if(is.MseekFamily(FT())){
          
          if(is.null(input$selectOutput)
             || input$selectOutput == "All entries"){
              print(processHistory(FT()))
          }
          else if(input$selectOutput == "No errors"){
              print(processHistory(FT())[!sapply(processHistory(FT()),
                                                 hasError)])
          }
          
          else if(input$selectOutput == "only Errors"){
              print(processHistory(FT())[sapply(processHistory(FT()),
                                                hasError)])
          }
          
      }else{
          print("No Feature Table loaded")
      }
      
      })
  
  dialog <- callModule(ModalWidget, "getbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                         fluidRow(selectizeInput(ns("selectOutput"), "Show", choices = c("All entries",
                                                                                         "No errors",
                                                                                         "only Errors")),
                                  downloadButton(ns("downloadHistory"), "Save history")),
                             
                           fluidRow(
                             verbatimTextOutput(ns("historyPrint"))                           
                             )#,

                           # fluidRow(actionButton(ns("getIntensities"), "Go")
                           #   )
                           )
                           
                         )}),
                       static = list(tooltip = "Show process History for this Feature Table",
                                     title = "Process History", 
                                     label = buttonLabel,
                                     icon = icon("history", lib = "font-awesome")))
  
                       output$downloadHistory <- downloadHandler(filename= function(){
                           paste0(FT()$tablename,"_processHistory.txt")
                       }, 
                       content = function(file){
                           tryCatch({
                             
                               showNotification(paste("Downloading file: ", 
                                                      paste0(FT()$tablename,"_processHistory.txt")),
                                                #paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))),
                                                duration = 10)
                               
                               removeModal()
                               
                           },
                           error = function(e){
                               writeLines(paste("ERROR: Process History was NOT saved: ",e),file)
                               showNotification(paste("ERROR:  Process History was NOT saved. The downloaded file is EMPTY and only serves to prevent the session from closing in Firefox. Error message: ",e), type = "error", duration = NULL)
                               
                           })
                       },
                       contentType = "text/plain")
  
  
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
MseekHistoryWidgetUI <- function(id)
{
  ns <- NS(id)
  
  ModalWidgetUI(ns("getbutton"))
  
}