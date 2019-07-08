#' GetSiriusWidget
#' 
#' Widget that starts a SIRIUS analysis as specified in reactives({}, which will 
#' add an entry to the index file in the SIRIUS folder to be observed by the SiriusModule
#' 
#' @inherit MseekWidgets
#' 
#' @param reactives a reactive() returning a list of \code{\link{runSirius}()}
#'  parameters
#' 
#' @describeIn GetSiriusWidget server logic
#' 
#' @export 
GetSiriusWidget <- function(input,output, session, 
                          reactives = reactive({
                            list(outfolder =  "METABOseek",
                                 ms2 = NULL,
                                 instrument = "orbitrap",
                                 parentmz = NULL,
                                 rt = NULL,
                                 comments = NULL,
                                 ion = "[M+H]+",
                                 charge= 1,
                                 fingerid = T,
                                 scanindices = NULL,
                                 sirpath = "sirius-console-64",
                                 moreOpts = "")
                          })
){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(siriusIndex = NULL)
  
  observeEvent(input$getSirius,{
    if(!is.null(reactives())){
    tryCatch({
  do.call(runSirius, reactives())
    }, error = function(e){
      print(e)
      showNotification(paste("A problem occured and SIRIUS search failed"), type = "error", duration = 0)
      
    })
      
      }
      
  })
  output$getsiriusbutton <- renderUI({
    
    actionButton(ns("getSirius"), "Run SIRIUS", title = "Run SIRIUS with current settings for the selected spectrum or averaged spectra.")
    
  })
}

#' @describeIn GetSiriusWidget UI elements
#' @export
GetSiriusWidgetUI <- function(id){
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns("getsiriusbutton"))
  )
  
}