#' GetSiriusModule
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' 
#' 
#' @export 
GetSiriusModule <- function(input,output, session, 
                          values = reactiveValues(),
                          reactives = reactive({
                            list(outfolder =  file.path(values$GlobalOpts$siriusFolder,"METABOseek"),
                                 ms2 = MergedSpecs,
                                 instrument = "orbitrap",
                                 parentmz = reactives()$mz,
                                 rt = reactives()$rt,
                                 comments = reactives()$comments,
                                 ion = "[M+H]+",
                                 charge= 1,
                                 fingerid = T,
                                 scanindices = reactives()$scanlist,
                                 sirpath = file.path(values$GlobalOpts$siriusFolder,"sirius-console-64.exe"),
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

#' GetSiriusModuleUI
#' 
#' 
#' UI module for interactive SIRIUS interface
#' 
#' @param id id to be used in ns()
#' 
#' @export
GetSiriusModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns("getsiriusbutton"))
  )
  
}