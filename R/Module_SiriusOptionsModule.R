#' SiriusOptionsModule
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' 
#' @importFrom shinyBS bsCollapse bsCollapsePanel updateCollapse
#' @importFrom data.table fread
#' 
#' @export 
SiriusOptionsModule <- function(input,output, session, 
                         values = reactiveValues(
                           GlobalOpts = GlobalOpts)){

  ns <- NS(session$ns(NULL))
  
  
output$controlSirius <- renderUI({
 
           fluidRow(
             column(3,
                    selectizeInput(ns("selDB"), "Database:", choices = values$GlobalOpts$SiriusDBoptions, selected = values$GlobalOpts$SiriusDBselected)),
             column(2,
                    selectizeInput(ns("selIon"), "Ion", choices = list("[M+?]+" = "[M+?]+",
                                                                       "[M+?]-" = "[M+?]-",
                                                                       "[M+H]+" = "[M+H]+",
                                                                       "[M+Na]+" = "[M+Na]+",
                                                                       "[M-H}-" = "[M-H]-",
                                                                       "[M+Cl]-" = "[M+Cl]-"), selected = values$GlobalOpts$SiriusSelIon)),
             column(2,
                    checkboxInput(ns("checkFinger"), "Get FingerID", value = values$GlobalOpts$SiriusCheckFinger)),
             column(2,
                    checkboxInput(ns("useMS1"), "Use MS1 spectrum", value = values$GlobalOpts$SiriusUseMS1)),
           
             column(2,
                    selectizeInput(ns("selInstrument"), "Instrument", choices = list("Orbitrap" = "orbitrap", "QTOF" = "qtof", "FT-ICR" = "fticr"), selected = values$GlobalOpts$SiriusSelInstrument)),
             column(2, 
                    textInput(ns("elements"), "Allow elements:", value = values$GlobalOpts$SiriusElements))
    )
})


# observeEvent(input$selCharge,{
#   
#   values$GlobalOpts$selCharge <- input$selCharge
#   
# })
observeEvent(input$selIon,{
  
  values$GlobalOpts$SiriusSelIon <- input$selIon
  MseekOptions(SiriusSelIon = input$selIon)
})

observeEvent(input$selDB,{
  
  values$GlobalOpts$SiriusDBselected <- input$selDB
  MseekOptions(SiriusDBselected = input$SiriusDBselected)
})

observeEvent(input$useMS1,{
  
  values$GlobalOpts$SiriusUseMS1 <- input$useMS1
 # MseekOptions(SiriusElements = input$elements)
})


observeEvent(input$checkFinger,{
  
  values$GlobalOpts$SiriusCheckFinger <- input$checkFinger
  MseekOptions(SiriusCheckFinger = input$checkFinger)
})
observeEvent(input$selInstrument,{
  
  values$GlobalOpts$SiriusSelInstrument <- input$selInstrument
  MseekOptions(SiriusSelInstrument = input$selInstrument)
})
observeEvent(input$elements,{
  
  values$GlobalOpts$SiriusElements <- input$elements
  MseekOptions(SiriusElements = input$elements)
})
}

#' SiriusOptionsModuleUI
#' 
#' 
#' UI module for interactive SIRIUS interface
#' 
#' @param id id to be used in ns()
#' 
#' @export
SiriusOptionsModuleUI <- function(id){
  ns <- NS(id)
  htmlOutput(ns("controlSirius"))
  
}