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
#' @importFrom splashR getSplash
#' @importFrom data.table fread
#' 
#' @export 
SiriusOptionsModule <- function(input,output, session, 
                         values = reactiveValues(
                           GlobalOpts = GlobalOpts)){

  ns <- NS(session$ns(NULL))
  
  
output$controlSirius <- renderUI({
 
           fluidRow(
             # column(3,
             #        selectizeInput(ns("selCharge"), "Charge:", choices = list("+1" = 1, "-1" = -1), selected = values$GlobalOpts$selCharge)),
             column(3,
                    selectizeInput(ns("selIon"), "Ion", choices = list("[M+?]+" = "[M+?]+",
                                                                       "[M+?]-" = "[M+?]-",
                                                                       "[M+H]+" = "[M+H]+",
                                                                       "[M+Na]+" = "[M+Na]+",
                                                                       "[M-H}-" = "[M-H]-",
                                                                       "[M+Cl]-" = "[M+Cl]-"), selected = values$GlobalOpts$SiriusSelIon)),
             column(3,
                    checkboxInput(ns("checkFinger"), "Get FingerID", value = values$GlobalOpts$SiriusCheckFinger)),
           
             column(3,
                    selectizeInput(ns("selInstrument"), "Instrument", choices = list("Orbitrap" = "orbitrap", "QTOF" = "qtof", "FT-ICR" = "fticr"), selected = values$GlobalOpts$SiriusSelInstrument)),
             column(3, 
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