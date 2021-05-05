#' SiriusOptionsModule
#' 
#' Module to change Sirius analysis options
#' 
#' @inherit MseekModules
#' 
#' @describeIn SiriusOptionsModule Server logic
#' 
#' @importFrom shinyBS bsCollapse bsCollapsePanel updateCollapse
#' @importFrom data.table fread
#' 
#' @export 
SiriusOptionsModule <- function(input,output, session, values){
  
  ns <- NS(session$ns(NULL))
  
  SiriusFolder <- callModule(FilePathModule, "siriusFolder",
                             filepaths = reactive({values$GlobalOpts$filePaths}),
                             label = "Sirius Folder", description= "Select folder that contains the sirius executable (sirius-console-64.exe or sirius in linux/macOS)",
                             displayFolder = T)
  
  SiriusFolder$dir <- .MseekOptions$siriusFolder
  
  
  output$controlSirius <- renderUI({
    
    fluidRow(
      column(1,
             selectizeInput(ns("selDB"), "Database:",
                            choices = values$GlobalOpts$SiriusDBoptions,
                            selected = values$GlobalOpts$SiriusDBselected)),
      column(1,
             selectizeInput(ns("selIon"),
                            "Ion", 
                            choices = list("[M+?]+" = "[M+?]+",
                                           "[M+?]-" = "[M+?]-",
                                           "[M+H]+" = "[M+H]+",
                                           "[M+Na]+" = "[M+Na]+",
                                           "[M-H}-" = "[M-H]-",
                                           "[M+Cl]-" = "[M+Cl]-"), 
                            selected = values$GlobalOpts$SiriusSelIon)),
      column(1,
             checkboxInput(ns("checkFinger"), "Get FingerID",
                           value = values$GlobalOpts$SiriusCheckFinger)),
      column(1,
             checkboxInput(ns("useMS1"), "Use MS1 spectrum", 
                           value = values$GlobalOpts$SiriusUseMS1)),
      
      column(2,
             selectizeInput(ns("selInstrument"), "Instrument",
                            choices = list("Orbitrap" = "orbitrap", 
                                           "QTOF" = "qtof", 
                                           "FT-ICR" = "fticr"),
                            selected = values$GlobalOpts$SiriusSelInstrument)),
      column(2, 
             textInput(ns("elements"), "Allow elements:",
                       value = values$GlobalOpts$SiriusElements)),
      column(4,
             FilePathModuleUI(ns("siriusFolder")))
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
    MseekOptions(SiriusDBselected = input$selDB)
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
  
  observeEvent(SiriusFolder$dir,{
    if(length(SiriusFolder$dir) > 0 && !is.na(SiriusFolder$dir)){
      values$GlobalOpts$siriusFolder <- SiriusFolder$dir
      MseekOptions(siriusFolder=SiriusFolder$dir)
    }
  }, ignoreInit =T)
  
}

#' @describeIn SiriusOptionsModule UI elements
#' @export
SiriusOptionsModuleUI <- function(id){
  ns <- NS(id)
  htmlOutput(ns("controlSirius"))
  
}