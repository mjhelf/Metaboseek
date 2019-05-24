#' MultiEICmodule
#' 
#' 
#' server module for interactive EIC view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values Import data from the shiny session
#' 
#' @export 
MultiEICmodule <- function(input, output, session, 
                           values = reactiveValues(MSData = MSData,
                                                   GlobalOpts = GlobalOpts,
                                                   featureTables = featureTables)){
  
  ns <- NS(session$ns(NULL))
  
  
  
  iEIC1 <- callModule(EICmodule,"EIC1", values = values,
                      keys = reactive({values$GlobalOpts$keyinput.keydown})
  )
  
  
  
  iEIC1$removable <- T
  
  internalValues <- reactiveValues(numEICs = 1,
                                   EIC1 = iEIC1,
                                   currentView = NULL
  )
  #### ** REMOVE BUTTON ####
  output$adder <- renderUI({
    actionButton(ns('addOne'), "Add EIC")
  })
  
  observeEvent(input$addOne,{
   
    internalValues$numEICs <- internalValues$numEICs + 1
    
    internalValues[[paste0("EIC", internalValues$numEICs)]] <- callModule(EICmodule,
                                                                             paste0("EIC", internalValues$numEICs),
                                                                             values = values,
                                                                          keys = reactive({values$GlobalOpts$keyinput.keydown}))
    
    internalValues[[paste0("EIC", internalValues$numEICs)]]$removable <- T
  })
  
  
  observeEvent(c(internalValues$EIC1$controls$marker),{
    
    internalValues$currentView  <- internalValues$EIC1
    
  })
  
  observeEvent(c(internalValues$EIC2$controls$marker),{
    
    internalValues$currentView  <- internalValues$EIC2
    
  })
  
  output$eicUIs <- renderUI({
    lapply(seq(internalValues$numEICs), function(i){
      EICmoduleUI(ns(paste0("EIC",i)))
    })
    })
  
  return(internalValues)
  
}

#' MultiEICmoduleUI
#' 
#' 
#' UI module for interactive EIC view
#' 
#' @param id id to be used in ns()
#' 
#' @export 
MultiEICmoduleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    
    fluidRow(
      htmlOutput(ns('adder'))
    ),
    htmlOutput(ns("eicUIs"))
  )
}