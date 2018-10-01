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
                           values = reactiveValues(MSData = MSData),
                           keys){
  
  ns <- NS(session$ns(NULL))
  
  
  
  iEIC1 <- callModule(EICmodule,"EIC1", values = values,
                      keys = keys
  )
  
  iEIC2 <- callModule(EICmodule,"EIC2", values = values,
                      keys =keys)
  
  iEIC3 <- callModule(EICmodule,"EIC3", values = values,
                      keys = keys)
  
  iEIC4 <- callModule(EICmodule,"EIC4", values = values,
                      keys =keys)
  
  iEIC5 <- callModule(EICmodule,"EIC5", values = values,
                      keys =keys)
  
  iEIC6 <- callModule(EICmodule,"EIC6", values = values,
                      keys = keys)
  
  iEIC1$removable <- T
  iEIC2$removable <- T
  iEIC3$removable <- T
  iEIC4$removable <- T
  iEIC5$removable <- T
  iEIC6$removable <- T
  
  iEIC2$active <- F
  iEIC3$active <- F
  iEIC4$active <- F
  iEIC5$active <- F
  iEIC6$active <- F
  
  
  internalValues <- reactiveValues(EIC1 = iEIC1,
                                   EIC2 = iEIC2,
                                   EIC3 = iEIC3,
                                   EIC4 = iEIC4,
                                   EIC5 = iEIC5,
                                   EIC6 = iEIC6,
                                   currentView = NULL,
                                   actives = c(T,F,F,F,F,F)
  )
  #### ** REMOVE BUTTON ####
  output$adder <- renderUI({
    actionButton(ns('addOne'), "Add EIC")
  })
  
  observeEvent(input$addOne,{
    Falses <- which(!internalValues$actives)
    
    if(length(Falses) > 0){
      internalValues[[paste0("EIC",Falses[1])]]$active <- TRUE
      internalValues$actives[Falses[1]] <- TRUE
    }
    
  })
  
  
  #report back if a plot gets deactivated
  observeEvent(c(internalValues$EIC1$active,
                 internalValues$EIC2$active,
                 internalValues$EIC3$active,
                 internalValues$EIC4$active,
                 internalValues$EIC5$active,
                 internalValues$EIC6$active),{
                   EICnumbers <- 1:6
                   
                   for(i in EICnumbers){
                     internalValues$actives[i] <- internalValues[[paste0("EIC",i)]]$active
                   }
                 })
  
  observeEvent(c(internalValues$EIC1$controls$marker),{
    
    internalValues$currentView  <- internalValues$EIC1
    
  })
  
  observeEvent(c(internalValues$EIC2$controls$marker),{
    
    internalValues$currentView  <- internalValues$EIC2
    
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
    EICmoduleUI(ns("EIC1")),
    EICmoduleUI(ns("EIC2")),
    EICmoduleUI(ns("EIC3")),
    EICmoduleUI(ns("EIC4")),
    EICmoduleUI(ns("EIC5")),
    EICmoduleUI(ns("EIC6"))
    # EICmoduleUI("EIC2"),
    #  EICmoduleUI("EIC3"),
    # EICmoduleUI("EIC4"),
    #EICmoduleUI("EIC5"),
    #EICmoduleUI("EIC6")
  )
}