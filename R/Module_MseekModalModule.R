#' MseekModalModule
#' 
#' 
#' server module for saving Tables
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
MseekModalModule <- function(input,output, session,
                             reactives = reactive({fp = NULL}),
                               static = list(tooltip = "Tooltip",
                                             title = "title", 
                                             label = "label",
                                             icon = icon("bar-chart", lib = "font-awesome"))
){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  
  output$modButton <- renderUI({
    div(title = static$tooltip,
        actionButton(ns("modbutton"), 
                     static$label,
                     icon = static$icon
        )
    )
  })
  
  observeEvent(input$modbutton,{
    
    showModal(
      modalDialog(
        reactives()$fp
        ,
        title = static$title,
        easyClose = F,
        fade = F,
        size = "l",
        footer = modalButton("Cancel") 
      ))
    
  })
  
 return(internalValues)
}

#' MseekModalModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
MseekModalModuleUI <- function(id)
{
  ns <- NS(id)
  
  htmlOutput(ns("modButton"))
  
}