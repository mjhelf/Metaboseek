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
                                             icon = icon("bar-chart", lib = "font-awesome")),
                             useActionLink = F
){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  
  output$modButton <- renderUI({
    
    if(useActionLink){
      
      
      actionLink(ns("modbutton"), static$label,
                 icon = static$icon, style="color:#ffffff;border-left-width:0;border-right:1px solid #eee",
                 title = static$tooltip )
      
    }else{
    
    
    div(title = static$tooltip,
        actionButton(ns("modbutton"), 
                     static$label,
                     icon = static$icon
        )
    )
    }
    
    
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