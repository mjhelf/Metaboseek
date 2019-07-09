#' SwitchButton
#' 
#' server module for UI element that changes appearance when interacted with 
#' (e.g. clicked on) and returns which appearance (state) it is currently using
#' 
#' @inherit MseekWidgets
#' @param states named list of arguments to be passed to type function. 
#' NOTE: Do not provide inputId.
#' @param type type of UI element, typically "actionButton" or "actionLink"
#' 
#' @importFrom shiny actionButton actionLink
#' 
#' @describeIn SwitchButton server logic
#' 
#' @export 
SwitchButton <- function(input,output, session, 
                         states = list(on = list(label = "",
                                                 title = "it's on",
                                                 icon = icon("react"),
                                                 style = "background-color: #1BBF1E;"),
                                       off = list(label = "",
                                                  icon = icon("react"),
                                                  title = "it's off")),
                         type = "actionButton"
){
    
    ns <- NS(session$ns(NULL))
    
    internalValues <- reactiveValues(state = if(length(states)>0){names(states)[1]}else{NULL},
                                     baseArgs = list(inputId = ns("switchButton")))
    
    
    observeEvent(input$switchButton,{
        
        i <- match(internalValues$state, names(states))
        if(i == length(states)){
            
            internalValues$state <- names(states)[1]
        }else{
            internalValues$state <- names(states)[i+1]
            
        }
        
    })
    
    output$swBu <- renderUI({
        
        do.call(type,c(internalValues$baseArgs,states[[internalValues$state]]))
        
    })  
    
    return(internalValues)
}

#' @describeIn SwitchButton UI elements
#' @export
SwitchButtonUI <- function(id){
    ns <- NS(id)
    
    htmlOutput(ns("swBu"))
    
    
}