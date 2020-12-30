#' ModalWidget
#' 
#' Generic module for modal dialogs that are launched from a button.
#' 
#' @inherit MseekWidgets
#' 
#' @param reactives a \code{reactive({})} returning a list with one element,
#'  \code{fp}, see details
#' @param useActionLink if TRUE, will use an \code{actionLink} instead 
#' of an \code{actionButton} to open the modal Dialog
#'  
#' @details Because the UI elements of the modal dialog are passed in as 
#' \code{reactives()$fp}, they can be namespaced and easily accessed in the 
#' parent module that can then handle the input from the modal dialog.
#' \describe{
#' \item{reactives()$fp}{Specify the UI elements of the modal dialog here}
#' \item{static}{
#' \itemize{
#' \item \code{tooltip} tooltip when hovering over the button
#' \item \code{title} title of the modal dialog
#' \item \code{label} label of the button opening the modal dialog
#' \item \code{icon} \code{\link[shiny]{icon}()} of the button opening the modal dialog
#' }
#' }
#' }
#' 
#' @return returns its internalValues
#' 
#' @describeIn ModalWidget Server logic
#' 
#' @export 
ModalWidget <- function(input,output, session,
                             reactives = reactive({fp = NULL}),
                               static = list(tooltip = "Tooltip",
                                             title = "title", 
                                             label = "label",
                                             icon = icon("bar-chart", lib = "font-awesome"),
                                             modalButtonLabel = "Cancel"),
                             useActionLink = F,
                        style="color:#ffffff;padding:15px;border-left-width:0;border-right:1px solid #eee"
){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  
  output$modButton <- renderUI({
    
    if(useActionLink){
      
      
      actionLink(ns("modbutton"), static$label,
                 icon = static$icon, style= style,
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
        easyClose = TRUE,
        fade = FALSE,
        size = "l",
        footer = modalButton(if(is.null(static$modalButtonLabel)){"Cancel"}else{static$modalButtonLabel}) 
      ))
    
  })
  
 return(internalValues)
}

#' @describeIn ModalWidget UI elements
#' @export
ModalWidgetUI <- function(id)
{
  ns <- NS(id)
  
  htmlOutput(ns("modButton"))
  
}