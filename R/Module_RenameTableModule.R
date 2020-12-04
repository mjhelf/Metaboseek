#' RenameTableModule
#' 
#' Rename the current Feature Table
#' 
#' @inherit MseekModules
#' 
#' @return Returns its internalValues
#' 
#' @describeIn RenameTableModule Server logic
#' 
#' @export 
RenameTableModule <- function(input,output, session, values){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  
 
  
  dialog <- callModule(ModalWidget, "renamingbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                           fluidRow(
                               column(9, 
                                      textInput(ns('nameinput'),
                                                "New name:",
                                                value = FeatureTable(values)$tablename)),
                               
                             column(3,
                                    actionButton(ns("abutton"), "Rename")
                             )
                           ))
                              )}),
                       static = list(tooltip = "Rename the currently active FeatureTable",
                                     title = "Rename Feature Table", 
                                     label = "Rename",
                                     icon = icon("file-signature", lib = "font-awesome")))
  
  
  
  
  observeEvent(input$abutton,{
    tryCatch({
        
          updateFT(values)
          
          FeatureTable(values) <- rename(FeatureTable(values), 
                                                         name = input$nameinput)

          removeModal()
          
          #showNotification(paste("An error occured: ", e), duration = 0, type = "error")
  },
      error = function(e){
        
        showNotification(paste("An error occured: ", e), duration = 0, type = "error")
        
        
      })
    
  })
  
  
  return(internalValues)
}

#' @describeIn RenameTableModule UI elements
#' @export
RenameTableModuleUI <- function(id)
{
  ns <- NS(id)
  
  ModalWidgetUI(ns("renamingbutton"))
  
}