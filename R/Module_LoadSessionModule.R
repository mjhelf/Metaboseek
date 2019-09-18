#' LoadSessionModule
#' 
#' Module for loading an entire Metaboseek session
#' 
#' @inherit MseekModules
#' 
#' @param useActionLink if TRUE, will use an \code{actionLink} instead 
#' of an \code{actionButton} to open the modal Dialog
#' 
#' @return nothing, but modifies values
#' 
#' @details Loads a Metaboseek session from a MseekSession object saved in
#'  an RDS file with the file extension ".Msks".
#' 
#' @describeIn LoadSessionModule server logic
#' 
#' @export 
LoadSessionModule <- function(input,output, session, values,
                            useActionLink = F){
    ns <- NS(session$ns(NULL))
    
    internalValues  <- reactiveValues(df = NULL,
                                      filename = NULL)
    
    
    
    dialog <- callModule(ModalWidget, "openModal",
                         reactives = reactive({  
                             list(fp = fluidPage(
                                 
                                 fluidRow(
                                     p("Load a Metaboseek session")
                                 ),
                                 fluidRow(
                                     FilePathModuleUI(ns("selectFile"))
                                     ),
                                 fluidRow(
                                     actionButton(ns("loadsession"),"Load Session")
                                     )
                                 ))}),
                         static = list(tooltip = "Load a Metaboseek session",
                                       title = "Load a Metaboseek session", 
                                       label = "Load Session",
                                       icon = icon("cube", lib = "font-awesome")),
                         useActionLink = useActionLink)
    
fileSelection <- callModule(FilePathModule, "selectFile",
                       filepaths = reactive({values$GlobalOpts$filePaths}),
                       label = "Session File", description= "Select Metaboseek session file",
                       selectFile = T,
                       displayFolder = T,
                       pattern = "\\.[Mm][Ss][Kk][Ss]$|")
    
    
    observeEvent(input$loadsession,{
        tryCatch({
            withProgress(message = 'Please wait!', detail = "Loading Metaboseek session", value = 0.5, {
              
                loadMseekSession(values, fileSelection$files$datapath)
            })
            
            removeModal()
            showNotification(paste("Session loaded successfully!"), duration = 0, type = "message")
            
        }
        ,
        error = function(e){
            
            showNotification(paste("An error occured: ", e), duration = 0, type = "error")
            
            
        })
        
    })
    
    
}

#' @describeIn LoadSessionModule UI elements
#' @export
LoadSessionModuleUI <- function(id)
{
    ns <- NS(id)
    
    ModalWidgetUI(ns("openModal"))
    
}