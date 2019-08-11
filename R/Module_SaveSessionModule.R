#' SaveSessionModule
#' 
#' Module for saving Tables
#' 
#' @inherit MseekModules
#' 
#' @return Returns nothing
#' 
#' @describeIn SaveSessionModule Server logic
#' 
#' @importFrom data.table fwrite
#' 
#' @export 
SaveSessionModule <- function(input,output, session, values,
                            reactives = reactive({list(filename = "savedSessions/session")}),
                            useActionLink = F
                            
){
  ns <- NS(session$ns(NULL))
  
  dialog <- callModule(ModalWidget, "openModal",
                       reactives = reactive({  
                           list(fp = fluidPage(
                               fluidRow(
                                   p("Save a Metaboseek session in the current project folder.")
                                   ),
                               hr(),
                               fluidRow(
                                   column(6,
                                   textInput(ns("tabname"), "File name:",
                                             value = paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                                                            basename(reactives()$filename)))),
                                   column(3,
                                          div(title = "include MS data in session file (much larger file, but session loads faster)",
                                              checkboxInput(ns("saveMSData"), "include MS data", value = FALSE))
                               )),
                               fluidRow(htmlOutput("fileWarning")),
                               fluidRow(
                                   column(6, div( title = "Save directly to current projectFolder (only works if you are working in a project folder)",
                                                  actionButton(ns("saveLocally"),
                                                               "Save locally")))
                               )
                           ))}),
                       static = list(tooltip = "Save a Metaboseek session",
                                     title = "Save a Metaboseek session", 
                                     label = "",
                                     icon = icon("floppy-disk", lib = "glyphicon")),
                       useActionLink = useActionLink)
  
  
  output$fileWarning <- renderUI({
    if(!is.null(values$projectData$projectFolder) && !is.null(input$tabname)){
      if(
        file.exists(file.path(values$projectData$projectFolder,
                              file.path(dirname(reactives()$filename),
                                        paste0(input$tabname,".msks"))))
      ){
        
        p("Warning: Filename already exists. You will override the existing file when saving locally!", style = "color:#ff1111;")
        
      }
    }
  })
  
  
  
  observeEvent(input$saveLocally,{
      
      tryCatch({
          withProgress(message = 'Please wait!', detail = "Saving Metaboseek session", value = 0.5, {
      
      if(!is.null(values$featureTables)){
          updateFT(values)
      }
    
    if(!is.null(values$projectData$projectFolder)){
      
      if(!dir.exists(dirname(file.path(values$projectData$projectFolder,
                                       file.path(dirname(reactives()$filename),
                                                 input$tabname))))){
        dir.create(dirname(file.path(values$projectData$projectFolder,
                                     file.path(dirname(reactives()$filename),
                                               input$tabname))), recursive = T)
      }
        
    saveMseekSession(values, path = file.path(values$projectData$projectFolder,
                                                  file.path(dirname(reactives()$filename),
                                                            paste0(input$tabname,".msks"))),
                         MSData = input$saveMSData)
            
      
      showNotification(paste("Table saved as: ", file.path(values$projectData$projectFolder, 
                                                           file.path(dirname(reactives()$filename),
                                                                     if(!is.null(input$selFormat) && input$selFormat == "instrumentList"){paste0(input$tabname,".txt")}
                                                                     else{input$tabname})), duration = 10))
      removeModal()
      
    }
    else{
      showNotification(paste("You have to work in a Project Folder to save files this way!"), type = "error", duration = 10)
    }
          })},
          error = function(e){
              
              showNotification(paste("An error occured: ", e), duration = 0, type = "error")
              
              
          })
  })
  
  
  
}

#' @describeIn SaveSessionModule UI elements
#' @export
SaveSessionModuleUI <- function(id)
{
  ns <- NS(id)
  
  ModalWidgetUI(ns("openModal"))
  
}