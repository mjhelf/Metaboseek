#' SaveNetworkModule
#' 
#' 
#' server module for saving Tables
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
SaveNetworkModule <- function(input,output, session,
                            reactives = reactive({list(graphname = NULL,
                                                       filename = "table.graphml")}),
                            values = reactiveValues(Networks = Networks),
                            static = list(tooltip = "Save Network as a graphml file",
                                          label = "Save Network",
                                          format = c("graphml"))
){
  ns <- NS(session$ns(NULL))
  
  output$saveNetworkButton <- renderUI({
    div(title = static$tooltip,
    actionButton(ns("savenetwork"), static$label, icon = icon("floppy-save", lib = "glyphicon"))
    )
  })
  
  observeEvent(input$savenetwork,{
    
    if(!is.null(reactives()$graphname) &&  reactives()$graphname %in% names(values$Networks) ){
    
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            p(strong("You can save this network in your project folder, or download it through your browser"))
          ),
          hr(),
          fluidRow(
            column(6, div(title = "Download table through Browser",     downloadButton(ns("modalDownload"),"Download"))),
            column(6, div( title = "Save directly to current projectFolder (only works if you are working in a project folder)", actionButton(ns("modalProjectFolder"), "Save locally")))
          )),
        title = "Save table",
        easyClose = T,
        fade = F,
        size = "s",
        footer = modalButton("Cancel") 
      ))
    
    
  }else{
    showNotification(paste("No network available to be saved!"), type = "error", duration = 10)
    
    
    
  }
    
  })
  
  output$modalDownload <- downloadHandler(filename= function(){basename(reactives()$filename)}, 
                                          content = function(file){
                                            try({
                                            write_graph(values$Networks[[reactives()$graphname]]$graph,
                                                        file,
                                                        format = "graphml")
                                            
                                            showNotification(paste("Downloading file: ", basename(reactives()$filename)), duration = 10)
                                            })
                                            removeModal()
                                          },
                                          contentType = "text/xml")
  
  
  # observeEvent(output$modalDownload,{
  #   print(output$modalDownload)
  # 
  # })
  
  observeEvent(input$modalProjectFolder,{
    
    if(!is.null(values$projectData$projectFolder)){
      
      if(!dir.exists(dirname(file.path(values$projectData$projectFolder, reactives()$filename)))){
        dir.create(dirname(file.path(values$projectData$projectFolder, reactives()$filename)), recursive = T)
      }
      
      write_graph(values$Networks[[reactives()$graphname]]$graph,
                  file.path(values$projectData$projectFolder, reactives()$filename),
                  format = "graphml")

      showNotification(paste("Network saved as: ", file.path(values$projectData$projectFolder, reactives()$filename)), duration = 10)
      removeModal()
      
    }
    else{
      showNotification(paste("You have to work in a Project Folder to save files this way!"), type = "error", duration = 10)
    }
  })
  
  
  
}

#' SaveNetworkModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
SaveNetworkModuleUI <- function(id)
{
  ns <- NS(id)
  
  htmlOutput(ns("saveNetworkButton"))
  
}