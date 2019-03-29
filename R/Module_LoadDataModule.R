#' LoadDataModule
#' 
#' 
#' server module for loading a Project Folder
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
LoadDataModule <- function(input,output, session,
                             values = reactiveValues(projectData = projectData,
                                                     featureTables = featureTables,
                                                     MSData = MSData,
                                                     GlobalOpts = GlobalOpts)
){
  
  ns <- NS(session$ns(NULL))
  
  
  
  Table <- callModule(LoadTableModule, "table",
                           values = reactiveValues(projectData = values$projectData,
                                                   featureTables = values$featureTables),
                           static = list(tooltip = "Load a Feature Table",
                                         label = "Load Feature Table",
                                         format = list(header = T,
                                                       sep = NULL,
                                                       quote = '"',
                                                       stringsAsFactors = F),
                                         pattern = "\\.csv$")
  )
  
  Projectfolder <- callModule(SelectProjectFolderModule, "projectfolder",
                        values = reactiveValues(projectData = values$projectData,
                                                featureTables = values$featureTables,
                                                MSData = values$MSData,
                                                GlobalOpts = values$GlobalOpts)
  )
  
 
  Msdata <- callModule(LoadMSDataModule, "msdata",
                             values = reactiveValues(projectData = values$projectData,
                                                     featureTables = values$featureTables,
                                                     MSData = values$MSData,
                                                     GlobalOpts = values$GlobalOpts)
  )
  
  
  
  internalValues <- reactiveValues(Table = Table,
                                   Projectfolder = Projectfolder,
                                   Msdata = Msdata)
  
  output$showfolder <- renderUI({
    
    p(
      if(is.null(values$projectData$projectFolder)){"No Project Folder currently selected"}
      else{paste("Current Project Folder:",  values$projectData$projectFolder)}
    )
    
  })

  return(internalValues)
}

#' LoadDataModuleUI
#' 
#' 
#' server module for loading Tables
#' 
#' @param id
#' 
#' @export 
LoadDataModuleUI <- function(id){
  
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4, style = "text-align:center;", htmlOutput(ns("showfolder"))
      ),
      column(4, style = "justify-content:center;display:flex", SelectProjectFolderModuleUI(ns("projectfolder"))
             ),
      column(2, style = "justify-content:center;display:flex", LoadTableModuleUI(ns("table"))
             ),
      column(2, style = "justify-content:center;display:flex", LoadMSDataModuleUI(ns("msdata"))
             )
    )
      
  )
}