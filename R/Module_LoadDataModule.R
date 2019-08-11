#' LoadDataModule
#' 
#' Container Module for loading a Project Folder, MS data, or feature Tables.
#' TODO: transition to Container format/naming
#' 
#' @inherit MseekContainers
#' 
#' @return returns its internalValues
#' 
#' @examples 
#' \dontrun{
#' library(shiny)
#' 
#' ui <- MseekMinimalUI(SelectProjectFolderModuleUI("examplemodule"), diagnostics = T)
#' 
#' server <- function(input, output) {
#'   MseekMinimalServer(diagnostics = T, data = F, tables = F)
#'   
#'   ExampleModule <- callModule(SelectProjectFolderModule, "examplemodule", values)
#' }
#' 
#' # Create Shiny app ----
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @describeIn LoadDataModule server logic
#' 
#' @export 
LoadDataModule <- function(input,output, session,
                             values = reactiveValues(projectData = NULL,
                                                     featureTables = NULL,
                                                     MSData = NULL,
                                                     GlobalOpts = NULL)
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
  
  callModule(LoadSessionModule, "loadsession",values)
  
  
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

#' @describeIn LoadDataModule UI elements
#' @export 
LoadDataModuleUI <- function(id){
  
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(12, style = "text-align:center;", htmlOutput(ns("showfolder"))
      )),
    fluidRow(
      column(4, style = "justify-content:center;display:flex", LoadSessionModuleUI(ns("loadsession"))
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