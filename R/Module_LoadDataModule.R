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
                                         pattern = "\\.csv$|\\.[Mm][Ss][Kk][Ff][Tt]$"),
                      width = "100%", style = "height: 50px;"
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
      shinydashboard::box(title = "Load Data",
                          width = 6,
                          height = '200px',
                          fluidRow(
                            column(6, style = "justify-content:center;display:flex", LoadTableModuleUI(ns("table"), style = "width: 100%;")
                            ),
                            column(6, style = "justify-content:center;display:flex", LoadMSDataModuleUI(ns("msdata"))
                            ))),
      shinydashboard::box(title = "Load Metaboseek Project",
                          width = 6,
                          height = '200px',
                          fluidRow(
                            column(1,
                                   div(h3(icon("question-circle-o")),
                                       style = "text-align: center;",
                                       title = "Generate a Project Folder by running XCMS with Metaboseek. MS Data associated with a Project Folder will be loaded along with a selected Feature Table. Filtered Feature Tables can be easily saved in the Project Folder.")
                            ),
                            column(8, style = "justify-content:center;",#display:flex",
                                   fluidRow(SelectProjectFolderModuleUI(ns("projectfolder"))),
                                   fluidRow(style = "text-align:center;", htmlOutput(ns("showfolder")))
                            ),
                            column(3, style = "justify-content:center;display:flex",
                                   LoadSessionModuleUI(ns("loadsession"))
                            ))
      )
    ))
}