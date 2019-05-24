#' MainPageContainer
#' 
#' Container for modules in the Metaboseek main page
#' 
#' 
#' @inherit MseekContainers
#' @describeIn MainPageContainer server logic module, to be called with \link[shiny]{callModule}()
#' 
#' @export 
MainPageContainer <- function(input,output, session,
                              values = reactiveValues(projectData = values$projectData,
                                                      featureTables = values$featureTables,
                                                      MSData = values$MSData,
                                                      GlobalOpts = values$GlobalOpts)){
    
    ns <- NS(session$ns(NULL))
    
    
    MainPlotBox <- callModule(MainPlotContainer, "mainplotbox",
                              values = reactiveValues(projectData = values$projectData,
                                                      featureTables = values$featureTables,
                                                      MSData = values$MSData,
                                                      GlobalOpts = values$GlobalOpts))
    
    callModule(OptionsContainer, "optionsbox",
               values = reactiveValues(projectData = values$projectData,
                                       featureTables = values$featureTables,
                                       MSData = values$MSData,
                                       GlobalOpts = values$GlobalOpts,
                                       MainPlotBox = MainPlotBox))
    
    callModule(FeatureTableContainer, "maintabbox",
               values = reactiveValues(featureTables = values$featureTables,
                                       MSData = values$MSData,
                                       GlobalOpts = values$GlobalOpts,
                                       projectData = values$projectData))
    
    
    callModule(FilterContainer, "filtersort", values)
    
    
}


#' @describeIn MainPageContainer returns the \code{shiny} UI elements for the METABOseek main page
#' 
#' @export
MainPageContainerUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      OptionsContainerUI(ns("optionsbox"))
    ),
    fluidRow(
      MainPlotContainerUI(ns("mainplotbox"))
    ),
    fluidRow(
      FeatureTableContainerUI(ns("maintabbox"))),
    fluidRow(
      FilterContainerUI(ns("filtersort"))
    )
  )
  
}