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
  
  
  
  OptionsBox <- callModule(OptionsContainer, "optionsbox",
                           values = reactiveValues(projectData = values$projectData,
                                                   featureTables = values$featureTables,
                                                   MainTable = MainTabBox$MainTable,
                                                   MSData = values$MSData,
                                                   GlobalOpts = values$GlobalOpts,
                                                   MainPlotBox = MainPlotBox)
  )
  
  
  MainPlotBox <- callModule(MainPlotContainer, "mainplotbox",
                            values = reactiveValues(projectData = values$projectData,
                                                    MainTable = MainTabBox$MainTable,
                                                    featureTables = values$featureTables,
                                                    MSData = values$MSData,
                                                    GlobalOpts = values$GlobalOpts))
  
  MainTabBox <- callModule(FeatureTableContainer, "maintabbox",
                           values = reactiveValues(featureTables = values$featureTables,
                                                   MSData = values$MSData,
                                                   GlobalOpts = values$GlobalOpts,
                                                   projectData = values$projectData))
  
  
  #FilterSort <- 
      callModule(FilterContainer, "filtersort", values = reactiveValues(featureTables = values$featureTables,
                                                                                   MainTable = MainTabBox$MainTable))
  
  # internalValues <- reactiveValues(
  #                                  MainPlotBox = MainPlotBox,
  #                                  MainTabBox = MainTabBox,
  #                                  FilterSort = FilterSort)
  
 # return(internalValues)
  
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