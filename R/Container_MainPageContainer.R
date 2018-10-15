#' MainPageContainer
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values Import data from the shiny session
#' 
#' @export 
MainPageContainer <- function(input,output, session,
                              values = reactiveValues(projectData = values$projectData,
                                                      featureTables = values$featureTables,
                                                      MSData = values$MSData,
                                                      GlobalOpts = values$GlobalOpts),
                              keys = reactive({keyin$keyd})
){
  
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
                                                    GlobalOpts = values$GlobalOpts),
                            keys = keys)
  
  MainTabBox <- callModule(FeatureTableContainer, "maintabbox",
                           values = reactiveValues(featureTables = values$featureTables,
                                                   MSData = values$MSData,
                                                   GlobalOpts = values$GlobalOpts,
                                                   projectData = values$projectData))
  
  
  FilterSort <-  callModule(FilterContainer, "filtersort", values = reactiveValues(featureTables = values$featureTables,
                                                                                   MainTable = MainTabBox$MainTable))
  
  internalValues <- reactiveValues(OptionsBox = OptionsBox,
                                   MainPlotBox = MainPlotBox,
                                   MainTabBox = MainTabBox,
                                   FilterSort = FilterSort)
  
  return(internalValues)
  
}

#' MainPageContainerUI
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param id
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