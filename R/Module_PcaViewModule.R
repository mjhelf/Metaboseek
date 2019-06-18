#' PcaViewModule
#' 
#' Module to view Principal Component Analysis (PCA) results
#' 
#' @inherit MseekModules
#' 
#' @return Returns its internalValues
#' 
#' @describeIn PcaViewModule Server logic
#' 
#' @import shiny
#' 
#' @export 
PcaViewModule <- function(input,output, session,
                              values = reactiveValues(featureTables = featureTables)
){
  #### Initialization ####
  
  ns <- NS(session$ns(NULL))
  
   # internalValues <- reactiveValues(samples = PcaViewSamples,
   #                                  features = PcaViewFeatures)
   # 
  PcaViewFeatures <- callModule(PlotBrowserModule, "pcaviewfeatures",
                        reactives = reactive({reactiveValues(PCAtable = values$featureTables$tables[[values$featureTables$active]]$df,
                                                             active = T)}),
                        values = NULL,
                        static = list(patterns = list(axis = "PCA__",
                                                      color = "",
                                                      hover = ""))
  )
  
  PcaViewSamples <- callModule(PlotBrowserModule, "pcaviewsamples",
                         reactives = reactive({reactiveValues(PCAtable = values$featureTables$tables[[values$featureTables$active]]$anagrouptable,
                                                              active = T)}),
                         values = NULL,
                         static = list(patterns = list(axis = "PCA__",
                                                       color = "",
                                                       hover = ""))
  )
  
  
  
  
  
 # return(internalValues)
  
}

#' @describeIn PcaViewModule UI elements
#' @export
PcaViewModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
  fluidRow(
    column(6,
           h4("MS feature PCA"),
           PlotBrowserModuleUI(ns("pcaviewfeatures"))
    ),
    column(6,
           h4("Sample PCA"),
           PlotBrowserModuleUI(ns("pcaviewsamples"))
    )
  )
  )
}
