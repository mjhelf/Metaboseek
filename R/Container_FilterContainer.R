#' FilterContainer
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @inherit MseekContainers
#' @describeIn FilterContainer server logic module, to be called with \link[shiny]{callModule}()
#' 
#' @export 
FilterContainer <- function(input,output, session,
                              values){
  
  
  
  callModule(ColumnSelModule, "colselector", values = reactiveValues(featureTables = values$featureTables,
                                                                                    MainTable = values$featureTables$Maintable))
  
  
  
  
  callModule(MultiFilterModule, "rowselector", values = reactiveValues(featureTables = values$featureTables,
                                                                                       MainTable = values$featureTables$Maintable))

  callModule(TableAnalysisModule, "tabanalysis", values,
                      reactives = reactive({list()}))
  
  callModule(ChangeFTGroupingModule, "tabgrouping",
             values = reactiveValues(fileGrouping = NULL,
                                     featureTables = values$featureTables,
                                     MSData = values$MSData,
                                     projectData = values$projectData))
  
  # internalValues <- reactiveValues(ColSelector = ColSelector,
  #                                  RowSelector = RowSelector)
  # 
  # return(internalValues)
  
}

#' @describeIn FilterContainer returns the \code{shiny} UI elements for the Main Table filters
#' 
#' @export
FilterContainerUI <- function(id){
  ns <- NS(id)
 
  tabBox(title = "Feature Table Actions",
         id = ns("FilterBox"), width = 12, side = "right", selected = "Filter Table",
         
         tabPanel("_"),
         tabPanel("Regroup Table",
                  ChangeFTGroupingModuleUI(ns("tabgrouping"))
         ),
         tabPanel("Analyze Table",
                  TableAnalysisModuleUI(ns("tabanalysis"))
         ),
         tabPanel("Filter Table",
                  fluidPage(
                    fluidRow(
                      column(6,
                             ColumnSelModuleUI(ns("colselector"))
                      ),
                      column(6,
                             MultiFilterModuleUI(ns("rowselector"))
                      )
                    )
                  )
         )
  )
  
  
}