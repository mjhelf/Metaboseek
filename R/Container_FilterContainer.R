#' FilterContainer
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @inherit MseekContainers
#' @describeIn FilterContainer server logic module, to be called with \link[shiny]{callModule}()
#' 
#' @export 
FilterContainer <- function(input,output, session,
                              values = reactiveValues(featureTables = featureTables,
                                                      MainTable = MainTable)
){
  
  
  
  callModule(ColumnSelModule, "colselector", values = reactiveValues(featureTables = values$featureTables,
                                                                                    MainTable = values$MainTable))
  
  
  
  
  callModule(MultiFilterModule, "rowselector", values = reactiveValues(featureTables = values$featureTables,
                                                                                       MainTable = values$MainTable))
  
  
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
 
  tabBox(title = "Table Filter",
         id = ns("FilterBox"), width = 12, side = "right", selected = "Table Filter",
         
         tabPanel("_"),
         tabPanel("Table Filter",
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