#' FilterContainer
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values Import data from the shiny session
#' 
#' @export 
FilterContainer <- function(input,output, session,
                              values = reactiveValues(featureTables = featureTables,
                                                      MainTable = MainTable)
){
  
  
  
  ColSelector <- callModule(ColumnSelModule, "colselector", values = reactiveValues(featureTables = values$featureTables,
                                                                                    MainTable = values$MainTable))
  
  
  
  
  RowSelector <-  callModule(MultiFilterModule, "rowselector", values = reactiveValues(featureTables = values$featureTables,
                                                                                       MainTable = values$MainTable))
  
  
  internalValues <- reactiveValues(ColSelector = ColSelector,
                                   RowSelector = RowSelector)
  
  return(internalValues)
  
}

#' FilterContainerUI
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param id
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