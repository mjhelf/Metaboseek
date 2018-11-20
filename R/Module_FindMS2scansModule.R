#' FindMS2ScansModule
#' 
#' 
#' server module for saving Tables
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
FindMS2ScansModule <- function(input,output, session,
                            values = reactiveValues(MSData = MSData,
                                                    featureTables = featureTables,
                                                    MainTable = MainTable),
                            static = list(tooltip = "Find MS2 scans for all parent m/zs in feature table",
                                          label = "Find MS2 scans")
){
  ns <- NS(session$ns(NULL))
  
  output$findMS2button <- renderUI({
    div(title = static$tooltip,
    actionButton(ns("findMS2"), 
                 static$label,
                 icon = icon("chart-bar", lib = "font-awesome")
                 )
    )
  })
  
  observeEvent(input$findMS2,{
    
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            p(strong("Find all MS2 scans for parent m/z values in your Feature table")),
            p("A column named MS2scans will be generated in your Feature Table that liosts all scans matching a feature. Empty entries mean no hits. If this column already exists, it will be overriden.")
          ),
          hr(),
          fluidRow(
            column(4, div(title = "Define ppm tolerance for parent m/z search",
                          numericInput(ns("MS2ppm"),"ppm window", value = 5))),
            column(4, div( title = "Define retention time tolerance for parent m/z search", 
                           numericInput(ns("MS2rtw"), "RT window (in seconds)", value = 15))),
            column(4, div( title = "Search MS2 scans", 
                           actionButton(ns("startMS2search"), "Start search")))
          )),
        title = "Search MS2 scans",
        easyClose = T,
        fade = F,
        size = "l",
        footer = modalButton("Cancel") 
      ))
    
  })
  
  
  observeEvent(input$startMS2search,{
    
    tryCatch({
      TableUpdateChunk()
      
      withProgress(message = 'Please wait!', detail = "Finding MS2 scans", value = 0.5, {
        
        MS2s <-  data.frame(MS2scans = mapply(listMS2scans,
                                      mz = values$featureTables$tables[[values$featureTables$active]]$df$mz,
                                      rt = values$featureTables$tables[[values$featureTables$active]]$df$rt,
                                      MoreArgs = list(ppm = input$MS2ppm,
                                                      rtw = input$MS2rtw,
                                                      MSData = values$MSData$data)
      ), stringsAsFactors = F)
      
      values$featureTables$tables[[values$featureTables$active]] <- updateFeatureTable(values$featureTables$tables[[values$featureTables$active]],MS2s)
})

      showNotification(paste("Finished MS2 search"), duration = 10)
      removeModal()
      
    },
    error = function(e){
      print(e)
      showNotification(paste("A problem occured and MS2 search failed"), type = "error", duration = 0)
    }
  )
  })
  
  
}

#' FindMS2ScansModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
FindMS2ScansModuleUI <- function(id)
{
  ns <- NS(id)
  
  htmlOutput(ns("findMS2button"))
  
}