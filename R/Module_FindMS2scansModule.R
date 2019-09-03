#' FindMS2ScansModule
#' 
#' A one-button modue to enrich a Feature Table with information about available 
#' MS2 scans for each molecular feature
#' 
#' @describeIn FindMS2ScansModule server logic
#' 
#' @inherit MseekModules
#' 
#' @export 
FindMS2ScansModule <- function(input,output, session, values,
                            static = list(tooltip = "Find MS2 scans for all parent m/zs in feature table",
                                          label = "Find MS2 scans")
){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  
  output$findMS2button <- renderUI({
    div(title = static$tooltip,
    actionButton(ns("findMS2"), 
                 static$label,
                 icon = icon("bar-chart", lib = "font-awesome")
                 )
    )
  })
  
  observeEvent(input$findMS2,{
    
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            p(strong("Find all MS2 scans for parent m/z values in your Feature table")),
            p("A column named MS2scans will be generated in your Feature Table 
              that lists all scans matching a feature. Empty entries mean no hits.
              If this column already exists, it will be overriden. Will look in 
              all MS data files that are loaded in the current layout.")
          ),
          hr(),
          fluidRow(
            column(3, div(title = "Define ppm tolerance for parent m/z search",
                          numericInput(ns("MS2ppm"),"ppm window", value = 5))),
            column(3, div( title = "Define retention time tolerance for parent m/z search", 
                           numericInput(ns("MS2rtw"), "RT window (in seconds)", value = 15))),
            column(2, div(title = "Spectra will be assigned to only one matching feature (the one with the closest elution time).",
                          checkboxInput(ns("rtMatch"),"unique assignments", value = T))),
            
            column(2, div( title = "Search MS2 scans", 
                           actionButton(ns("startMS2search"), "Start search"))),
            column(2, div( title = "You can skip this step if you have already assigned MS2 scans to features in the feature table.", 
                           actionButton(ns("skipMS2search"), "Skip")))
          )),
        title = "Search MS2 scans",
        easyClose = T,
        fade = F,
        size = "l",
        footer = modalButton("Cancel") 
      ))
    
  })
  
  observeEvent(input$skipMS2search,{
    
    if(!is.null(values$featureTables$tables[[values$featureTables$active]]$df$MS2scans)){

      removeModal()
      internalValues$done <- TRUE
      
    }else{
      showNotification(paste("No MS2scan data available for this feature table yet, you cannot skip this step."), type = "error", duration = 0)
    }
  })
  
  
  observeEvent(input$startMS2search,{
    
    tryCatch({
        updateFT(values)
        
      withProgress(message = 'Please wait!', detail = "Finding MS2 scans", value = 0.5, {
          FeatureTable(values) <- FTMS2scans(FeatureTable(values),
                                             values$MSData$data[values$MSData$layouts[[values$MSData$active]]$filelist],
                                             ppm = input$MS2ppm,
                                             rtw = input$MS2rtw,
                                             uniqueMatch = input$rtMatch)
     })
      if(hasError(previousStep(FeatureTable(values)))){
          showNotification(paste("An error occured: ",
                                 unlist(error(previousStep(FeatureTable(values))))),
                           duration = 0, type = "error")
          
      }else{

      showNotification(paste("Finished MS2 search"), duration = 10)
      removeModal()
      }
      internalValues$done <- TRUE
      
    },
    error = function(e){
      print(e)
      showNotification(paste("A problem occured and MS2 search failed"), type = "error", duration = 0)
    }
  )
  })
  
  return(internalValues)
}


#' @describeIn FindMS2ScansModule UI elements
#' @export
FindMS2ScansModuleUI <- function(id)
{
  ns <- NS(id)
  
  htmlOutput(ns("findMS2button"))
  
}