#' LabelFinderModule
#' 
#' Module for finding labeled features
#' 
#' @inherit MseekModules
#' 
#' @return Returns its internalValues
#' 
#' @describeIn LabelFinderModule Server logic
#' 
#' @export 
LabelFinderModule <- function(input,output, session, values){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  
  output$unlabSamples <- renderUI({
    
    selectizeInput(ns("ref_intensityCols"), "Unlabeled samples:", 
                   selected = basename(names(values$MSData$data))[sapply(names(values$MSData$data), function(x){any(grepl(basename(x),FeatureTable(values)$intensities))})],
                   choices = basename(names(values$MSData$data)),
                   multiple = T)
    
  })
  
  output$labSamples <- renderUI({
      
      selectizeInput(ns("comp_intensityCols"), "Labeled samples:", 
                     selected = basename(names(values$MSData$data))[sapply(names(values$MSData$data), function(x){any(grepl(basename(x),FeatureTable(values, tableID = input$labtable)$intensities))})],
                     choices = basename(names(values$MSData$data)),
                     multiple = T)
      
  })
  
  
  dialog <- callModule(ModalWidget, "labelfindbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                           fluidRow(
                             p("A new Feature Table will be generated with Features from the currently active feature table which meet matching criteria with the selected Labeled sample Feature Table")
                           ),
                           fluidRow(
                               column(6,
                                      textInput(ns("tabid"), "Output Feature Table Name:", 
                                                     value = paste0("Labelfinder_", FeatureTable(values)$tablename))
                               ),
                             column(6,
                                    htmlOutput(ns("unlabSamples"))
                             )
                             ),
                           fluidRow(
                               column(6,
                                      selectizeInput(ns("labtable"), "Labeled sample Feature Table:", 
                                                                    selected = values$featureTables$active, 
                                                                    choices = values$featureTables$index,
                                                                    multiple = FALSE)
                               ),
                               column(6,
                                      htmlOutput(ns("labSamples"))
                               )
                               ),
                           fluidRow( 
                             column(4,
                                    div(title = "Mass shift to search for",
                                        numericInput(ns("labelmz"), "mass shift m/z", value = 2*1.00335)
                                    )),
                             column(4,
                                    div(title = "Retention time tolerance for matching peaks.",
                                        numericInput(ns("rtdiff"), "RT tolerance (sec)", value = 3, min = 0)
                                    )),
                             column(4,
                                    div(title = "Require labeled and unlabeled peaks to be of at most this many times different width",
                                        numericInput(ns("pktolerance"), "Peak width tolerance factor", value = 10, min = 0)
                                    ))),
                             fluidRow( 
                               column(4,
                                      div(title = "m/z tolerance for peak matching",
                                          numericInput(ns("ppm_compare"), "m/z tolerance (ppm)", value = 5, min = 0)
                                      )),
                               column(4,
                                      div(title = "m/z tolerance for EIC extraction",
                                          numericInput(ns("ppm_extract"), "m/z tolerance (ppm)", value = 5, min = 0)
                                      )),
                               column(4,
                                      div(title = "Retention time window for EIC extraction (for intensities)",
                                          numericInput(ns("rtw_extract"), "RT window (sec)", value = 5, min = 0)
                                      ))),
                           fluidRow( 
                               column(4,
                                      div(title = "MINIMUM intensity ratio unlabeled peak / labeled peak in unlabeled samples",
                                          numericInput(ns("ifoldS1"), "min fold I1S1/I2S1", value = 10, min = 0)
                                      )),
                               column(4,
                                      div(title = "MAXIMUM intensity ratio unlabeled peak / labeled peak in labeled samples",
                                          numericInput(ns("ifoldS2"), "max I1S2/I2S2", value = 5, min = 0)
                                      )),
                             column(2,
                                    actionButton(ns("abutton"), "Go")
                             )
                           ))
                              )}),
                       static = list(tooltip = "Find features that differ by a specific m/z difference",
                                     title = "Labelfinder", 
                                     label = "Labelfinder",
                                     icon = icon("tag", lib = "font-awesome")))
  
  
  
  
  observeEvent(input$abutton,{
    tryCatch({
      withProgress(message = 'Please wait!', detail = "Finding labeled peaks", value = 0.5, {
        
          updateFT(values)
          
        tabid <- paste0("table",length(values$featureTables$index))
        names(tabid) <- input$tabid
        
           
       
          #simpler:
          values$featureTables$tables[[tabid]] <- LabelFinder(FeatureTable(values),
                                                              object2 = FeatureTable(values, tableID = input$labtable),
                                                              MSData = values$MSData$data,
                                                              newName = names(tabid),
                                                              ref_intensityCols = paste0(input$ref_intensityCols, "__XIC"),
                                                              comp_intensityCols = paste0(input$comp_intensityCols, "__XIC"),
                                                              labelmz = input$labelmz,
                                                              pktolerance = input$pktolerance, 
                                                              rtdiff = input$rtdiff,
                                                              ppm_compare = input$ppm_compare,
                                                              ifoldS1 = input$ifoldS1,
                                                              ifoldS2 = input$ifoldS2,
                                                              ppm_extract = input$ppm_extract,
                                                              rtw_extract = input$rtw_extract
                                                              )

          values$featureTables$index <- c( values$featureTables$index, tabid)
          values$featureTables$tableSwitch <- T
          values$featureTables$active <- unname(tabid)
          values$featureTables$row_filters <- TRUE
          
          
          removeModal()
          
          showNotification(paste("Labeled feature finding completed."), duration = 0, type = "message")
          #showNotification(paste("An error occured: ", e), duration = 0, type = "error")
          
        }
      )},
      error = function(e){
        
        showNotification(paste("An error occured: ", e), duration = 0, type = "error")
        
        
      })
    
  })
  
  
  return(internalValues)
}

#' @describeIn LabelFinderModule UI elements
#' @export
LabelFinderModuleUI <- function(id)
{
  ns <- NS(id)
  
  ModalWidgetUI(ns("labelfindbutton"))
  
}