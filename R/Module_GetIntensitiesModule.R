#' GetIntensitiesModule
#' 
#' One-button module to get intensities for all features in a feature 
#' table across the loaded MS data files
#' 
#' @inherit MseekModules
#' 
#' @describeIn GetIntensitiesModule server logic
#' 
#' @export 
GetIntensitiesModule <- function(input,output, session,
                           values){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  

  
  
  dialog <- callModule(ModalWidget, "getintbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                         
                           fluidRow(
                             p("Calculate the peak intensities for the current feature table using all MS data files that are", strong("in the currently active MS Grouping layout."))
                           ),
                           fluidRow(
                             column(4,
                                    div(title = "Use rtmin and rtmax of each feature for intensity calculation. 
If this is selected, the rt window setting for peak intensity calculation starts at these reported outsides of the peak instead of at its apex, making the RT window width broader and variable between peaks.",
                                        checkboxInput(ns("intensRangeCheck"), "Use rtmin/rtmax", value  = TRUE)
                                    )),
                             column(4,
                                    div(title = "Calculate  peak areas rather than mean intensities in the retention time window.",
                                        checkboxInput(ns("areaCheck"), "Peak areas", value  = FALSE)
                                    )),
                             column(4,
                                    div(title = "Subtract the minimum intensity value inside the peak retention time window from each EIC before calculating mean intensity/ peak area.",
                                        checkboxInput(ns("baselineCheck"), "Subtract baseline", value  = TRUE)
                                    ))),
                             fluidRow(
                             column(6,
                                    div(title = "Peak retention time window size (+/- in seconds from either a features rt value or its rtmin and rtmax values",
                                        numericInput(ns("intensRTsec"),"Retention time window (seconds)", value = 5, min = 0)
                                    )),
                           
                             column(6,
                                    div(title = "m/z tolerance for intensity calculation",
                                        numericInput(ns("intensppm"), "m/z tolerance (ppm)", value = 5, min = 0)
                                    ))),
                           fluidRow(
                             column(6,
                                    div(title = "Define the SUFFIX for the intensity columns. Column names will be filename__SUFFIX. WARNING: Columns with identical names will be replaced in the current table!",
                                        textInput(ns("intensSuffix"),"Intensity column suffix:", value = "XICmanual")
                                    ))),
                           fluidRow(
                             column(5),
                             column(1,
                                    actionButton(ns("getIntensities"), "Go")
                             )
                           )
                           
                         ) )     }),
                       static = list(tooltip = "Get peak intensities for the features in this table",
                                     title = "Peak intensity calculation", 
                                     label = "Get intensities",
                                     icon = icon("area-chart", lib = "font-awesome")))
  
  
  
  
  observeEvent(input$getIntensities,{
    tryCatch({
      withProgress(message = 'Please wait!', detail = "Calculating peak intensities", value = 0.5, {
          updateFT(values)
          

          FeatureTable(values) <- getMseekIntensities(FeatureTable(values),
                                                      rawdata = values$MSData$data[values$MSData$layouts[[values$MSData$active]]$filelist],
                                                      adjustedRT = FALSE,
                                                      ppm = input$intensppm,
                                                      rtrange = input$intensRangeCheck,
                                                      rtw = input$intensRTsec,
                                                      areaMode = input$areaCheck,
                                                      #using SnowParam causes issues here which may be related to running in a shiny session,
                                                      #TODO: fix this 
                                                      BPPARAM = BiocParallel::SerialParam(),  #if(length(FeatureTable(values)$df$mz) > 10000){bpparam()}else{BiocParallel::SerialParam()},
                                                      baselineSubtract = input$baselineCheck,
                                                      SN = NULL,
                                                      columnSuffix = paste0("__", input$intensSuffix))
          })
       
      
          if(hasError(previousStep(FeatureTable(values)))){
              showNotification(paste("An error occured: ",
                                     unlist(error(previousStep(FeatureTable(values))))),
                               duration = 0, type = "error")
              
          }else{
          removeModal()
          showNotification(paste("Completed peak intensity calculation."), duration = 0, type = "message")
          }
        }
      ,
      error = function(e){
        
        showNotification(paste("An error occured: ", e), duration = 0, type = "error")
        
        
      })
    
  })
  
  
  return(internalValues)
}

#' @describeIn GetIntensitiesModule server logic
#' @export
GetIntensitiesModuleUI <- function(id)
{
  ns <- NS(id)
  
  ModalWidgetUI(ns("getintbutton"))
  
}