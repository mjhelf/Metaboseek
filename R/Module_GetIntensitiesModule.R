#' GetIntensitiesModule
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
GetIntensitiesModule <- function(input,output, session,
                           values = reactiveValues(MSData = MSData,
                                                   featureTables = featureTables,
                                                   GlobalOpts = GlobalOpts)){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  

  
  
  dialog <- callModule(MseekModalModule, "getintbutton",
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
        METABOseek:::TableUpdateChunk()
        

        newdf <- as.data.frame(lapply(bplapply(values$MSData$data[values$MSData$layouts[[values$MSData$active]]$filelist], 
                                              METABOseek::exIntensities, 
                                              mz = values$featureTables$tables[[values$featureTables$active]]$df$mz,
                                              ppm = input$intensppm,
                                              rtw = if(input$intensRangeCheck){data.frame(rtmin = values$featureTables$tables[[values$featureTables$active]]$df$rtmin-input$intensRTsec,
                                                                                            rtmax = values$featureTables$tables[[values$featureTables$active]]$df$rtmax+input$intensRTsec)}
                                              else{data.frame(rtmin = values$featureTables$tables[[values$featureTables$active]]$df$rt-input$intensRTsec,
                                                              rtmax = values$featureTables$tables[[values$featureTables$active]]$df$rt+input$intensRTsec)}, 
                                              baselineSubtract = input$baselineCheck, 
                                              areaMode = input$areaCheck,
                                              
                                              SN = NULL,
                                              BPPARAM = SnowParam(workers = if(length(values$featureTables$tables[[values$featureTables$active]]$df$mz) > 10000){values$GlobalOpts$enabledCores}else{1}
                                              )),
                                     unlist))
        
        colnames(newdf) <- paste0(basename(values$MSData$layouts[[values$MSData$active]]$filelist),"__", input$intensSuffix)
        
     
        
     values$featureTables$tables[[values$featureTables$active]] <- updateFeatureTable(values$featureTables$tables[[values$featureTables$active]],newdf)

        
      })
       
      
          
          
          removeModal()
          showNotification(paste("Completed peak intensity calculation."), duration = 0, type = "message")
          
        }
      ,
      error = function(e){
        
        showNotification(paste("An error occured: ", e), duration = 0, type = "error")
        
        
      })
    
  })
  
  
  return(internalValues)
}

#' GetIntensitiesModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
GetIntensitiesModuleUI <- function(id)
{
  ns <- NS(id)
  
  MseekModalModuleUI(ns("getintbutton"))
  
}