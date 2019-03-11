#' PeakPickModule
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
PeakPickModule <- function(input,output, session,
                           values = reactiveValues(MSData = MSData,
                                                   featureTables = featureTables,
                                                   MainTable = MainTable)){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  
  output$keepColumns <- renderUI({
    
    selectizeInput(ns("keepcolumns"), "Keep columns:", 
                   selected = "comments",
                   choices = colnames(values$featureTables$tables[[values$featureTables$active]]$df),
                   multiple = T)
    
  })
  
  
  dialog <- callModule(MseekModalModule, "peakpickbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                           fluidRow(
                             p("A new Feature Table will be generated, based on the mz values in the currently active Feature Table, and peaks detected in all MS data files", strong("in the currently active MS Grouping layout."))
                           ),
                           fluidRow(
                             column(6,
                                    htmlOutput(ns("keepColumns"))
                             ),
                             column(6,
                                    checkboxInput(ns("getintensities"), "Get intensities", value = T)
                             )),
                           fluidRow( 
                             column(5,
                                    div(title = "local noise window in number of scans. This number of scans around a detected apex will be used to measure local noise levels. increase for long gradients/broad peaks.",
                                        numericInput(ns("noisewindow"), "local noise window", value = 20, min = 0)
                                    )),
                             column(5,
                                    div(title = "Retention time tolerance when merging peaks found in multiple files.",
                                        numericInput(ns("rttol"), "RT tolerance (sec.)", value = 3, min = 0)
                                    ))),
                             fluidRow( 
                               column(5,
                                      div(title = "m/z tolerance for peak detection AND intensity calculation",
                                          numericInput(ns("intensppm"), "m/z tolerance (ppm)", value = 5, min = 0)
                                      )),
                               column(5,
                                      div(title = "Minimum number of consecutive scans in a peak",
                                          numericInput(ns("minscan"), "min. scan number", value = 2, min = 0)
                                      )),
                             column(2,
                                    actionButton(ns("abutton"), "Go")
                             )
                           ),
                           fluidRow(
                             hr(),
                             h4("Peak intensity calculation options")),
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
                                      div(title = "Define the SUFFIX for the intensity columns. Column names will be filename__SUFFIX. WARNING: Columns with identical names will be replaced in the current table!",
                                          textInput(ns("intensSuffix"),"Intensity column suffix:", value = "XIC")
                                      ))
                              )
                           
                         ))      }),
                       static = list(tooltip = "Find peaks for the mz values in this table",
                                     title = "Find peaks", 
                                     label = "Find peaks",
                                     icon = icon("area-chart", lib = "font-awesome")))
  
  
  
  
  observeEvent(input$abutton,{
    tryCatch({
      withProgress(message = 'Please wait!', detail = "Finding peaks", value = 0.5, {
        
        METABOseek:::TableUpdateChunk()
        
        tabid <- paste0("table",length(values$featureTables$index))
        names(tabid) <- paste0("peakpick_", values$featureTables$tables[[values$featureTables$active]]$tablename)
        
        
        #newdf <- makeRTlist(values$featureTables$tables[[values$featureTables$active]]$df, values$MSData$data, retainColumns = input$keepcolumns)
        
        newdf <- makeRTlist2(df = values$featureTables$tables[[values$featureTables$active]]$df,
                             rawdata = values$MSData$data[values$MSData$layouts[[values$MSData$active]]$filelist],
                             ppm = input$intensppm,
                             retainColumns = input$keepcolumns, 
                             findProps = list(SN = 1,
                                              minwidth = (input$minscan + 2),
                                              localNoise = input$noisewindow,
                                              localNoiseFactor = 0.5,
                                              globalNoiseFactor = 0.5,
                                              extend = T),
                             mergeProps = list(rttol = input$rttol, minint = 0, minrelint = 0, topN = 100))
        
        if(is.null(newdf) || nrow(newdf) == 0){
          
          removeModal()
          showModal(
            modalDialog(
              p("No peaks were found for this feature table in the loaded MS data files.")
              ,
              title = "Peak picking failed!",
              easyClose = T,
              fade = F,
              size = "s",
              footer = modalButton("Ok") 
            ))
          
        }else if(input$getintensities){
            
          intens <- as.data.frame(lapply(bplapply(values$MSData$data[values$MSData$layouts[[values$MSData$active]]$filelist], 
                                                 METABOseek::exIntensities, 
                                                 mz = newdf$mz,
                                                 ppm = input$intensppm,
                                                 rtw = if(input$intensRangeCheck){data.frame(rtmin = newdf$rtmin-input$intensRTsec,
                                                                                             rtmax = newdf$rtmax+input$intensRTsec)}
                                                 else{data.frame(rtmin = newdf$rt-input$intensRTsec,
                                                                 rtmax = newdf$rt+input$intensRTsec)}, 
                                                 baselineSubtract = input$baselineCheck, 
                                                 areaMode = input$areaCheck,
                                                 
                                                 SN = NULL,
                                                 BPPARAM = SnowParam(workers = if(length(newdf$mz) > 10000){values$GlobalOpts$enabledCores}else{1}
                                                 )),
                                        unlist))
          
          colnames(intens) <- paste0(basename(values$MSData$layouts[[values$MSData$active]]$filelist),"__", input$intensSuffix)
          
          newdf <- updateDF(intens, newdf)
        }
          #values$featureTables$tables[[tabid]] <- values$featureTables$tables[[values$featureTables$active]]
          
          #may be unnecessarily complicated
          #values$featureTables$tables[[paste0("peakPicked_",values$featureTables$active)]] <- updateFeatureTable(values$featureTables$tables[[paste0("peakPicked_",values$featureTables$active)]],makeRTlist(values$featureTables$tables[[paste0("peakPicked_",values$featureTables$active)]]$df, values$MSData$data, retainColumns = NULL))
          
          
          #simpler:
          values$featureTables$tables[[tabid]] <- constructFeatureTable(df = newdf,
                                                                        tablename = names(tabid),
                                                                        anagrouptable = if(input$getintensities){data.frame(Column = colnames(intens),
                                                                                                                            Group = rep("G1", length(colnames(intens))),
                                                                                                                            stringsAsFactors = F)}else{NULL},
                                                                        editable = F)
          #values$featureTables$tables[[tabid]]$df <- newdf
          
          values$featureTables$index <- c( values$featureTables$index, tabid)
          values$featureTables$tableSwitch <- T
          values$featureTables$active <- unname(tabid)
          values$featureTables$row_filters <- TRUE
          
          
          removeModal()
          showNotification(paste("Peak finding completed."), duration = 0, type = "message")
          
        }
      )},
      error = function(e){
        
        showNotification(paste("An error occured: ", e), duration = 0, type = "error")
        
        
      })
    
  })
  
  
  return(internalValues)
}

#' PeakPickModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
PeakPickModuleUI <- function(id)
{
  ns <- NS(id)
  
  MseekModalModuleUI(ns("peakpickbutton"))
  
}