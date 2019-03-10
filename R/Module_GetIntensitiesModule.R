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
                                                   MainTable = MainTable)){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  
  output$keepColumns <- renderUI({
    
    selectizeInput(ns("keepcolumns"), "Keep columns:", 
                   selected = "comments",
                   choices = colnames(values$featureTables$tables[[values$featureTables$active]]$df),
                   multiple = T)
    
    
  })
  
  
  dialog <- callModule(MseekModalModule, "getintbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                         
                           fluidRow(
                             p("Calculate the peak intensities for the current feature table using all MS data files that are", strong("in the currently active MS Grouping layout."))
                           ),
                           fluidRow(
                             column(6,
                                    div(title = "Use rtmin and rtmax of each feature for intensity calculation. 
If this is selected, the rt window setting for peak intensity calculation starts at these reported outsides of the peak instead of at its apex, making the RT window width broader and variable between peaks.",
                                        checkboxInput(ns("intensRangeCheck"), "Use rtmin/rtmax", value  = TRUE)
                                    )),
                             column(6,
                                    div(title = "RT window size (+/- in seconds from either a features rt value or its rtmin and rtmax values",
                                        numericInput(ns("intensRTsec"),"Retention time window (seconds)", value = 5, min = 0)
                                    ))),
                           fluidRow(
                             column(6,
                                    div(title = "m/z tolerance for intensity calculation",
                                        numericInput(ns("intensppm"), "m/z tolerance (ppm)", value = 5, min = 0)
                                    )),
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
  
  
  
  
  observeEvent(input$abutton,{
    tryCatch({
      withProgress(message = 'Please wait!', detail = "Calculating peak intensities", value = 0.5, {
        
       for(i in values$MSData$layouts[[values$MSData$active]]$filelist){
              
              newdf[[paste0(basename(i),"__", input$intensSuffix)]] <- exIntensities(rawfile= values$MSData$data[[i]],
                                                                                     mz = newdf$mz,
                                                                                     ppm = input$intensppm,
                                                                                     rtw= if(input$intensRangeCheck){data.frame(rtmin = newdf$rtmin-input$intensRTsec,
                                                                                                                                rtmax = newdf$rtmax+input$intensRTsec)}
                                                                                     else{data.frame(rtmin = newdf$rt-input$intensRTsec,
                                                                                                     rtmax = newdf$rt+input$intensRTsec)})
              
            }
          }
          
          #values$featureTables$tables[[tabid]] <- values$featureTables$tables[[values$featureTables$active]]
          
          #may be unnecessarily complicated
          #values$featureTables$tables[[paste0("peakPicked_",values$featureTables$active)]] <- updateFeatureTable(values$featureTables$tables[[paste0("peakPicked_",values$featureTables$active)]],makeRTlist(values$featureTables$tables[[paste0("peakPicked_",values$featureTables$active)]]$df, values$MSData$data, retainColumns = NULL))
          
          
          #simpler:
          values$featureTables$tables[[tabid]] <- constructFeatureTable(df = newdf,
                                                                        tablename = names(tabid),
                                                                        anagrouptable = if(input$getintensities){values$MSData$layouts[[values$MSData$active]]$rawgrouptable}else{NULL},
                                                                        editable = F)
          #values$featureTables$tables[[tabid]]$df <- newdf
          
          values$featureTables$index <- c( values$featureTables$index, tabid)
          values$featureTables$tableSwitch <- T
          values$featureTables$active <- unname(tabid)
          values$featureTables$row_filters <- TRUE
          
          
          removeModal()
          showNotification(paste("Peak finding completed."), duration = 0, type = "message")
          
        }}
      )},
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