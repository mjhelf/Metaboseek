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
      p("A new Feature Table will be generated, based on the mz values in the currently active Feature Table, and peaks detected in all currently loaded MS data files.")
      ),
      fluidRow(
        column(4,
      htmlOutput(ns("keepColumns"))
      ),
      column(2,
      checkboxInput(ns("getintensities"), "Get intensities", value = T)
      ),
      column(1,
      actionButton(ns("abutton"), "Go")
      )
      )
      
    ) )     }),
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
    

    newdf <- makeRTlist(values$featureTables$tables[[values$featureTables$active]]$df, values$MSData$data, retainColumns = input$keepcolumns)
    
    newdf <- do.call(rbind, newdf)
    
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
      
    }else{
    
    if(input$getintensities){
    
    for(i in seq(length(values$MSData$data))){

    newdf[[paste0(basename(names(values$MSData$data)[i]),"__XIC")]] <- exIntensities(rawfile= values$MSData$data[[i]],
                               mz = newdf$mz,
                               ppm = 5,
                               rtw= data.frame(newdf$rtmin-5,newdf$rtmax+5))
    
    }
    }

   #values$featureTables$tables[[tabid]] <- values$featureTables$tables[[values$featureTables$active]]
    
    #may be unnecessarily complicated
    #values$featureTables$tables[[paste0("peakPicked_",values$featureTables$active)]] <- updateFeatureTable(values$featureTables$tables[[paste0("peakPicked_",values$featureTables$active)]],makeRTlist(values$featureTables$tables[[paste0("peakPicked_",values$featureTables$active)]]$df, values$MSData$data, retainColumns = NULL))
    
       
    #simpler:
    values$featureTables$tables[[tabid]] <- constructFeatureTable(df = newdf,
                                                                  tablename = names(tabid),
                                                                  anagrouptable = if(input$getintensities){values$MSData$layouts[[values$MSData$active]]$rawgrouptable}else{NULL})
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