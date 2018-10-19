#' MultiFilterModule
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
MultiFilterModule <- function(input,output, session,
                              values = reactiveValues(featureTables = featureTables,
                                                      MainTable = MainTable),
                              static = list(lab = "Filter")
){
  
  ns <- NS(session$ns(NULL))
  
  looping <- function(lls){
    if(length(lls) == 1){return(lls[[1]])}
    
    ret <- lls[[1]]
    for(i in c(2:length(lls))){
      
      ret <- ret & lls[[i]]
      
    }
    return(ret)
  }
  
  Filter1 <-  callModule(FilterModule, "filter1", values = reactiveValues(featureTables = values$featureTables,
                                                                          MultiFilter = internalValues))
  
  
  output$applyFilter <- renderUI({mActionButton(ns("applyfilter"), "Apply Filters", red = internalValues$outdated)})
  
  observeEvent(input$applyfilter,{
    
    internalValues$applyButton <- internalValues$applyButton + 1
    
    #TableUpdateChunk()
    
    logilist <- list()

    for(i in grep("Filter", names(internalValues), value = T)){
      
      if(length(internalValues[[i]]$colSelected) == 0 || !internalValues[[i]]$colSelected %in% internalValues$colnames){
        internalValues[[i]]$active <- F
      }
      
      if(length(internalValues[[i]]$active) !=0 && internalValues[[i]]$active){
        if(internalValues[[i]]$numeric){
          
          
          
          internalValues[[i]]$filter <- (values$featureTables$tables[[values$featureTables$active]]$df[,internalValues[[i]]$colSelected] >= as.numeric(internalValues[[i]]$minSel)
                                    & values$featureTables$tables[[values$featureTables$active]]$df[,internalValues[[i]]$colSelected] <= as.numeric(internalValues[[i]]$maxSel))
        }else{

          
          if(input$modeSel=="contains"){
            internalValues[[i]]$filter <- grepl(internalValues[[i]]$txtSel,as.character(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues[[i]]$colSelected]))
          }else if(input$modeSel=="does not contain"){
            internalValues[[i]]$filter <- !grepl(internalValues[[i]]$txtSel,as.character(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues[[i]]$colSelected]))
          }else if(input$modeSel=="is not"){
            internalValues[[i]]$filter <- ! (as.character(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues[[i]]$colSelected]) == internalValues[[i]]$txtSel)
            
          }
          #if(input$modeSel=="is"){
          else{
            internalValues[[i]]$filter <- as.character(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues[[i]]$colSelected]) == internalValues[[i]]$txtSel
          }
        }
      }else{
        internalValues[[i]]$filter <- T
      }
      
      #looping function not really necessary anymore
      logilist[[i]] <- internalValues[[i]]$filter
      
    }

    values$featureTables$row_filters <- looping(logilist)
    internalValues$outdated <- F
  })
  
  observeEvent(input$addFilter,{
    
    for(i in grep("Filter", names(internalValues), value = T)){
      #Make sure the Filter modules do not forget their values when a new one is added and the Filter UIs are rerendered
      internalValues[[i]]$minSelInit <- internalValues[[i]]$minSel
      internalValues[[i]]$maxSelInit <- internalValues[[i]]$maxSel
      internalValues[[i]]$txtSelInit <- internalValues[[i]]$txtSel
      internalValues[[i]]$modeSelInit <- internalValues[[i]]$modeSel

    }
    
    internalValues$numFils <- internalValues$numFils + 1
    
    internalValues[[paste0("Filter", internalValues$numFils)]] <- callModule(FilterModule,
                                                                             paste0("filter", internalValues$numFils),
                                                                             values = reactiveValues(featureTables = values$featureTables,
                                                                                                     MultiFilter = internalValues))
    
  })
  
  internalValues <- reactiveValues(numFils = 1,
                                   applyButton = 0,
                                   Filter1 = Filter1,
                                   colnames = NULL,
                                   outdated = F)
  
  output$FilterUIs <- renderUI({
    lapply(seq(internalValues$numFils), function(i){
      FilterModuleUI(ns(paste0("filter",i)))
    })
    
  })
  
  
  
  observeEvent(values$featureTables$tables[[values$featureTables$active]]$df,{
    internalValues$outdated <- T
    internalValues$colnames <- colnames(values$featureTables$tables[[values$featureTables$active]]$df)
  })
  
  return(internalValues)
  
}

#' MultiFilterModuleUI
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param id
#' 
#' @export
MultiFilterModuleUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(column(4,
      htmlOutput(ns("applyFilter"))
      ),column(4,
      actionButton(ns("addFilter"), "Add Filter")
      )
    ),
    htmlOutput(ns("FilterUIs"))
  )
  
}