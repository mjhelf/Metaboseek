#' MultiFilterModule
#' 
#' Module to apply filters to the active \code{\link{FeatureTable}}.
#' 
#' @inherit MseekModules
#' 
#' @return Returns its internalValues
#' 
#' @describeIn MultiFilterModule Server logic
#' 
#' @export 
MultiFilterModule <- function(input,output, session, values,
                              static = list(lab = "Filter",
                                            modFeatureTable = T,
                                            name = NULL,
                                            activate = F)
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
    
    #let's try if this causes problems here:
    updateFT(values)
    
    logilist <- list()
    logiresult <- TRUE
    
    for(i in grep(static$lab, names(internalValues), value = T)){
      
      if(length(internalValues[[i]]$colSelected) == 0 || !internalValues[[i]]$colSelected %in% internalValues$colnames){
        internalValues[[i]]$active <- F
      }
      
      if(length(internalValues[[i]]$active) !=0 && internalValues[[i]]$active){
        if(internalValues[[i]]$numeric){
          
          
          
          internalValues[[i]]$filter <- (FeatureTable(values)$df[,internalValues[[i]]$colSelected] >= as.numeric(internalValues[[i]]$minSel)
                                         & FeatureTable(values)$df[,internalValues[[i]]$colSelected] <= as.numeric(internalValues[[i]]$maxSel))
        }else{
          
          
          if(!is.null(internalValues[[i]]$modeSel) && internalValues[[i]]$modeSel=="contains"){
            internalValues[[i]]$filter <- grepl(internalValues[[i]]$txtSel,
                                                as.character(FeatureTable(values)$df[,internalValues[[i]]$colSelected]),
                                                fixed = T)
          }else if(!is.null(internalValues[[i]]$modeSel) && internalValues[[i]]$modeSel=="does not contain"){
            internalValues[[i]]$filter <- !grepl(internalValues[[i]]$txtSel,
                                                 as.character(FeatureTable(values)$df[,internalValues[[i]]$colSelected]),
                                                 fixed = T)
          }else if(!is.null(internalValues[[i]]$modeSel) && internalValues[[i]]$modeSel=="is not"){
            internalValues[[i]]$filter <- ! (as.character(FeatureTable(values)$df[,internalValues[[i]]$colSelected]) == internalValues[[i]]$txtSel)
            
          }
          #if(input$modeSel=="is"){
          else{
            internalValues[[i]]$filter <- as.character(FeatureTable(values)$df[,internalValues[[i]]$colSelected]) == internalValues[[i]]$txtSel
          }
        }
        
        if(internalValues[[i]]$excludeNAs){
          #working under the assumption that NA values from the column in numeric, and in some cases (is, is not) in character,
          #are passed into the logical() 
        internalValues[[i]]$filter[is.na(internalValues[[i]]$filter)] <- FALSE
        }else{
        internalValues[[i]]$filter[is.na(internalValues[[i]]$filter)] <- TRUE
        }
        
      }else{
        internalValues[[i]]$filter <- T
      }
      
      #looping function not really necessary anymore
      logilist[[i]] <- internalValues[[i]]$filter

      #TODO: build this, probably need logilist again to group filters together      
      # if(!length(internalValues[[i]]$operator) || internalValues[[i]]$operator == "AND"){
      # logiresult <- logiresult & internalValues[[i]]$filter
      # }else{
      #   logiresult <- logiresult | internalValues[[i]]$filter
      # }
      
      
    }
    
    if(static$modFeatureTable){
    values$featureTables$row_filters <- looping(logilist)
    }else{
      internalValues$results <- looping(logilist)
    }
    
    internalValues$outdated <- F
    
    #main reason for using static$lab instead of "Filter"
    for(i in grep(static$lab, names(internalValues), value = T)){
      #Make sure the Filter modules do not forget their values when a new one is added and the Filter UIs are rerendered
      
      if(internalValues[[i]]$numeric){
        values$featureTables$Filters$filterSet[[i]] <- list(active = internalValues[[i]]$active,
                                                            colSelected = internalValues[[i]]$colSelected,
                                                            numeric = internalValues[[i]]$numeric,
                                                            minSelInit = internalValues[[i]]$minSel,
                                                            maxSelInit = internalValues[[i]]$maxSel,
                                                            excludeNAs = internalValues[[i]]$excludeNAs)
        
      }
      else{
        values$featureTables$Filters$filterSet[[i]] <- list(active = internalValues[[i]]$active,
                                                            colSelected = internalValues[[i]]$colSelected,
                                                            numeric = internalValues[[i]]$numeric,
                                                            txtSelInit = internalValues[[i]]$txtSel,
                                                            modeSelInit = internalValues[[i]]$modeSel,
                                                            excludeNAs = internalValues[[i]]$excludeNAs)
      }
    }
    values$featureTables$Filters$filteredTable <- values$featureTables$active
    
  })
  
  observeEvent(input$addFilter,{
    
    for(i in grep(static$lab, names(internalValues), value = T)){
      #Make sure the Filter modules do not forget their values when a new one is added and the Filter UIs are rerendered
      internalValues[[i]]$minSelInit <- internalValues[[i]]$minSel
      internalValues[[i]]$maxSelInit <- internalValues[[i]]$maxSel
      internalValues[[i]]$txtSelInit <- internalValues[[i]]$txtSel
      internalValues[[i]]$modeSelInit <- internalValues[[i]]$modeSel
      
    }
    
    internalValues$numFils <- internalValues$numFils + 1
    
    internalValues[[paste0(static$lab, internalValues$numFils)]] <- callModule(FilterModule,
                                                                             paste0("filter", internalValues$numFils),
                                                                             values = reactiveValues(featureTables = values$featureTables,
                                                                                                     MultiFilter = internalValues))
    
  })
  
  internalValues <- reactiveValues(numFils = 1,
                                   applyButton = 0,
                                   colnames = NULL,
                                   outdated = F,
                                   active = F,
                                   name = static$name)
  
  internalValues[[paste0(static$lab, 1)]] <- Filter1
  
  
  output$FilterUIs <- renderUI({
    lapply(seq(internalValues$numFils), function(i){
      FilterModuleUI(ns(paste0("filter",i)))
    })
    
  })
  
  output$optionalHeader <- renderUI({
    if(!is.null(static$name) && static$activate){
    fluidRow(
      column(3,
             checkboxInput(ns("activate"), "activate", value = internalValues$active) ),
      column(6, 
             textInput(ns("filname"), "Filter name", value = internalValues$name)
             ),
      column(3,
             actionButton(ns("namebutton"), "Change name")
             )
    )
    }
  })
  
  observeEvent(input$namebutton,{
    internalValues$name <- input$filname
  })
  
  observeEvent(input$activate,{
    internalValues$active <- input$activate
  })
  
  
  observeEvent(FeatureTable(values)$df,{
    internalValues$outdated <- T
    internalValues$colnames <- colnames(FeatureTable(values)$df)
    values$featureTables$Filters$filterSet <- list()
    values$featureTables$Filters$filteredTable <- values$featureTables$active
  })
  
  
  observeEvent(values$featureTables$loadedFilters,{
    if(!is.null(values$featureTables$loadedFilters)){
      
      #first check how many Filters are already available and add Filter modules as needed
      internalValues$numFils <- length(grep(static$lab, names(values$featureTables$loadedFilters$filterSet)))      
      #if(internalValues$numFils < length(values$featureTables$loadedFilters$filterSet)){
        
        for(i in seq(internalValues$numFils)){
        

        internalValues[[paste0(static$lab, i)]] <- callModule(FilterModule,
                                                                                 paste0("filter", i),
                                                                                 values = reactiveValues(featureTables = values$featureTables,
                                                                                                         MultiFilter = internalValues),
                                                            initValues = list(active = values$featureTables$loadedFilters$filterSet[[i]]$active,
                                                                              filter = TRUE,
                                                                              colSelected = values$featureTables$loadedFilters$filterSet[[i]]$colSelected,
                                                                              summary = NULL,
                                                                              numeric = values$featureTables$loadedFilters$filterSet[[i]]$numeric,
                                                                              minSel = NULL,
                                                                              maxSel = NULL,
                                                                              modeSel = NULL,
                                                                              txtSel = NULL,
                                                                              minSelInit = values$featureTables$loadedFilters$filterSet[[i]]$minSelInit,
                                                                              maxSelInit = values$featureTables$loadedFilters$filterSet[[i]]$maxSelInit,
                                                                              modeSelInit = values$featureTables$loadedFilters$filterSet[[i]]$modeSelInit,
                                                                              txtSelInit = values$featureTables$loadedFilters$filterSet[[i]]$txtSelInit,
                                                                              loadingFilters = T)
                                                            )
        }
      
      
      values$featureTables$loadedFilters$modulesAvailable <- internalValues$numFils
      values$featureTables$loadedFilters <- NULL
      
    }
    })
  
  # observeEvent(values$featureTables$loadedFilters$modulesAvailable,{
  #   
  #   if(!is.null(values$featureTables$loadedFilters$modulesAvailable) && values$featureTables$loadedFilters$modulesAvailable == length(grep("Filter", names(internalValues)))){
  # 
  #     #now feed in the information from the loadedFilter:
  #     for(i in seq(length(values$featureTables$loadedFilters$filterSet))){
  #       for(entry in names(values$featureTables$loadedFilters$filterSet[[i]])){
  #         
  #         internalValues[[paste0("Filter", i)]][[entry]]  <- values$featureTables$loadedFilters$filterSet[[i]][[entry]]
  #         
  #         
  #       }
  #       
  #     }
  #     
  #     print(internalValues$Filter1)   
  #     values$featureTables$loadedFilters <- NULL
  #   }
  #   else{
  #     print(paste0("available: ",length(grep("Filter", names(internalValues))),"/", values$featureTables$loadedFilters$modulesAvailable))
  #   }
  # })
      
      return(internalValues)
      
}

#' @describeIn MultiFilterModule UI elements
#' @export
MultiFilterModuleUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    htmlOutput(ns("optionalHeader")),
    fluidRow(column(4,
                    htmlOutput(ns("applyFilter"))
    ),column(4,
             actionButton(ns("addFilter"), "Add Filter")
    )
    ),
    htmlOutput(ns("FilterUIs"))
  )
  
}