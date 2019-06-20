#' FilterModule
#' 
#' Module to apply filters to a featureTable (UI). Has to be inside a 
#' MultiFilterModule, and will modify the parent modules internalValues, accessed as values$MultiFilter!
#' 
#' @inherit MseekModules
#' @param initValues set initial values for input fields
#' 
#' @return a reactivevalues object with its internalValues
#' 
#' @describeIn FilterModule server logic
#' 
#' @export 
FilterModule <- function(input,output, session,
                                    values = reactiveValues(featureTables = featureTables,
                                                            MultiFilter = MultiFilter),
                         static = list(lab = "Filter"),
                         initValues = list(active = F,
                                           filter = TRUE,
                                           colSelected = NULL,
                                           summary = NULL,
                                           numeric = T,
                                           minSel = NULL,
                                           maxSel = NULL,
                                           modeSel = NULL,
                                           txtSel = NULL,
                                           minSelInit = NULL,
                                           maxSelInit = NULL,
                                           modeSelInit = NULL,
                                           txtSelInit = NULL,
                                           loadingFilters = F)
){
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- ListToReactiveValues(initValues)
  
  #    reactive({paste0(df(),"!!!")})
  fu <- reactive({   df()})
  
  output$activeCheck <- renderUI({checkboxInput(ns('activecheck'), 'activate', value = internalValues$active)})
  
  observeEvent(input$activecheck,{
    
    internalValues$active <- input$activecheck
    values$MultiFilter$outdated <- T
    
    
  })
  
  #inp <- reactive({input$colSel})
  
  # output$insider <- renderPrint({print(summary(df()[,input$colSel])
  # )})
  
  output$colSel <- renderUI({
    
    tooltip <- if(internalValues$numeric){
      tryCatch({
      paste0("Numeric column, range:",
             round(min(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected])),3),
             " - ", 
             round(max(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected])),3),
             ", mean: ",
             round(mean(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected])),3),
             ", median: ",
             round(median(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected])),3)
      )},
      warning = function(w){paste("Error in calculation")},
      error = function(e){paste("Error in calculation")})
      }else{"This column does not contain numeric values."}
    
    div(title = tooltip,
    selectizeInput(ns('colsel'), static$lab,
                                            choices = values$MultiFilter$colnames,
                                            selected = if(is.null(internalValues$colSelected) || !internalValues$colSelected %in% values$MultiFilter$colnames){NULL}else{internalValues$colSelected},
                                            multiple = F)
    )
    }) 
  
  observeEvent(input$colsel,{
    
    internalValues$colSelected <- input$colsel
    values$MultiFilter$outdated <- T
    
  })
  
 
  observeEvent(internalValues$colSelected,{
    if(!internalValues$loadingFilters){
    #returns TRUE if the selected column is numeric
    internalValues$numeric <- !is.na(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[1,internalValues$colSelected]))
    if(length(internalValues$numeric) > 0 && internalValues$numeric){
    premin <- min(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected]))
    internalValues$minSelInit <- ifelse(premin < 0, premin*1.01, premin*0.99)
    
    premax <- max(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected]))
    internalValues$maxSelInit <- ifelse(premax < 0, premax*0.99, premax*1.01)
    }
    }else{
      internalValues$loadingFilters <- F
    }
  })
  
  
  #  if(!is.null(cond()){
  output$minSel <- renderUI({if(internalValues$numeric){column(3,numericInput(ns('minsel'), 'min.', 
                                                                           value = internalValues$minSelInit,
                                                                           min = NA,
                                                                           max = NA)
                                                               )
    }})
  
  observeEvent(input$minsel,{
    internalValues$minSel <- input$minsel
    values$MultiFilter$outdated <- T
    
  })
  
  output$maxSel <- renderUI({if(internalValues$numeric){column(3,numericInput(ns('maxsel'), 'max.',
                                                                           value = internalValues$maxSelInit,
                                                                           min = NA,
                                                                           max = NA)
                                                               )
    }})
  
  observeEvent(input$maxsel,{
    internalValues$maxSel <- input$maxsel
    values$MultiFilter$outdated <- T
    
  })
  
  output$modeSel <- renderUI({if(!internalValues$numeric){column(3,selectizeInput(ns('modesel'), 'condition',
                                                                               choices = c("contains", "does not contain", "is", "is not"),
                                                                               selected = internalValues$modeSelInit,
                                                                               multiple = F)
                                                                 )
    }})
  
  observeEvent(input$modesel,{
    internalValues$modeSel <- input$modesel
    values$MultiFilter$outdated <- T
    
  })
  
  output$txtSel <- renderUI({if(!internalValues$numeric){column(3,
                                                                textInput(ns('txtsel'), 'string',
                                                                         value = internalValues$txtSelInit)
                                                                )
    }})
  
  observeEvent(input$txtsel,{   
    internalValues$txtSel <- input$txtsel

    values$MultiFilter$outdated <- T
    
  })
  
  

    
  return(internalValues)
  
  
  
}

#' @describeIn FilterModule UI elements
#' @export
FilterModuleUI <- function(id){
  ns <- NS(id)
  if(!is.null(htmlOutput(ns('colSel')))){
    tagList(
      fluidRow(
        column(2,
               htmlOutput(ns('activeCheck'))
               #                   checkboxInput(ns('toggler'), 'activate')
        ),
        column(4,    
               htmlOutput(ns('colSel'))),
               htmlOutput(ns('minSel')),
               htmlOutput(ns('maxSel')),
               htmlOutput(ns('modeSel')),
                htmlOutput(ns('txtSel'))
      
    )
    )
  }
}
