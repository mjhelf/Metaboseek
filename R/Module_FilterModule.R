#' FilterModule
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
FilterModule <- function(input,output, session,
                                    values = reactiveValues(featureTables = featureTables,
                                                            MultiFilter = MultiFilter),
                         static = list(lab = "Filter")
){
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(active = F,
                                   filter = TRUE,
                                   colSelected = NULL,
                                   summary = NULL,
                                   numeric = T,
                                   minSel = NULL,
                                   maxSel = NULL,
                                   modeSel = NULL,
                                   txtSel = NULL
                                   )
  
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
      warning = function(w){paste("Error in calculation")})
      }else{"This column does not contain numeric values."}
    
    div(title = tooltip,
    selectizeInput(ns('colsel'), static$lab,
                                            choices = values$MultiFilter$colnames,
                                            selected = internalValues$colSelected,
                                            multiple = F)
    )
    }) 
  
  observeEvent(input$colsel,{
    
    internalValues$colSelected <- input$colsel
    values$MultiFilter$outdated <- T
    
  })
  
 
  observeEvent(internalValues$colSelected,{
    #returns TRUE if the selected column is numeric
    internalValues$numeric <- !is.na(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[1,internalValues$colSelected]))
    if(length(internalValues$numeric) > 0 && internalValues$numeric){
    premin <- min(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected]))
    internalValues$minSel <- ifelse(premin < 0, premin*1.01, premin*0.99)
    
    premax <- max(as.numeric(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected]))
    internalValues$maxSel <- ifelse(premax < 0, premax*0.99, premax*1.01)
    
    
    }
  })
  
  
  #  if(!is.null(cond()){
  output$minSel <- renderUI({if(internalValues$numeric){column(3,numericInput(ns('minsel'), 'min.', 
                                                                           value = internalValues$minSel,
                                                                           min = NA,
                                                                           max = NA)
                                                               )
    }})
  
  observeEvent(input$minsel,{
    internalValues$minSel <- input$minsel
    values$MultiFilter$outdated <- T
    
  })
  
  output$maxSel <- renderUI({if(internalValues$numeric){column(3,numericInput(ns('maxsel'), 'max.',
                                                                           value = internalValues$maxSel,
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
                                                                               selected =internalValues$modeSel,
                                                                               multiple = F)
                                                                 )
    }})
  
  observeEvent(input$modesel,{
    internalValues$modeSel <- input$modesel
    values$MultiFilter$outdated <- T
    
  })
  
  output$txtSel <- renderUI({if(!internalValues$numeric){column(3,
                                                                textInput(ns('txtsel'), 'string',
                                                                         value = internalValues$txtSel)
                                                                )
    }})
  
  observeEvent(input$txtsel,{
    internalValues$txtSel <- input$txtsel
    values$MultiFilter$outdated <- T
    
  })
  
  
  observe({
    if(length(internalValues$active) !=0 && internalValues$active){
      if(internalValues$numeric){
        internalValues$filter <- (values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected] >= as.numeric(internalValues$minSel)
                                  & values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected] <= as.numeric(internalValues$maxSel))
      }else{
        if(input$modeSel=="contains"){
          internalValues$filter <- grepl(internalValues$txtSel,as.character(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected]))
        }else if(input$modeSel=="does not contain"){
          internalValues$filter <- !grepl(internalValues$txtSel,as.character(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected]))
        }else if(input$modeSel=="is not"){
          internalValues$filter <- ! (as.character(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected]) == internalValues$txtSel)
          
        }
        #if(input$modeSel=="is"){
        else{
          internalValues$filter <- as.character(values$featureTables$tables[[values$featureTables$active]]$df[,internalValues$colSelected]) == internalValues$txtSel
      }
      }
    }else{
      internalValues$filter <- T
    }
    })
    
  return(internalValues)
  
  
  
}

#' FilterModuleUI
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param id
#' 
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
