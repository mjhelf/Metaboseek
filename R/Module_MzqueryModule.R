#' MzqueryModule
#' 
#' Use the \code{MassTools} and \code{Rdisop} packages to predict molecular formulas
#' 
#' @inherit MseekModules
#' 
#' @describeIn MzqueryModule Server logic
#' 
#' @return Returns its internalValues
#' 
#' @importFrom MassTools calcMF
#' @importFrom BiocParallel bpparam
#' @importFrom Rdisop initializeElements
#' 
#' @export 
MzqueryModule <- function(input,output, session, values,
                          reactives = reactive({list(mz = NULL)})){
###Enter mz textInput
  ns <- NS(session$ns(NULL))
  
  
  internalValues <- reactiveValues(mz = NULL,
                               sources = NULL,
                               set = NULL #copy of set() to check if set() has changed
  )

observe({
  if(!identical(internalValues$set, reactives())){
  internalValues$mz <- reactives()$mz

  #if(!identical(names(internalValues$sources), internalValues$sources)){
  internalValues$sources <- names(reactives()$mz)
  #}
  
  internalValues$set <- reactives()
  #print(internalValues$mz)
}
    })

output$mzUI <- renderUI({
  #print("ui")
  fluidPage(
    fluidRow(
      h3("Molecular formula prediction"),
      column(5,
             fluidRow(
               column(3,
                      selectizeInput(ns("selelements"), "Elements",
                                     choices = names(getOption("MassTools.elements")),
                                     multiple = T,
                                     selected =   values$GlobalOpts$mzquery.elements#c("C","H","N","O","P","S")
                                     )),
               column(2,
                      numericInput(ns("mzcharge"), "Charge", value = values$GlobalOpts$mzquery.mzcharge, step=1) 
               ),
               column(2,
                      numericInput(ns("mzppm"), "ppm", value = values$GlobalOpts$mzquery.mzppm, step=0.1)),
               column(2,
                      numericInput(ns("minUnsat"), "min. DBE", value = values$GlobalOpts$mzquery.minUnsat, step=1)),
               column(2,
                      numericInput(ns("maxUnsat"), "max. DBE", value = values$GlobalOpts$mzquery.maxUnsat, step=1))
               ),
             
             fluidRow(
               column(3,
                      textInput(ns("minelements"), label = "Minimum elements:", value = values$GlobalOpts$mzquery.minElements)
                      ),
               column(3,
                      textInput(ns("maxelements"), label = "Maximum elements:", value = values$GlobalOpts$mzquery.maxElements)),
                      
               column(3,
                      selectizeInput(ns("selparity"), "Parity", choices = list("either" = "either",
                                                                               "odd" = "o",
                                                                               "even" = "e"),
                                     selected = values$GlobalOpts$mzquery.parity)),
               column(3,
                      checkboxInput(ns("maxcounts"), label = "Element filter", value = values$GlobalOpts$mzquery.maxcounts))
               ),
             
             fluidRow(
               column(3,
                      checkboxInput(ns("valencefilter"), label = "Valence filters", value = values$GlobalOpts$mzquery.valencefilter)),
             column(3,
                    checkboxInput(ns("hcratio"), label = "H/C ratio", value = values$GlobalOpts$mzquery.hcratio)),
      column(3,
             checkboxInput(ns("moreratios"), label = "NOPS/C ratio", value = values$GlobalOpts$mzquery.moreratios)),
    column(3,
           checkboxInput(ns("elementheuristic"), label = "Element filter 2", value = values$GlobalOpts$mzquery.elementheuristic))

               
             ),
    fluidRow(
      column(3,
             selectizeInput(ns("source"), label = "source", choices = c(internalValues$sources, "custom") )),
      column(3,
             numericInput(ns("mzquery"), label = "custom m/z:", value = 0)),
      column(3,
             checkboxInput(ns("autoCalc"), label = "Calculate automatically", value = FALSE),
             htmlOutput(ns("mzInfo"))),
      column(2,
             actionButton(ns("mzButton"), "calculate")),
      column(2,
             actionButton(ns("mzButton2"), "calculate all"))
               
      )),
      
      column(7,
             rHandsontableOutput(ns('hot1')),
             htmlOutput(ns("mzInfo2"))
      )
    ))
}) 

#make the mz query settings available everywhere:
observeEvent(c(input$selelements, input$mzppm, input$mzcharge,
               input$minUnsat, input$maxUnsat,
               input$minelements, input$maxelements,
               input$selparity, input$maxcounts,
               input$valencefilter, input$hcratio,
               input$moreratios, input$elementheuristic
               ),{
  
                   
  if(length(input$selelements) >0 && (is.null(values$GlobalOpts$mzquery.elements) || input$selelements != values$GlobalOpts$mzquery.elements)){
  values$GlobalOpts$mzquery.elements <- input$selelements
  
                   }
  
  values$GlobalOpts$mzquery.mzppm <- input$mzppm
  values$GlobalOpts$mzquery.mzcharge <- input$mzcharge
  
  values$GlobalOpts$mzquery.minUnsat <- input$minUnsat
  values$GlobalOpts$mzquery.maxUnsat <- input$maxUnsat
  
  values$GlobalOpts$mzquery.minElements <- input$minelements
  values$GlobalOpts$mzquery.maxElements <- input$maxelements
  
  
  values$GlobalOpts$mzquery.parity <- input$selparity
  values$GlobalOpts$mzquery.maxcounts <- input$maxcounts
  
  values$GlobalOpts$mzquery.valencefilter <- input$valencefilter
  values$GlobalOpts$mzquery.hcratio <- input$hcratio
  
  values$GlobalOpts$mzquery.moreratios <- input$moreratios
  values$GlobalOpts$mzquery.elementheuristic <- input$elementheuristic
  
})

observeEvent(values$GlobalOpts$mzquery.elements,{
             if(length(values$GlobalOpts$mzquery.elements) >0 
                && (is.null(values$GlobalOpts$mzquery.elements))){
    values$GlobalOpts$mzquery.InitializedElements <- initializeElements(input$selelements)
             }
})


output$mzInfo <- renderUI({
  #print(internalValues$mz[[input$source]])
   p(paste0("Calculate sum formulas for m/z ", 
          if(length(input$source) > 0 && input$source != "custom"){round(as.numeric(internalValues$mz[[input$source]]),5)}else{round(as.numeric(input$mzquery),5)}
         ))
})

output$mzInfo2 <- renderUI({
  #print(internalValues$mz[[input$source]])
  if(!is.null(mzauto()) && !is.null(mzauto()$table)){
  p("Showing results for m/z ",
           strong(round(as.numeric(mzauto()$mz),5)),
           "from ", mzauto()$source)
           
  }else{
    p("No molecular formulas found for current query!")
    
  }
})

mzauto <- reactive({
  if(length(input$selelements) != 0 && length(input$source) > 0 && input$source != "custom" && input$autoCalc){
    mzi <- as.numeric(internalValues$mz[[input$source]])
    
   
    
    return(list(table = calcMF(mzi, 
                               summarize = F,  
                               z = input$mzcharge,
                               ppm = input$mzppm,
                               elements = values$GlobalOpts$mzquery.InitializedElements,
                               BPPARAM = NULL,#bpparam(),
                               Filters = list(DBErange = c(input$minUnsat,input$maxUnsat),
                                              minElements = input$minelements,
                                              maxElements = input$maxelements,
                                              parity = if(input$selparity == "either"){NULL}else{input$selparity},
                                              maxCounts = input$maxcounts,
                                              SENIOR3 = if(input$valencefilter){0}else{NULL},
                                              HCratio = input$hcratio,
                                              moreRatios = input$moreratios,
                                              elementHeuristic = input$elementheuristic)),
                # massquery(mzi,
                #                 elem = input$mzspace,
                #                 ppm= input$mzppm,
                #                 charge = input$mzcharge,
                #                 IntegerSaturation = input$integUnsat,
                #                 minUnsat = input$minUnsat,
                #                 maxUnsat = input$maxUnsat),
                mz = mzi,
                source = input$source))
  }
  else{
    return(mztab())
  }
  
})

#get sum formula table from ChemCalc
mztab <- eventReactive(input$mzButton,{
  #print(input$source)
  if(length(input$selelements) != 0 && length(input$source) > 0){
    mzi <- if(input$source != "custom"){as.numeric(internalValues$mz[[input$source]])}else{as.numeric(input$mzquery)}
  
   
    
    return(list(table = calcMF(mzi, 
                               summarize = F,  
                               z = input$mzcharge,
                               elements = values$GlobalOpts$mzquery.InitializedElements,
                               ppm = input$mzppm,
                               BPPARAM = NULL,#bpparam(),
                               Filters = list(DBErange = c(input$minUnsat,input$maxUnsat),
                                              minElements = input$minelements,
                                              maxElements = input$maxelements,
                                              parity = if(input$selparity == "either"){NULL}else{input$selparity},
                                              maxCounts = input$maxcounts,
                                              SENIOR3 = if(input$valencefilter){0}else{NULL},
                                              HCratio = input$hcratio,
                                              moreRatios = input$moreratios,
                                              elementHeuristic = input$elementheuristic)),
                  
                  # massquery(mzi,
                  #                 elem = input$mzspace,
                  #                 ppm= input$mzppm,
                  #                 charge = input$mzcharge,
                  #                 IntegerSaturation = input$integUnsat,
                  #                 minUnsat = input$minUnsat,
                  #                 maxUnsat = input$maxUnsat),
                mz = mzi,
                source = input$source))
  }
    })

observeEvent(input$mzButton2,{
  #print(input$source)
 # if(length(input$source) > 0){
   
  tryCatch({
    withProgress(message = 'Please wait!', detail = "calculating molecular formulas", value = 0.5, {
    
      if(length(input$selelements) == 6
         && all(input$selelements %in% c("C","H","N","O","P","S"))){
        ele <-  NULL
      }else{
        ele <- initializeElements(input$selelements)
      }
    
        updateFT(values)
        
      res <- data.frame(predicted_MFs =if(!is.null(values$featureTables$tables[[values$featureTables$active]]$df$predicted_MFs)){
        values$featureTables$tables[[values$featureTables$active]]$df$predicted_MFs}
        else{
          character(nrow(values$featureTables$tables[[values$featureTables$active]]$df))
          },
                        stringsAsFactors = F)
    
    res$predicted_MFs[values$featureTables$row_filters] <- calcMF(values$featureTables$tables[[values$featureTables$active]]$df$mz[values$featureTables$row_filters], 
                               summarize = T,  
                               z = input$mzcharge,
                               ppm = input$mzppm,
                               BPPARAM = bpparam(),
                               elements = ele,
                               Filters = list(DBErange = c(input$minUnsat,input$maxUnsat),
                                              minElements = input$minelements,
                                              maxElements = input$maxelements,
                                              parity = if(input$selparity == "either"){NULL}else{input$selparity},
                                              maxCounts = input$maxcounts,
                                              SENIOR3 = if(input$valencefilter){0}else{NULL},
                                              HCratio = input$hcratio,
                                              moreRatios = input$moreratios,
                                              elementHeuristic = input$elementheuristic))
    
    values$featureTables$tables[[values$featureTables$active]] <- updateFeatureTable(values$featureTables$tables[[values$featureTables$active]],res)
    

    })
    showNotification(paste("Molecula formula calculation completed."), duration = 10, type = "message")
  },
  error = function(e){
    showNotification(paste("An error occured:", e), duration = NULL, type = "error")
    
  })
  
  
  
 # }
})


#render sum formula table
output$hot1 <- renderRHandsontable({
  if(!is.null(mzauto()) && !is.null(mzauto()$table)){
  #options(digits=8)
  rhandsontable(mzauto()$table,#[,which(colnames(mzauto()$table) != "info")],#format(mztab(),digits=10),
                #ftable1()[which(rownames(ftable1()) %in% input$selcol1),],
                readOnly = TRUE,
                contextMenu = FALSE,
                selectCallback = TRUE,
                height = 200,
              
                digits=8) %>%
    hot_cols(columnSorting = TRUE,format="0.000000")%>%
    hot_col("mz",format="0.000000")%>%
    hot_col("unsat",format="0.0")%>%
    hot_col("ppm",format="0.000")
  }
})

return(internalValues)

}

#' @describeIn MzqueryModule UI elements
#' @export 
MzqueryModuleUI <- function(id){
 ns <- NS(id)
 
  
htmlOutput(ns('mzUI'))

}



