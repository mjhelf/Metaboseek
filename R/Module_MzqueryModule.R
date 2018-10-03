#' MzqueryModule
#' 
#' Chemcalc mz query module
#' server module to get molecular formula predictions from chemcalc
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param tag id to be used in ns()
#' @param set Import data from the shiny session
#' 
#' @export 
MzqueryModule <- function(input,output, session, tag, 
                          set = list(search = list(elements = NULL,
                                                                          mz = NULL, 
                                                                          data = NULL) # the entire spectrum data for isotope matching
                                                                 )){
###Enter mz textInput
  ns <- NS(tag)
selections <- reactiveValues(search = list(            elements = "C0-100H0-202N0-15O0-20",
                                                      mz = NULL, 
                                                      data = NULL),
                             sources = NULL,
                                          set = NULL #copy of set() to check if set() has changed
)

observe({
  if(!identical(selections$set, set())){
  selections$search$elements <- set()$search$elements
  selections$search$mz <- set()$search$mz
  selections$search$data <- set()$search$data
  
  #if(!identical(names(selections$sources), selections$sources)){
  selections$sources <- names(set()$search$mz)
  #}
  
  selections$set <- set()
  #print(selections$search$mz)
}
    })

output$mzUI <- renderUI({
  #print("ui")
  fluidPage(
    fluidRow(
      h3("Molecular formula prediction"),
      column(4,
             fluidRow(
               column(6,
                      textInput(ns("mzspace"), label = "Select elements:", value = "C0-100H0-202N0-15O0-20")
                      ),
               column(6,
                      checkboxInput(ns("integUnsat"), label = "only integer unsaturation", value = FALSE))
               ),
             fluidRow(
               column(6,
                      numericInput(ns("mzcharge"), "Charge", 1, step=1) 
               ),
               column(6,
                      numericInput(ns("mzppm"), "ppm", 5, step=0.1))
               ),
             fluidRow(
               column(6,
                      numericInput(ns("minUnsat"), "min. unsaturation", 0, step=1)),
               column(6,
                      numericInput(ns("maxUnsat"), "max. unsaturation", 40, step=1))
               
             )),
      column(3,
             selectizeInput(ns("source"), label = "source", choices = c(selections$sources, "custom") ),
             textInput(ns("mzquery"), label = "custom m/z:", value = 0),
             htmlOutput(ns("mzInfo")),
             fluidRow(
               column(4,
                      actionButton(ns("mzButton"), "calculate")),
               column(8,
                      checkboxInput(ns("autoCalc"), label = "Calculate automatically", value = FALSE))
             )
      ),
      
      column(5,
             rHandsontableOutput(ns('hot1')),
             htmlOutput(ns("mzInfo2")),
             a("Search uses the chemcalc web service", href = "http://www.chemcalc.org/", target = "_blank")
      )
    ))
}) 

output$mzInfo <- renderUI({
  #print(selections$search$mz[[input$source]])
   p(paste0("Calculate sum formulas for m/z ", 
          if(length(input$source) > 0 && input$source != "custom"){round(as.numeric(selections$search$mz[[input$source]]),5)}else{round(as.numeric(input$mzquery),5)}
         ))
})

output$mzInfo2 <- renderUI({
  #print(selections$search$mz[[input$source]])
  if(!is.null(mzauto())){
  p("Showing results for m/z ",
           strong(round(as.numeric(mzauto()$mz),5)),
           "from ", mzauto()$source)
           
  }
})

mzauto <- reactive({
  if(length(input$source) > 0 && input$source != "custom" && input$autoCalc){
    mzi <- as.numeric(selections$search$mz[[input$source]])
    
    return(list(table = massquery(mzi,
                                  elem = input$mzspace,
                                  ppm= input$mzppm,
                                  charge = input$mzcharge,
                                  IntegerSaturation = input$integUnsat,
                                  minUnsat = input$minUnsat,
                                  maxUnsat = input$maxUnsat),
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
  if(length(input$source) > 0){
    mzi <- if(input$source != "custom"){as.numeric(selections$search$mz[[input$source]])}else{as.numeric(input$mzquery)}
  
    return(list(table = massquery(mzi,
                                  elem = input$mzspace,
                                  ppm= input$mzppm,
                                  charge = input$mzcharge,
                                  IntegerSaturation = input$integUnsat,
                                  minUnsat = input$minUnsat,
                                  maxUnsat = input$maxUnsat),
                mz = mzi,
                source = input$source))
  }
    })

#render sum formula table
output$hot1 <- renderRHandsontable({
  #options(digits=8)
  rhandsontable(mzauto()$table[,which(colnames(mzauto()$table) != "info")],#format(mztab(),digits=10),
                #ftable1()[which(rownames(ftable1()) %in% input$selcol1),],
                readOnly = TRUE,
                contextMenu = FALSE,
                selectCallback = TRUE,
                height = 200,
              
                digits=8) %>%
    hot_cols(columnSorting = TRUE,format="0.000000")%>%
    hot_col("em",format="0.000000")%>%
    hot_col("unsat",format="0.0")%>%
    hot_col("ppm",format="0.000")
  
})

}

#' massquery
#' 
#' Chemcalc mz query module
#' UI module to get molecular formula predictions from chemcalc
#' 
#' @param id id to be used in ns()
#'
#' 
#' @export 
MzqueryModuleUI <- function(id){
 ns <- NS(id)
 
  
htmlOutput(ns('mzUI'))

}



