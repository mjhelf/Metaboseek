#' PeptideSequencePlotWidget
#' 
#' This module allows setting fixed and variable peptide modifications to be 
#' applied in peptide-related calculations (fragment annotation and peptide mass calculations)
#' 
#' @details Plots with MassTools::plotAnnotatedPeptide
#' 
#' @param reactives list of arguments to be passed to
#'  \code{MassTools::\link[MassTools]{plotAnnotatedPeptide}} 
#'  inside of shiny::reactive()
#' @param layout list of layout options (active: logical, and height: numeric)
#' 
#' @inherit MseekWidgets
#' 
#' @describeIn PeptideSequencePlotWidget server logic for PeptideSequencePlotWidget
#' @importFrom MassTools plotAnnotatedPeptide
#'
#' @export
PeptideSequencePlotWidget <- function(input,output, session, reactives = reactive({list()}),
                                      layout = reactive({list(active = T,
                                                              height = 550)})){
    
    ns <- NS(session$ns(NULL))
    
    
    output$plotSeq <- renderPlot({
        if(!is.null(reactives())
           && !is.null(layout()) && layout()$active
        ){
            do.call(plotAnnotatedPeptide, reactives())
        }
    })
    
    output$renderedPlot <- renderUI({
        if(!is.null(reactives())
           && !is.null(layout()) && layout()$active
        ){
            scallback <- !is.null(layout()$selectCallback) && layout()$selectCallback
            
            plotOutput(ns("plotSeq"),
                       height = if(is.null(layout()$height)){"550px"}else{paste0(layout()$height,"px")}
            )
        }
    })
    
    
}

#' @describeIn PeptideSequencePlotWidgetUI UI elements for PeptideSequencePlotWidget
#'
#' @export
PeptideSequencePlotWidgetUI <- function(id){
    ns <- NS(id)
    
    htmlOutput(ns("renderedPlot"))   
    
}