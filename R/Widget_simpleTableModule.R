#' simpleTableModule
#' 
#' A simple Table module without sorting or pagination
#' 
#' @inherit MseekWidgets
#' @param df reactive containing the data.frame to display
#' 
#' @return Returns its internalValues, most importantly elements \code{df} 
#' and \code{liveView}
#' 
#' @describeIn simpleTableModule Server logic
#' 
#' @export 
simpleTableModule <- function(input,output, session,
                              df = reactive({NULL}),
                              static = list(readOnly = T,
                                            contextMenu = F,
                                            height = "auto")
){
  #### Initialization ####
  
  ns <- NS(session$ns(NULL))
  
  internalValues  <- reactiveValues(df= NULL,
                                    update = T,
                                    liveView = NULL,
                                    selected_cols = NULL,
                                    selected_rows = NULL)
  
  # observeEvent(internalValues$update,{
  #   print("updateObserved")
  # })
  
  intDF <- reactive({
    internalValues$update
    return(internalValues$df)
  })
  
  output$maintable <- renderRHandsontable({
    if(!is.null(
      #internalValues$df
      df())
      || !is.null(intDF())
      
      #&& internalValues$update
    ){ 
      rhandsontable(#internalValues$df
        if(!is.null(df())){df()}else{intDF()}
        #intDF()
        ,
        readOnly = static$readOnly,
        contextMenu = static$contextMenu,
        selectCallback = TRUE,
        height = static$height,
        digits=8,
        row_highlight = 1,
        highlightCol = TRUE,
        highlightRow = TRUE,
        autoWrapCol = FALSE,
        autoWrapRow = FALSE)
    }
  })
  
  observeEvent(input$maintable_select$select,{
    if(!is.null(input$maintable_select$select)){
      internalValues$selected_cols <- as.integer(input$maintable_select$select$c):as.integer(input$maintable_select$select$c2)
      internalValues$selected_rows <- as.integer(input$maintable_select$select$r):as.integer(input$maintable_select$select$r2)
    }
    
  })
  
  #make hot_to_r accessible from outside module
  observeEvent(input$maintable,{
    
    if(!is.null(input$maintable) && !identical(internalValues$liveView, hot_to_r(input$maintable))){
      internalValues$liveView <- hot_to_r(input$maintable)
    }
    
    if(is.null(input$maintable)){
      internalValues$liveView <- NULL
    }
    
  })
  
  return(internalValues)
  
}

#' @describeIn simpleTableModule UI elements
#' @export
simpleTableModuleUI <- function(id){
  
  
  ns <- NS(id)
  fluidPage(
    fluidRow(
      rHandsontableOutput(ns("maintable"))
    )
  )
  
  
}