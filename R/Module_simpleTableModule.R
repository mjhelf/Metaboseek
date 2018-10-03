#' simpleTableModule
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param set Import data from the shiny session
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
        highlightRow = TRUE)
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

#' simpleTableModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
simpleTableModuleUI <- function(id){
  
  
  ns <- NS(id)
  fluidPage(
    fluidRow(
      rHandsontableOutput(ns("maintable"))
    )
  )
  
  
}