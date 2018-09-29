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


#' TableModule
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param tag id to be used in ns()
#' @param set Import data from the shiny session
#' 
#' @export 
TableModule <- function(input,output, session, tag, set = list(df =  NULL,
                                                               update = 1, #change this to trigger update of the df or set to NULL to update anytime df changes
                                                               layout = list(
                                                                 perpage = 100,
                                                                 height = 300,
                                                                 readOnly = T,
                                                                 contextMenu = T,
                                                                 fixedColumnsLeft = 1,
                                                                 invertReadOnly = NULL,
                                                                 format = list(col = NULL,
                                                                               format = NULL)
                                                               ))
){
  
  ns <- NS(tag)
  
  tableProperties <- reactiveValues(inpage = NULL,
                                    page = 1,
                                    showTable = NULL,
                                    updating = F,
                                    
                                    row_order = NULL,
                                    selected_rows = NULL,
                                    selected_cols = NULL,
                                    
                                    sortCheck = F,
                                    decreasing = T,
                                    sortby = NULL,
                                    set = NULL)
  
  observeEvent(set(),{
    if(length(set()$df) == 0 ){
      tableProperties$set <- set()
      tableProperties$selected_cols <- NULL
      tableProperties$selected_rows <- NULL
      tableProperties$df <- NULL
      tableProperties$showTable <- NULL
      
    }
    
    if(!is.null(set()$df) && (is.null(tableProperties$set$update) || set()$update != tableProperties$set$update)){
      
      tableProperties$row_order <- seq(nrow(set()$df))
      
      tableProperties$set <- set()
      tableProperties$updating <- T
      tableProperties$selected_cols <- NULL
      tableProperties$selected_rows <- NULL
      
    }
    
    
  })
  
  observeEvent(c(tableProperties$page,
                 tableProperties$decreasing,
                 tableProperties$sortBy,
                 tableProperties$sortCheck,
                 tableProperties$set$update,
                 tableProperties$set$df),{
    if(!is.null(tableProperties$set$df) && length(tableProperties$set$df) > 0){
      # if(tableProperties$updating){
      #   tableProperties$updating <- F
      # }else{
      #   #update the df with any possible changes before changing anything else
      #   if(!is.null(input$maintable) && !identical(tableProperties$showTable, hot_to_r(input$maintable))){
      #     
      #     tableProperties$set$df[row.names(hot_to_r(input$maintable)),colnames(hot_to_r(input$maintable))] <- hot_to_r(input$maintable)
      #   }
      # }
      
      if(tableProperties$sortCheck && length(tableProperties$sortBy) > 0){
        tableProperties$row_order <- order(tableProperties$set$df[,tableProperties$sortBy], decreasing = tableProperties$decreasing)
      }else{
        tableProperties$row_order <- seq(nrow(tableProperties$set$df))
      }
      
      tableProperties$inpage <- if(is.null(set()$layout$perpage)){
        tableProperties$row_order}
      else if(tableProperties$page >= ceiling(length(tableProperties$row_order)/set()$layout$perpage)){
        tableProperties$page <- ceiling(length(tableProperties$row_order)/set()$layout$perpage)
        tableProperties$row_order[c((tableProperties$page*set()$layout$perpage-(set()$layout$perpage-1)):(length(tableProperties$row_order)))]}
      else{
        if(tableProperties$page < 1){tableProperties$page <- 1}
        tableProperties$row_order[c((tableProperties$page*set()$layout$perpage-(set()$layout$perpage-1)):(tableProperties$page*set()$layout$perpage))]}
      
      tableProperties$showTable <- tableProperties$set$df[tableProperties$inpage,]
      
    }
  })
  
  
  output$maintable <- renderRHandsontable({
    if(!is.null(tableProperties$showTable) 
       && (is.null(isolate(input$maintable)) || !identical(tableProperties$showTable,isolate(hot_to_r(input$maintable))))){
      
      #  rheight <- if(nrow(combino()[inpage(),])<40){NULL}else{500}
      
      rhandsontable(tableProperties$showTable,
                    readOnly = set()$layout$readOnly,
                    contextMenu = set()$layout$contextMenu,
                    selectCallback = TRUE,
                    height = set()$layout$height,
                    # width = 1000,
                    digits=8,
                    row_highlight = 1,
                    #format = "0.0000",
                    highlightCol = TRUE,
                    highlightRow = TRUE) %>%
        #  hot_cols(renderer = "
        #        function(instance, td, row, col, prop, value, cellProperties) {
        #               Handsontable.TextCell.renderer.apply(this, arguments);
        #            td.style.color = 'black';
        #       }")%>%
        hot_col(col = set()$layout$format$col[which(set()$layout$format$col %in% colnames(tableProperties$showTable))], format=set()$layout$format$format)%>%
        hot_col(col = set()$layout$invertReadOnly[which(set()$layout$invertReadOnly %in% colnames(tableProperties$showTable))], readOnly = !set()$layout$readOnly)%>%
        hot_cols(fixedColumnsLeft = set()$layout$fixedColumnsLeft)%>%
        hot_cols(columnSorting = FALSE)#%>%
      #hot_col("em",format="0.000000")%>%
      # hot_cols(renderer = "
      #  function(instance, td, row, col, prop, value, cellProperties) {
      #    Handsontable.renderers.TextRenderer.apply(this, arguments);
      #      tbl = HTMLWidgets.widgets.filter(function(widget) {
      #// this should match the table id specified in the shiny app
      #          return widget.name === 'maintable'
      #})[0];
      
      #   hrows = tbl.params.row_highlight
      #  hrows = hrows instanceof Array ? hrows : [hrows] 
      #
      #     if (hrows.includes(row)) {
      #      td.style.background = 'pink';
      #   }
      
      #  return td;
      #}")
      
      # 
      
    }
  })
  
  output$tabUI <- renderUI({
    
   # if(!is.null(tableProperties$set$df)){
      fluidRow(
        rHandsontableOutput(ns("maintable"))
      )
  #  }
  })
  
  
  output$tableInfo <- renderText({
    if(!is.null(set()$layout$perpage)){
      paste0(nrow(tableProperties$set$df),
             " items (",
             ceiling(nrow(tableProperties$set$df)/set()$layout$perpage),"page(s))"
      )
    }
    
  })
  
  output$tabCtrls <- renderUI({
    if(!is.null(tableProperties$set$df)){
      fluidRow(
        column(3,
               checkboxInput(ns('sortCheck'), 'sort', value = tableProperties$sortCheck),
               checkboxInput(ns('decreasingCheck'), 'decreasing', value = tableProperties$decreasing)),
        column(3,
               
               selectizeInput(ns('sortBy'), "Sort by column", choices = colnames(tableProperties$set$df), selected = tableProperties$sortBy)
        ),
        column(3,
               if(!is.null(set()$layout$perpage)){
                 numericInput(ns('page'), "page", value = tableProperties$page, min = 1)
               }
        ),
        column(3,
               if(!is.null(set()$layout$perpage)){
                 textOutput(ns('tableInfo'))
               }
        )
      )
    }
  })
  
  observeEvent(input$sortCheck,{
    tableProperties$sortCheck <- input$sortCheck
  })
  
  observeEvent(input$decreasingCheck,{
    tableProperties$decreasing <- input$decreasingCheck
  })
  
  observeEvent(input$sortBy,{
    tableProperties$sortBy <- input$sortBy
  })
  
  observeEvent(input$page,{
    tableProperties$page <- input$page
  })
  
  observeEvent(input$maintable_select$select,{
    if(!is.null(input$maintable_select$select)){
      tableProperties$selected_cols <- as.integer(input$maintable_select$select$c):as.integer(input$maintable_select$select$c2)
      tableProperties$selected_rows <- as.integer(input$maintable_select$select$r):as.integer(input$maintable_select$select$r2)
      
      
    }
    
  })
  
  
  
  return(reactive({list(props = tableProperties#,
                        #live = if(!is.null(input$maintable)){hot_to_r(input$maintable)}else{NULL})
  )}))
  
}

#' TableModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
TableModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    
    htmlOutput(ns("tabUI")),
    htmlOutput(ns('tabCtrls'))
    
  )
  
  
}