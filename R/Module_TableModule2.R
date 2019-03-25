#' TableModule
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
TableModule2 <- function(input,output, session,
                        reactives = reactive({list(df = NULL,
                                                   rowFilters = NULL,
                                                   colFilters = NULL)}),
                        values = reactiveValues(),
                        static = list(perpage = 100,
                                      height = 300,
                                      readOnly = T,
                                      contextMenu = T,
                                      fixedColumnsLeft = 1,
                                      invertReadOnly = NULL,
                                      format = list(col = NULL,
                                                    format = NULL))
){
  #### Initialization ####
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(df = NULL,
                                   inpage = NULL,
                                    page = 1,
                                    showTable = NULL,
                                    updating = F,
                                   liveView = NULL, #makes hot_to_r accessible from outside module
                                    
                                    row_order = NULL,
                                    selected_rows = NULL,
                                    selected_cols = NULL,
                                    
                                    sortCheck = F,
                                    decreasing = T,
                                    sortby = NULL,
                                    set = NULL)
  
  
  #### External updates ####
  
  observeEvent(reactives()$df,{
    
    internalValues$selected_cols <- NULL
      internalValues$selected_rows <- NULL
      
      if(length(reactives()$df) == 0 ){
         internalValues$showTable <- NULL
        }
    
    if(!is.null(reactives()$df)){
            internalValues$row_order <- seq(nrow(reactives()$df))
    }
      #internalValues$showTable <- NULL
      internalValues$df <- reactives()$df
      internalValues$updating <- T
  })
  
  observeEvent(c(internalValues$page,
                 internalValues$decreasing,
                 internalValues$sortBy,
                 internalValues$sortCheck,
                 input$maintable$changes,
                 internalValues$df
                 #internalValues$updating
                 ),{
                   if(!is.null(internalValues$df) && length(internalValues$df) > 0){
                      
                    # print(internalValues$updating)
                     
                     if(!is.null(input$maintable$changes$event) && input$maintable$changes$event == "afterRemoveRow"){
                       
                       internalValues$df <- internalValues$df[- internalValues$inpage[(input$maintable$changes$ind + 1) : (input$maintable$changes$ind + input$maintable$changes$ct)] ,]
                     }
                     
                     if(!is.null(input$maintable$changes$event) && input$maintable$changes$event == "afterCreateRow"){
                       
                       internalValues$df[(nrow(internalValues$df) + 1):(nrow(internalValues$df) + input$maintable$changes$ct),] <- NA
                     }
                     

                       #update the df with any possible changes before changing anything else
                     if(!is.null(input$maintable)
                        && !identical(internalValues$showTable, hot_to_r(input$maintable))
                        && !is.null(internalValues$showTable)
                        && !is.null(input$maintable$changes$changes)
                        && !internalValues$updating){
                       
                         internalValues$df[row.names(hot_to_r(input$maintable)),colnames(hot_to_r(input$maintable))] <- hot_to_r(input$maintable)

                          }

                     if(internalValues$sortCheck && length(internalValues$sortBy) > 0){
                       if(!is.null(reactives()$rowFilters)){
                       internalValues$row_order <- order(internalValues$df[rowFilters,internalValues$sortBy], decreasing = internalValues$decreasing)}
                       else{
                         internalValues$row_order <- order(internalValues$df[,internalValues$sortBy], decreasing = internalValues$decreasing)
                          }
                     
                       }else{
                       internalValues$row_order <- if(!is.null(reactives()$rowFilters)){
                         seq(nrow(internalValues$df[reactives()$rowFilters,]))}else{
                           seq(nrow(internalValues$df))
                         }
                     }
                     
                     internalValues$inpage <- if(is.null(static$perpage)){
                       internalValues$row_order}
                     else if(internalValues$page >= ceiling(length(internalValues$row_order)/static$perpage)){
                       internalValues$page <- ceiling(length(internalValues$row_order)/static$perpage)
                       internalValues$row_order[c((internalValues$page*static$perpage-(static$perpage-1)):(length(internalValues$row_order)))]}
                     else{
                       if(internalValues$page < 1){internalValues$page <- 1}
                       internalValues$row_order[c((internalValues$page*static$perpage-(static$perpage-1)):(internalValues$page*static$perpage))]}
                     
                     if(!is.null(reactives()$colFilters)){
                     isolate(internalValues$showTable <- internalValues$df[internalValues$inpage,reactives()$colFilters])}
                     else{
                       isolate(internalValues$showTable <- internalValues$df[internalValues$inpage,])
                     }
                     
                   }
                   internalValues$updating <- F
                 })
  
  
  
  
  #make hot_to_r accessible from outside module
  observeEvent(input$maintable,{
    
    
    if(!is.null(input$maintable) && !identical(internalValues$liveView, hot_to_r(input$maintable))){
      internalValues$liveView <- hot_to_r(input$maintable)
    }
    
  })
  
 
  
  
  
  
  #### UI Elements and their single observers ####
  
  
  output$sortC <- renderUI({
    if(is.null(static$sort) || static$sort){
    checkboxInput(ns('sortCheck'), 'sort', value = internalValues$sortCheck)
    }
  })
  
  observeEvent(input$sortCheck,{
    if(is.null(static$sort) || static$sort){
      
    internalValues$sortCheck <- input$sortCheck
    }
  })
  
  output$decreasingC <- renderUI({
    if(is.null(static$sort) || static$sort){
      
    checkboxInput(ns('decreasingCheck'), 'decreasing', value = internalValues$decreasing)
    }
  })
  
  observeEvent(input$decreasingCheck,{
    if(is.null(static$sort) || static$sort){
      
    internalValues$decreasing <- input$decreasingCheck
    }
  })
  
  output$sortByC <- renderUI({
    if(!is.null(internalValues$df) && (is.null(static$sort) || static$sort)){

  selectizeInput(ns('sortBy'), "Sort by column", choices = colnames(internalValues$df), selected = internalValues$sortBy)
    }
      })

  observeEvent(input$sortBy,{
    internalValues$sortBy <- input$sortBy
  })
  
  output$perPageN <- renderUI({
  if(!is.null(static$perpage)){
    numericInput(ns('page'), "page", value = internalValues$page, min = 1)
  }
  })
  
  observeEvent(input$page,{
    if(!is.null(input$page) && !is.na(input$page)){
    internalValues$page <- input$page
    }
    })
  
  output$perPageI <- renderUI({
  if(!is.null(static$perpage)){
    textOutput(ns('tableInfo'))
  }
  })
  
  output$tableInfo <- renderText({
    if(!is.null(static$perpage)){
      paste0(nrow(internalValues$df),
             " items (",
             ceiling(nrow(internalValues$df)/static$perpage),"page(s))"
      )
    }
    
  })
  
  output$maintable <- renderRHandsontable({
    if(!is.null(internalValues$showTable) 
       && (is.null(isolate(input$maintable)) || !identical(internalValues$showTable,isolate(hot_to_r(input$maintable))))){
      
      #  rheight <- if(nrow(combino()[inpage(),])<40){NULL}else{500}
      
      rhandsontable(internalValues$showTable,
                    readOnly = static$readOnly,
                    contextMenu = static$contextMenu,
                    selectCallback = TRUE,
                    height = static$height,
                    # width = 1000,
                    digits=8,
                    row_highlight = 1,
                    #format = "0.0000",
                    highlightCol = TRUE,
                    highlightRow = TRUE,
                    autoWrapCol = FALSE,
                    autoWrapRow = FALSE) %>%
        #  hot_cols(renderer = "
        #        function(instance, td, row, col, prop, value, cellProperties) {
        #               Handsontable.TextCell.renderer.apply(this, arguments);
        #            td.style.color = 'black';
        #       }")%>%
        hot_col(col = static$format$col[which(static$format$col %in% colnames(internalValues$showTable))], format=static$format$format)%>%
        hot_col(col = static$invertReadOnly[which(static$invertReadOnly %in% colnames(internalValues$showTable))], readOnly = !static$readOnly)%>%
        hot_cols(fixedColumnsLeft = static$fixedColumnsLeft)%>%
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
  
  observeEvent(input$maintable_select$select,{
    if(!is.null(input$maintable_select$select)){
      internalValues$selected_cols <- as.integer(input$maintable_select$select$c):as.integer(input$maintable_select$select$c2)
      internalValues$selected_rows <- as.integer(input$maintable_select$select$r):as.integer(input$maintable_select$select$r2)
      }
    
  })
  #### Multi Observers ####
  
  
  
  #### Compounded UI ####
  
  output$tabCtrls <- renderUI({
    if(!is.null(internalValues$df)){
      fluidRow(
        column(3,
               htmlOutput(ns('sortC')),
               htmlOutput(ns('decreasingC'))
        ),
        column(3,
               htmlOutput(ns('sortByC'))
        ),
        column(3,
               htmlOutput(ns('perPageN'))
        ),
        column(3,
               htmlOutput(ns('perPageI'))
        )
      )
    }
  })
  
  output$tabUI <- renderUI({
    fluidRow(
      rHandsontableOutput(ns("maintable"))
    )
  })
  
  
  return(internalValues)
  
}

#' TableModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
TableModule2UI <- function(id){
  ns <- NS(id)
  fluidPage(
    
    htmlOutput(ns("tabUI")),
    htmlOutput(ns('tabCtrls'))
    
  )
  
  
}