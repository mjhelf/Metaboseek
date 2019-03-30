#' MainTableModule
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
MainTableModule <- function(input,
                            output,
                            session,
                            values = reactiveValues(featureTables = featureTables,
                                                    GlobalOpts = GlobalOpts,
                                                    projectData = projectData),
                            static = list(height = 300,
                                          readOnly = T,
                                          contextMenu = T,
                                          fixedColumnsLeft = 1,
                                          invertReadOnly = NULL,
                                          controls = T,
                                          format = list(col = NULL,
                                                        format = NULL))
){
  
  #### Initialization ####
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(order = integer(0),
                                   inpage = integer(0),
                                   
                                   sortCheck = F,
                                   decreasing = T,
                                   sortBy = NULL,
                                   page = 1,
                                   set = NULL,
                                   liveView = NULL,
                                   hasUpdates = F
  )
  
  observe({internalValues$hasUpdates <- !is.null(input$maintable$changes$changes)})
  
  callModule(SelectActiveTableModule, "tablechange", values = reactiveValues(featureTables = values$featureTables,
                                                                             MainTable = internalValues))
  
  callModule(SaveTableModule, "savetable",
             reactives = reactive({list(df = NULL,
                                        filename = file.path("Saved Tables", 
                                                             paste0(
                                                                    gsub("\\.csv$","",values$featureTables$tables[[values$featureTables$active]]$tablename),
                                                                    #filter settings may be pasted here in the future
                                                                    ".csv"))
             )}),
             values = reactiveValues(projectData = values$projectData,
                                     featureTables = values$featureTables,
                                     MainTable = internalValues),
             static = list(tooltip = "Save the Feature Table",
                           label = "Save Table",
                           format = c("csv"),
                           allowformats = list("Comma separated (.csv)" = "csv",
                                               "Inclusion/Exclusion list" = "instrumentList"))
  )
  
  observeEvent(c(internalValues$page,
                 internalValues$decreasing,
                 internalValues$sortBy,
                 internalValues$sortCheck,
                 values$featureTables$tableSwitch,
                 values$featureTables$row_filters),{
                   
                   # if(!is.null(input$maintable$changes$event) && input$maintable$changes$event == "afterRemoveRow"){
                   #   
                   #   values$featureTables$tables[[values$featureTables$active]]$df <- values$featureTables$tables[[values$featureTables$active]]$df[- internalValues$inpage[(input$maintable$changes$ind + 1) : (input$maintable$changes$ind + input$maintable$changes$ct)] ,]
                   # }
                   # 
                   # if(!is.null(input$maintable$changes$event) && input$maintable$changes$event == "afterCreateRow"){
                   #   
                   #   values$featureTables$tables[[values$featureTables$active]]$df[(nrow(values$featureTables$tables[[values$featureTables$active]]$df) + 1):(nrow(values$featureTables$tables[[values$featureTables$active]]$df) + input$maintable$changes$ct),] <- NA
                   # }
                   
                   # if(!is.null(values$featureTables$tableSwitch) && values$featureTables$tableSwitch){
                   #   internalValues$liveView <- NULL
                   # }
                   
                   #update the df with any possible changes before changing anything else
                   if(!is.null(input$maintable)
                      && !identical(values$featureTables$tables[[values$featureTables$active]]$df[internalValues$inpage,], internalValues$liveView)
                      && !is.null(internalValues$liveView)
                      && !is.null(input$maintable$changes$changes)
                      && !values$featureTables$tableSwitch
                   ){
                     values$featureTables$tables[[values$featureTables$active]]$df[row.names( internalValues$liveView),colnames( internalValues$liveView)] <-  internalValues$liveView
                     
                   }               
                   
                   if(is.null(internalValues$sortBy) || !internalValues$sortBy %in% colnames(values$featureTables$tables[[values$featureTables$active]]$df)){
                     internalValues$sortBy <- colnames(values$featureTables$tables[[values$featureTables$active]]$df)[1]
                   }
                   
                   if(internalValues$sortCheck && length(internalValues$sortBy) > 0){
                     #this seems like it might be slow
                     whi <- if(length(values$featureTables$row_filters) ==1 && values$featureTables$row_filters){seq(nrow(values$featureTables$tables[[values$featureTables$active]]$df))}else{which(values$featureTables$row_filters)}
                     ord <- order(values$featureTables$tables[[values$featureTables$active]]$df[values$featureTables$row_filters,internalValues$sortBy], decreasing = internalValues$decreasing)
                     internalValues$order <- whi[ord]
                   }
                   #case that includes NULL (at initialization) and TRUE if no filter is active               
                   else if(length(values$featureTables$row_filters) <2){
                     internalValues$order <- seq(nrow(values$featureTables$tables[[values$featureTables$active]]$df))
                   }else{
                     internalValues$order <- which(values$featureTables$row_filters)
                   }
                   internalValues$inpage <- if(is.null(values$GlobalOpts$perPage)){
                     internalValues$order}
                   else if(internalValues$page >= ceiling(length(internalValues$order)/values$GlobalOpts$perPage)){
                     internalValues$page <- ceiling(length(internalValues$order)/values$GlobalOpts$perPage)
                     internalValues$order[c((internalValues$page*values$GlobalOpts$perPage-(values$GlobalOpts$perPage-1)):(length(internalValues$order)))]}
                   else{
                     if(internalValues$page < 1){internalValues$page <- 1}
                     internalValues$order[c((internalValues$page*values$GlobalOpts$perPage-(values$GlobalOpts$perPage-1)):(internalValues$page*values$GlobalOpts$perPage))]}
                   
                   values$featureTables$tableSwitch <- F
                   
                   
                 })
  
  
  output$maintable <- renderRHandsontable({
    if(length(internalValues$inpage > 0 )){
      
      
      rhandsontable(values$featureTables$tables[[values$featureTables$active]]$df[internalValues$inpage,values$featureTables$selectedCols[values$featureTables$selectedCols %in% colnames(values$featureTables$tables[[values$featureTables$active]]$df)]],
                    readOnly = !values$featureTables$tables[[values$featureTables$active]]$editable,
                    contextMenu = values$featureTables$tables[[values$featureTables$active]]$editable,
                    selectCallback = TRUE,
                    height = if(length(internalValues$inpage) < 22){NULL}else{500},
                    outsideClickDeselects = FALSE,
                    digits=8,
                    highlightCol = TRUE, 
                    highlightRow = TRUE,
                    autoWrapCol = FALSE,
                    autoWrapRow = FALSE) %>%
        hot_col("comments", readOnly = FALSE)%>%
        hot_cols(columnSorting = FALSE,format="0.000")%>%
        hot_col(col = grep("^mz",values$featureTables$selectedCols[values$featureTables$selectedCols %in% colnames(values$featureTables$tables[[values$featureTables$active]]$df)], value = T), format="0.000000")%>%
        hot_cols(fixedColumnsLeft = 3)
      
      
      
      
      
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
  
  observeEvent(input$maintable_select$select,{
    if(!is.null(input$maintable_select$select)){
      internalValues$selected_cols <- as.integer(input$maintable_select$select$c):as.integer(input$maintable_select$select$c2)
      internalValues$selected_rows <- as.integer(input$maintable_select$select$r):as.integer(input$maintable_select$select$r2)
    }
    
  })
  
  output$sortC <- renderUI({
    checkboxInput(ns('sortCheck'), 'sort', value = internalValues$sortCheck)
  })
  
  observeEvent(input$sortCheck,{
    internalValues$sortCheck <- input$sortCheck
  })
  
  output$decreasingC <- renderUI({
    checkboxInput(ns('decreasingCheck'), 'decreasing', value = internalValues$decreasing)
  })
  
  observeEvent(input$decreasingCheck,{
    internalValues$decreasing <- input$decreasingCheck
  })
  
  output$sortByC <- renderUI({
    
    selectizeInput(ns('sortBy'), "Sort by column", choices = colnames(values$featureTables$tables[[values$featureTables$active]]$df), selected = internalValues$sortBy)
    
  })
  
  observeEvent(input$sortBy,{
    internalValues$sortBy <- input$sortBy
  })
  
  output$pageN <- renderUI({
    if(!values$featureTables$tables[[values$featureTables$active]]$editable){
      numericInput(ns('page'),
                   "page",
                   value = internalValues$page, min = 1)
    }
  })
  
  observeEvent(input$page,{
    if(!is.null(input$page) && !is.na(input$page)){
    internalValues$page <- input$page
    }
    
  })
  
  output$perPageI <- renderUI({
    if(!values$featureTables$tables[[values$featureTables$active]]$editable){
      textOutput(ns('tableInfo'))
    }
  })
  
  output$tableInfo <- renderText({
    if(!values$featureTables$tables[[values$featureTables$active]]$editable){
      paste0(length(internalValues$order),
             " items (",
             ceiling(length(internalValues$order)/values$GlobalOpts$perPage),"page(s))"
      )
    }
    
  })
  
  output$tabCtrls <- renderUI({
    
    fluidRow(
      column(1,
             htmlOutput(ns('sortC')),
             htmlOutput(ns('decreasingC'))
      ),
      column(3,
             htmlOutput(ns('sortByC'))
      ),
      column(1,
             htmlOutput(ns('pageN'))
      ),
      column(2,
             htmlOutput(ns('perPageI'))
      ),
      column(2,
             SaveTableModuleUI(ns("savetable"))
      ),
      column(3,
             SelectActiveTableModuleUI(ns("tablechange"))
      )
    )
    
  })
  
  output$tabUI <- renderUI({
    fluidRow(
      rHandsontableOutput(ns("maintable"))
    )
  })
  
  
  
  
  return(internalValues) 
}

#' MainTableModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
MainTableModuleUI <- function(id){
  
  
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    htmlOutput(ns("tabUI")),
    htmlOutput(ns('tabCtrls'))
    
  )
  
  
}