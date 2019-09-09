#' MainTableModule
#' 
#' Main Feature Table viewer Module
#' 
#' @inherit MseekModules
#' 
#' @return returns its internalValues and modifies \code{\link{values}}
#' @describeIn MainTableModule server logic
#' 
#' @examples
#' \dontrun{
#' library(shiny)
#' 
#' ui <- MseekMinimalUI(MainTableModuleUI("examplemodule"),
#'                      diagnostics = F, dashboard = F)
#' 
#' server <- function(input, output) {
#'     MseekMinimalServer(diagnostics = F, data = F, tables = T)
#'     
#'     callModule(MainTableModule, "examplemodule", values = reactiveValues(featureTables = values$featureTables,
#'                                                                                           GlobalOpts = values$GlobalOpts,
#'                                                                                           projectData = values$projectData))
#' }
#' # Create Shiny app ----
#' shinyApp(ui, server)
#' }
#' @details 
#' \describe{
#' \item{static}{
#' \itemize{
#' \item \code{heigth} height of the Table view in pixels
#' \item \code{readOnly} if TRUE, no changes to table can be made from GUI
#' \item \code{contextMenu} if TRUE, allow right click 
#' \item \code{fixedColumnsLeft} number of columns to always show at the left 
#' even when scrolling horizontally
#' \item \code{invertReadOnly} character of column names for which to invert 
#' the readOnly rule (e.g. only allow editing on comments column)
#' \item \code{controls} show UI elements for filtering
#' \item \code{format} a named list with elements \code{col} and \code{format}
#' }
#' 
#' }
#' }
#' @export 
MainTableModule <- function(input, output, session,
                            values = reactiveValues(featureTables = NULL,
                                                    GlobalOpts = NULL,
                                                    projectData = NULL),
                            static = list(height = 300,
                                          readOnly = TRUE,
                                          contextMenu = TRUE,
                                          fixedColumnsLeft = 1,
                                          invertReadOnly = NULL,
                                          controls = TRUE,
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
                                   hasUpdates = F,
                                   updatedFrom = NULL,
                                   resortTrigger = FALSE,
                                   renderedTable = NULL
  )
  
  observeEvent(values,{
  values$featureTables$Maintable <- internalValues
  }, once = TRUE)
  
  
  observeEvent(input$maintable$changes$changes,{
      internalValues$hasUpdates <- !is.null(input$maintable$changes$changes)
     # if(internalValues$hasUpdates){
          internalValues$updatedFrom <- internalValues$renderedTable
         # }
      }, priority = 2000, ignoreNULL = F)
  
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
                                     featureTables = values$featureTables),
             static = list(tooltip = "Save the Feature Table",
                           label = "Save Table",
                           format = c("csv"),
                           allowformats = list("Comma separated (.csv)" = "csv",
                                               "Metaboseek Feature Table (.mskFT)" = "mskFT",
                                               "Inclusion/Exclusion list" = "instrumentList"))
  )
  
  callModule(MseekHistoryWidget, "tablehistory", FT = reactive({FeatureTable(values)}))
  
  
  observeEvent(c(internalValues$page,
                 internalValues$decreasing,
                 internalValues$sortBy,
                 internalValues$sortCheck,
                 #values$featureTables$tableSwitch,
                 values$featureTables$row_filters),{
                   

                   #update the df with any possible changes before changing anything else
                   if(!is.null(input$maintable)
                      && !identical(values$featureTables$tables[[values$featureTables$active]]$df[internalValues$inpage,], internalValues$liveView)
                      && !is.null(internalValues$liveView)
                      && !is.null(input$maintable$changes$changes)
                      && !values$featureTables$tableSwitch
                   ){
                       updateFT(values)
                     #values$featureTables$tables[[values$featureTables$active]]$df[row.names( internalValues$liveView),colnames( internalValues$liveView)] <-  internalValues$liveView
                   }
                     
                     internalValues$resortTrigger <- !internalValues$resortTrigger
                 })
  
  observeEvent(c(internalValues$resortTrigger,
                 values$featureTables$tableSwitch),{
                 
                     # if(is.null(values$featureTables$tableSwitch)
                     #    || !values$featureTables$tableSwitch){
                     #    isolate({values$featureTables$row_filters <- TRUE})
                     # }
                     
                   if(is.null(internalValues$sortBy) 
                      || !internalValues$sortBy %in% colnames(values$featureTables$tables[[values$featureTables$active]]$df)){
                     internalValues$sortBy <- colnames(values$featureTables$tables[[values$featureTables$active]]$df)[1]
                   }
                   
                   if(internalValues$sortCheck && length(internalValues$sortBy) > 0){
                     #this seems like it might be slow
                     whi <- if(length(values$featureTables$row_filters) ==1 
                               && values$featureTables$row_filters){seq(nrow(values$featureTables$tables[[values$featureTables$active]]$df))
                         }else{which(values$featureTables$row_filters)}
                     ord <- order(values$featureTables$tables[[values$featureTables$active]]$df[values$featureTables$row_filters,internalValues$sortBy],
                                  decreasing = internalValues$decreasing)
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
                   
                   values$featureTables$tableSwitch <- FALSE
                   
                   
                 })
  
  
  output$maintable <- renderRHandsontable({
    if(!is.null(FeatureTable(values))
        && length(internalValues$inpage > 0 )){
      
      selcols <- if(!is.null(values$featureTables$selectedCols)){
          values$featureTables$selectedCols[values$featureTables$selectedCols %in% colnames(FeatureTable(values)$df)]
      }else{colnames(FeatureTable(values)$df)}
      
              internalValues$renderedTable <- activeFT(values)

      rhandsontable(values$featureTables$tables[[values$featureTables$active]]$df[internalValues$inpage, selcols],
                    readOnly = !FeatureTable(values)$editable,
                    contextMenu = FeatureTable(values)$editable,
                    selectCallback = TRUE,
                    height = if(length(internalValues$inpage) < 22){
                        NULL}else{500},
                    outsideClickDeselects = FALSE,
                    digits=8,
                    highlightCol = TRUE, 
                    highlightRow = TRUE,
                    autoWrapCol = FALSE,
                    autoWrapRow = FALSE) %>%
        hot_col("comments", readOnly = FALSE)%>%
        hot_cols(columnSorting = FALSE,format="0.000")%>%
        hot_col(col = grep("^mz", selcols, value = T), format="0.000000")%>%
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
      column(1,
             SaveTableModuleUI(ns("savetable"))
      ),
      column(1,
             MseekHistoryWidgetUI(ns("tablehistory"))),
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

#' @describeIn MainTableModule UI elements
#' @export
MainTableModuleUI <- function(id){
  
  
  ns <- NS(id)
  fluidPage(
   # shinyjs::useShinyjs(),
    htmlOutput(ns("tabUI")),
    htmlOutput(ns('tabCtrls'))
    
  )
  
  
}