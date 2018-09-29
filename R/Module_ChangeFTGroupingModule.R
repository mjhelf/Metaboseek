#' ChangeFTGroupingModule
#' 
#' 
#' server module to select and group intensity columns in Feature tables.
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @import shiny
#' @importFrom shinyjs toggle
#' 
#' @export 
ChangeFTGroupingModule <- function(input,output, session,
                                   reactives = reactive({list()}),
                                   values = reactiveValues(featureTables = featureTables,
                                                           MSData = MSData,
                                                           projectData = projectData),
                                   static = list()
){
  
  ns <- NS(session$ns(NULL))
  
  
  GroupingTable <-  callModule(simpleTableModule, "groupingTable",
                               df = reactive({NULL}),
                               static = list(readOnly = F,
                                             contextMenu = T,
                                             height = 270)
  )
  
  Preview <-  callModule(simpleTableModule, "preview",
                         df = reactive({values$featureTables$tables[[values$featureTables$active]]$df[1:(min(10,nrow(values$featureTables$tables[[values$featureTables$active]]$df))),]}),
                         static = list(readOnly = T,
                                       contextMenu = F,
                                       height = 270)
  )
  
  callModule(SaveTableModule, "saveGrouping",
             reactives = reactive({list(df = GroupingTable$liveView,
                                        filename = file.path("Table_groupingssdf","sdf",paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),"_",values$featureTables$tables[[values$featureTables$active]]$tablename,"_table.tGrouping")))}),
             values = reactiveValues(projectData = values$projectData),
             static = list(tooltip = "Save Grouping Table",
                           label = "Save",
                           format = c("tsv"))
  )
  
  TableLoader <- callModule(LoadTableModule, "tableLoader",
                            values = reactiveValues(projectData = values$projectData,
                                                    featureTables = NULL),
                            static = list(tooltip = "Load",
                                          label = "Load",
                                          format = list(header = T,
                                                        sep = "\t",
                                                        quote = '"',
                                                        stringsAsFactors = F),
                                          pattern = "\\.tGrouping$")
  )
  
  
  observeEvent(values$featureTables$active,{
    GroupingTable$df <- values$featureTables$tables[[values$featureTables$active]]$anagrouptable
    GroupingTable$update <- !GroupingTable$update
    # Preview$df <- values$featureTables$tables[[values$featureTables$active]]$df[1:(min(10,nrow(values$featureTables$tables[[values$featureTables$active]]$df))),]
    # Preview$update <- !Preview$update
  })
  
  observeEvent(TableLoader$df,{
    print(TableLoader$df)
    print(identical(TableLoader$df, GroupingTable$df))
    if(!is.null(TableLoader$df)){
      GroupingTable$df <- TableLoader$df
      GroupingTable$update <- !GroupingTable$update
    }
  })
  
  # when pressing Select Columns button (intcols)
  ###Override default column range with selected columns when pressing Button intcols, and load new anagrouptable template
  observeEvent(input$intcols,{
    internalValues$colrange <- Preview$selected_cols
    
    Groupnames <- if(!is.null(values$featureTables$tables[[values$featureTables$active]]$anagrouptable$Group) 
                     && length(Preview$selected_cols) == nrow(values$featureTables$tables[[values$featureTables$active]]$anagrouptable)){
      values$featureTables$tables[[values$featureTables$active]]$anagrouptable$Group}
    else{rep("G1",(length(Preview$selected_cols)))}
    
    
    values$featureTables$tables[[values$featureTables$active]] <- updateFTgrouping(values$featureTables$tables[[values$featureTables$active]],
                                                                                   data.frame(Column=colnames(values$featureTables$tables[[values$featureTables$active]]$df)[Preview$selected_cols],
                                                                                              Group = Groupnames,
                                                                                              stringsAsFactors = F))
    
    GroupingTable$df <- values$featureTables$tables[[values$featureTables$active]]$anagrouptable
    GroupingTable$update <- !GroupingTable$update
    
  })
  
  # when pressing Select Columns button (intcols)
  ###Override default column range with selected columns when pressing Button intcols, and load new anagrouptable template
  observeEvent(input$updateGroups,{
    if(!is.null(GroupingTable$liveView)){
      
      cnames <- colnames(values$featureTables$tables[[values$featureTables$active]]$df)
      
      if(!any(c((!GroupingTable$liveView$Column %in% cnames), duplicated(GroupingTable$liveView$Column) ))){
        
        
        if(!identical(values$featureTables$tables[[values$featureTables$active]]$anagrouptable, GroupingTable$liveView)){
          
          if(length(values$featureTables$tables[[values$featureTables$active]]$anagrouptable$Group) > 0){
            groupCols <- paste0(unique(c(GroupingTable$liveView$Group,
                                         values$featureTables$tables[[values$featureTables$active]]$anagrouptable$Group)),
                                "__")}
          else{
            groupCols <- NULL
          }
          groupSpecificCols <- c("topgroup", "maxfold", "maxfoldover2", "best_minFold", "best_minFoldMean")
          
          internalValues$GroupConflicts <- unlist(sapply(c(groupCols,
                                                           groupSpecificCols),
                                                         grep,
                                                         cnames ))
          
          names(internalValues$GroupConflicts) <- cnames[internalValues$GroupConflicts]
          
          
          # columns with __XIC do not represent a conflict
          noConflict <- grep("__XIC", cnames[internalValues$GroupConflicts])
          if(length(noConflict) >0){
            internalValues$GroupConflicts <- internalValues$GroupConflicts[-noConflict]
          }
          
          
          if(length(internalValues$GroupConflicts) > 0 ){
            showModal(
              modalDialog(
                p(strong("There are already results from a grouped analysis in the table.")),
                p("It is recommended to remove the following columns from the column to avoid incorrect Group analysis information:"),
                hr(),
                selectizeInput(ns("selectDelete"), "Columns to be deleted:",
                               choices = as.list(internalValues$GroupConflicts),
                               selected = internalValues$GroupConflicts,
                               multiple = T),
                hr(),
                p("Do you want to delete these columns from the table?"),
                checkboxInput(ns('deleteCheck'), 'Delete these columns', value = T),
                title = "Warning",
                easyClose = F,
                footer = actionButton(ns("modalOk"), "Ok")
              ))
            
          }
        }
        
        values$featureTables$tables[[values$featureTables$active]] <- updateFTgrouping(values$featureTables$tables[[values$featureTables$active]],
                                                                                       GroupingTable$liveView)
      }
      else{
        showModal(
          modalDialog(
            p(strong("There is a problem with the Column names you supplied.")),
            hr(),
            if(any(!GroupingTable$liveView$Column %in% cnames)){
              p("These Columns do not exist in the Feature Table:", strong(paste(unique(GroupingTable$liveView$Column[!GroupingTable$liveView$Column %in% cnames]), collapse = ", ")))
            }else{p("")},
            if(any(duplicated(GroupingTable$liveView$Column))){
              p("These Columns are listed multiple times in the Grouping Table:", strong(paste(unique(GroupingTable$liveView$Column[duplicated(GroupingTable$liveView$Column)]), collapse = ", ")))
            }else{p("")},
            
            title = "Error",
            easyClose = T,
            footer = modalButton("Cancel")
          ))
        
        
        
      }
    }
  })
  
  observeEvent(input$removeGroups,{
    
    cnames <- colnames(values$featureTables$tables[[values$featureTables$active]]$df)
    
    groupSpecificCols <- c("topgroup", "maxfold", "maxfoldover2", "best_minFold", "best_minFoldMean")
    
    if(length(values$featureTables$tables[[values$featureTables$active]]$anagrouptable$Group) > 0){
      groupCols <- paste0(unique(c(
        values$featureTables$tables[[values$featureTables$active]]$anagrouptable$Group)),
        "__")}
    else{
      groupCols <- NULL
    }
    
    internalValues$GroupConflicts <- unlist(sapply(c(groupCols,
                                                     groupSpecificCols),
                                                   grep,
                                                   cnames ))
    
    names(internalValues$GroupConflicts) <- cnames[internalValues$GroupConflicts]
    
    
    # columns with __XIC do not represent a conflict
    noConflict <- grep("__XIC", cnames[internalValues$GroupConflicts])
    if(length(noConflict) >0){
      internalValues$GroupConflicts <- internalValues$GroupConflicts[-noConflict]
    }
    
    
    if(length(internalValues$GroupConflicts) > 0 ){
      showModal(
        modalDialog(
          p(strong("There are already results from a grouped analysis in the table.")),
          p("It is recommended to remove the following columns from the column to avoid incorrect Group analysis information:"),
          hr(),
          selectizeInput(ns("selectDelete"), "Columns to be deleted:",
                         choices = as.list(internalValues$GroupConflicts),
                         selected = internalValues$GroupConflicts,
                         multiple = T),
          hr(),
          p("Do you want to delete these columns from the table?"),
          checkboxInput(ns('deleteCheck'), 'Delete these columns', value = T),
          title = "Warning",
          easyClose = F,
          footer = actionButton(ns("modalOk"), "Ok")
        ))
      
    }
    
    
    values$featureTables$tables[[values$featureTables$active]] <- updateFTgrouping(values$featureTables$tables[[values$featureTables$active]],
                                                                                   NULL)
    GroupingTable$df <- NULL
    GroupingTable$update <- !GroupingTable$update
    
    showNotification(paste("Removed all Grouping information - select Columns to generate new grouping information"), duration = 15, type = "warning")
    
    
  })
  
  observeEvent(input$modalOk,{
    print(input$selectDelete)
    if(input$deleteCheck){
      values$featureTables$tables[[values$featureTables$active]]$df <- values$featureTables$tables[[values$featureTables$active]]$df[,-as.integer(input$selectDelete)]
    }
    removeModal()
  })
  
  internalValues <- reactiveValues(inputTable = NULL,
                                   colrange = NULL, #columns in tablestuff$tablecut containing intensity values of interest
                                   anagroupraw = NULL, #columnnames in tablestuff$tablecut containing intensity values of interest with their respective analysis group (dataframe)
                                   tablename = NULL,
                                   #Preview = Preview,
                                   GroupingTable = GroupingTable
                                   
  )
  
  # observe({print(Preview$selected_cols)})
  return(internalValues)
}

#' ChangeFTGroupingModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
ChangeFTGroupingModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      shinydashboard::box(title = "Group intensity columns",
                          width = 3,
                          height = "415px",
                          fluidPage(
                            
                            fluidRow(
                              
                              column(6,
                                     div(class = "centerContainer",
                                         SaveTableModuleUI(ns("saveGrouping")))),
                              column(6,
                                     div(class = "centerContainer",
                                         LoadTableModuleUI(ns("tableLoader"))))
                            ),
                            fluidRow(
                              div(class = "centerContainer",
                                  simpleTableModuleUI(ns('groupingTable'))))
                          ),
                          footer =  div(class = "bottomHolder", fluidPage(
                            fluidRow(
                              column(6,
                                     div(title = "Remove all Grouping information",
                                         actionButton(ns("removeGroups"),"Remove Grouping"))),
                              column(6,
                                     div(title = "Use this grouping information for the active Feature Table",
                                         actionButton(ns("updateGroups"),"Update Grouping")))
                            )
                          )
                          )
      )
      ,
      shinydashboard::box(title = "Feature table preview",
                          width = 9,
                          height = "415px",
                          
                          fluidPage(
                            fluidRow(div(style = "height:34px;")),
                            
                            fluidRow(              
                              simpleTableModuleUI(ns('preview'))
                              
                            )),
                          
                          footer = div(class = "bottomHolder", fluidPage(
                            fluidRow(
                              div(title = "The selected columns should contain all intensity columns of interest!",
                                  actionButton(ns("intcols"),"Select Columns")
                              )
                            )
                          )
                          )
      )
    )
    
  )
}