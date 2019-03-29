#' RegroupMSDataModule
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
RegroupMSDataModule <- function(input,output, session,
                                reactives = reactive({list()}),
                                values = reactiveValues(MSData = MSData,
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
  
  callModule(SaveTableModule, "saveGrouping",
             reactives = reactive({list(df = GroupingTable$liveView,
                                        filename = file.path("MS_data_grouping_", paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),"_",SelectMSGrouping$active,".msGrouping")))}),
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
                                          pattern = "\\.msGrouping$")
  )
  
  SelectMSGrouping <- callModule(SelectMSGroupingModule, "selectLayout",
                                 values = reactiveValues(MSData = values$MSData),
                                 static = list(editOnly = T)
  )
  
  
  observeEvent(SelectMSGrouping$active,{
    GroupingTable$df <- values$MSData$layouts[[SelectMSGrouping$active]]$rawgrouptable[,c("File", "Group", "Group2")]
    GroupingTable$update <- !GroupingTable$update
    
  })
  
  observeEvent(TableLoader$df,{
    
    
    
    
    
    if(!is.null(TableLoader$df)){
      tryCatch({
      GroupingTable$df <- TableLoader$df
      GroupingTable$update <- !GroupingTable$update

      values$MSData$layouts[[basename(TableLoader$filename)]] <-  constructRawLayout(TableLoader$df, stem = NULL)  
      SelectMSGrouping$active <- basename(TableLoader$filename)
      },
      error = function(e){
        showNotification(paste("ERROR: THis table could not be,loaded as a grouping scheme"), type = "error", duration = 15)
        
      })
      #should in the future trigger the checking of filenames, etc

      
    }
  })
  
  
  
  # when pressing Select Columns button (intcols)
  ###Override default column range with selected columns when pressing Button intcols, and load new anagrouptable template
  observeEvent(c(input$updateGroups),{
    if(!is.null(GroupingTable$liveView)){
      
      if(!all(basename(GroupingTable$liveView$File) %in%  basename(names(values$MSData$data)))){
        internalValues$missingFiles <-  GroupingTable$liveView$File[!basename(GroupingTable$liveView$File) %in%  basename(names(values$MSData$data))]
        showModal(modalDialog(
          p("These MSData files are not currently loaded:"),
          p(paste(basename(internalValues$missingFiles), collapse = ", ")),
          p("These file names will be removed from the grouping scheme. Load these files into Mseek first."),
          actionButton(ns("modalRemoveEntries"), "OK"),
          title = "Not all files are loaded!",
          easyClose = F,
          fade = F,
          size = "m",
          footer = modalButton("Cancel") 
        ))
        
      }else{
        if(!all(GroupingTable$liveView$File %in%  names(values$MSData$data))){
          fixedTable <- GroupingTable$liveView
          fixedTable$File <- names(values$MSData$data)[sapply(lapply(basename(GroupingTable$liveView$File), "==", basename(names(values$MSData$data))), which)]
          values$MSData$layouts[[SelectMSGrouping$active]] <-  constructRawLayout(fixedTable, stem = NULL)  
          values$MSData$active <- SelectMSGrouping$active
          GroupingTable$df <- fixedTable
          GroupingTable$update <- !GroupingTable$update
        }
        
      values$MSData$layouts[[SelectMSGrouping$active]] <-  constructRawLayout(GroupingTable$liveView, stem = NULL)  
      values$MSData$active <- SelectMSGrouping$active
      }
    }
    
  })
  
  observeEvent(input$modalRemoveEntries,{
    removeModal()
    
    fixedTable  <- GroupingTable$liveView[basename(GroupingTable$liveView$File) %in%  basename(names(values$MSData$data)),]
    if(nrow(fixedTable) > 0){
      fixedTable$File <- names(values$MSData$data)[sapply(lapply(basename(fixedTable$File), "==", basename(names(values$MSData$data))), which)]
    values$MSData$layouts[[SelectMSGrouping$active]] <-  constructRawLayout(fixedTable, stem = NULL)  
    values$MSData$active <- SelectMSGrouping$active
    GroupingTable$df <- fixedTable
    GroupingTable$update <- !GroupingTable$update
    }
    
  })
  
  observeEvent(input$newGroup,{

       showModal(modalDialog(
          textInput(ns("newgroupingname"), "Name of new Grouping scheme:", value = paste0("Grouping", length(values$MSData$layouts)+1)),
          actionButton(ns("modalNewGroup"), "OK"),
          title = "Make a new grouping scheme",
          easyClose = F,
          fade = F,
          size = "m",
          footer = modalButton("Cancel") 
        ))
    
  })
  
  observeEvent(input$modalNewGroup,{
   if(!is.null(input$newgroupingname) && input$newgroupingname != ""){
     
     grouptable <- data.frame(File = names(values$MSData$data),
                              Group = rep("G1", length(names(values$MSData$data))),
                              Group2 = rep("G1", length(names(values$MSData$data))),
                              stringsAsFactors = F)
     
    values$MSData$layouts[[input$newgroupingname]] <-  constructRawLayout(grouptable, stem = NULL) 
    SelectMSGrouping$active <- input$newgroupingname
    
         removeModal()

   }else{
     showNotification(paste("ERROR: You did not provide a name for the grouping scheme"), type = "error", duration = 15)
   }
  
    
    
  })
  
  internalValues <- reactiveValues()
  
  # observe({print(Preview$selected_cols)})
  #return(internalValues)
}

#' RegroupMSDataModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
RegroupMSDataModuleUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      SelectMSGroupingModuleUI(ns("selectLayout"))
    ),
    fluidRow(
      div(style="display:inline-block",
                 SaveTableModuleUI(ns("saveGrouping"))),
      div(style="display:inline-block",
                 LoadTableModuleUI(ns("tableLoader")))
    ),
    fluidRow(
          simpleTableModuleUI(ns('groupingTable'))),
    fluidRow(
      div(style="display:inline-block",
             div(title = "Add a new grouping scheme",
                 actionButton(ns("newGroup"),"New Grouping"))),
      div(style="display:inline-block",
             div(title = "Update the selected grouping scheme for MS data",
                 actionButton(ns("updateGroups"),"Update Grouping")))
    )
  )
}