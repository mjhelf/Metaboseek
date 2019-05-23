#' FeatureTableContainer
#' 
#' Module containing the MainTableModule
#' 
#' @inherit MseekContainers
#' @describeIn FeatureTableContainer server logic module, to be called with \link[shiny]{callModule}()
#' 
#' @export 
FeatureTableContainer <- function(input,output, session,
                            values = reactiveValues(featureTables = featureTables,
                                                    MSData = MSData,
                                                    GlobalOpts = GlobalOpts,
                                                    projectData = projectData)
){
  
  MainTable <- callModule(MainTableModule, "maintable",
                          values = reactiveValues(featureTables = values$featureTables,
                                                  GlobalOpts = values$GlobalOpts,
                                                  projectData = values$projectData),
                          static = list(height = 300,
                                        readOnly = T,
                                        contextMenu = T,
                                        fixedColumnsLeft = 1,
                                        invertReadOnly = NULL,
                                        controls = T,
                                        format = list(col = NULL,
                                                      format = NULL)))
  
  
  ####TODO move these modules out of this container and make the feature table box more compact/ not a tabBox
  #TabGrouping <- 
      callModule(ChangeFTGroupingModule, "tabgrouping",
                          reactives = reactive({list()}),
                          values = reactiveValues(fileGrouping = NULL,
                                                  featureTables = values$featureTables,
                                                  MSData = values$MSData,
                                                  projectData = values$projectData),
                          static = list()
  )
  

  
  #TabAnalysis <- 
      callModule(TableAnalysisModule, "tabanalysis",
                          reactives = reactive({list()}),
                          values = reactiveValues(fileGrouping = NULL,
                                                  GlobalOpts = values$GlobalOpts,
                                                  featureTables = values$featureTables,
                                                  MSData= values$MSData,
                                                  MainTable = MainTable))

    
  internalValues <- reactiveValues(MainTable = MainTable#,
                                  # TabGrouping = TabGrouping,
                                   #TabAnalysis = TabAnalysis
                                   )
  
  return(internalValues)
  
}

#' @describeIn FeatureTableContainer returns the \code{shiny} UI elements for the Main Table - containing box
#' 
#' @export
FeatureTableContainerUI <- function(id){
  ns <- NS(id)
  
  tabBox(title = "Feature table",
         id = ns("TableBox"),
         #status = "primary",
         #collapsible = T,
         width = 12,
         side = "right",
         selected = "View Table",#selectedTabs$FeatureTable,
         
         tabPanel("_"),
         
         tabPanel("Regroup Table",
                  ChangeFTGroupingModuleUI(ns("tabgrouping"))
                  ),
         tabPanel("Analyze Table",
                  TableAnalysisModuleUI(ns("tabanalysis"))
         ),
         tabPanel("View Table",
                MainTableModuleUI(ns("maintable"))
         )
  )
  
  
}