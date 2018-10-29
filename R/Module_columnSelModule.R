#' columnSelModule
#' 
#' 
#' server module for selection of columns to show in main Table
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values import reactiveValues from the shiny session
#' @param load data to load from previous session (not implemented)
#' 
#' @import shiny
#' 
#' @export 
ColumnSelModule <- function(input,output, session,
                       
                       values = reactiveValues(featureTables = featureTables,
                                               MainTable = MainTable)
){
  ns <- NS(session$ns(NULL))
  
  
  internalValues <- reactiveValues(selectedGroup = NULL,
                                   gPropsSelected = NULL,
                                   sPropsSelected = NULL,
                                   intensitiesSelected = NULL,
                                   othersSelected = NULL
  )
  
observeEvent(c(#values$featureTables$active,
               colnames(values$featureTables$tables[[values$featureTables$active]]$df)
               ),{
                 
                 if( !identical(internalValues$colnames, colnames(values$featureTables$tables[[values$featureTables$active]]$df))){
                 
                   
                   if(!is.null(values$featureTables$tables[[values$featureTables$active]]$gNames) 
                      && (is.null(internalValues$selectedGroup)
                      || !internalValues$selectedGroup %in% values$featureTables$tables[[values$featureTables$active]]$gNames)){
                   internalValues$selectedGroup <- values$featureTables$tables[[values$featureTables$active]]$gNames[1]
                   }
                   
                   
                 internalValues$colnames <- colnames(values$featureTables$tables[[values$featureTables$active]]$df)
                 
                 values$featureTables <- values$featureTables
                 if(!is.null(internalValues$selectedGroup)){
                   
                 internalValues$gPropsSelected <- values$featureTables$tables[[values$featureTables$active]]$gProps[[internalValues$selectedGroup]]
                 
                 internalValues$sPropsSelected <- values$featureTables$tables[[values$featureTables$active]]$sProps[[internalValues$selectedGroup]]
                 internalValues$intensitiesSelected <- values$featureTables$tables[[values$featureTables$active]]$anagroupnames[[internalValues$selectedGroup]]
                 }
                 }
                 
                 
                 
                 internalValues$othersSelected <- unique(c(internalValues$colnames[internalValues$colnames %in% internalValues$othersSelected],
                                                    values$featureTables$tables[[values$featureTables$active]]$summaryStats))
                 
                 })

# observeEven(values$featureTables$loadedColumns,{
#   
#   if(!is.null(values$featureTables$loadedColumns)){
#     
#     internalValues$selectedGroup <- values$featureTables$loadedColumns$activeGroup
#   
#     if(!is.null(internalValues$selectedGroup)){
#   internalValues$gPropsSelected <- values$featureTables$loadedColumns$columns[values$featureTables$loadedColumns$columns %in% values$featureTables$tables[[values$featureTables$active]]$gProps[[internalValues$selectedGroup]]]
#   
#   internalValues$sPropsSelected <- values$featureTables$loadedColumns$columns[values$featureTables$loadedColumns$columns %in% values$featureTables$tables[[values$featureTables$active]]$sProps[[internalValues$selectedGroup]]]
#   internalValues$intensitiesSelected <- values$featureTables$loadedColumns$columns[values$featureTables$loadedColumns$columns %in% values$featureTables$tables[[values$featureTables$active]]$anagroupnames[[internalValues$selectedGroup]]]
#     internalValues$othersSelected <- values$featureTables$loadedColumns$columns[!values$featureTables$loadedColumns$columns %in% c(internalValues$gPropsSelected, internalValues$sPropsSelected, internalValues$intensitiesSelected)]
#     } 
#     else{
#       internalValues$othersSelected <- values$featureTables$loadedColumns$columns
#       
#     }
#   }
# })




# internalValues$othersSelected <- unique(c(internalValues$colnames[internalValues$colnames %in% internalValues$othersSelected],
#                                           values$featureTables$tables[[values$featureTables$active]]$summaryStats))
#   
  ###Column Selection
  
  output$mainSelGroup <- renderUI({
    selectizeInput(ns('mainSelGroup'), 'Group of interest',
                                                  choices = values$featureTables$tables[[values$featureTables$active]]$gNames,
                                                  selected = internalValues$selectedGroup,
                                                  multiple = F,
                                                  width = '100%')})
  
  output$mainSelgProps <- renderUI({selectizeInput(ns('mainSelgProps'), 'Group properties', 
                                                   choices = values$featureTables$tables[[values$featureTables$active]]$gProps,
                                                   selected = internalValues$gPropsSelected,
                                                   multiple = T,
                                                   width = '100%')
  })
  
  observeEvent(c(input$mainSelgProps),
               {internalValues$gPropsSelected <- input$mainSelgProps })
  
  
  output$mainSelsProps <- renderUI({selectizeInput(ns('mainSelsProps'), 'Sample properties', 
                                                   choices = values$featureTables$tables[[values$featureTables$active]]$sProps,
                                                   selected = internalValues$sPropsSelected,
                                                   multiple = T,
                                                   width = '100%')})
  
  observeEvent(c(input$mainSelsProps),
               {internalValues$sPropsSelected <- input$mainSelsProps })
  
  output$mainSelIntensities <- renderUI({
    intShowAs <- values$featureTables$tables[[values$featureTables$active]]$anagroupnames
    singlegroups <- which(sapply(intShowAs,length) == 1)
    for (i in singlegroups){
      names(intShowAs[[i]]) <- intShowAs[[i]]
    }
    intNormShowAs <- values$featureTables$tables[[values$featureTables$active]]$anagroupnames_norm
    singlegroups <- which(sapply(intNormShowAs,length) == 1)
    for (i in singlegroups){
      names(intNormShowAs[[i]]) <- intNormShowAs[[i]]
    }
    
    selectizeInput(ns('mainSelIntensities'), 'Sample intensities', 
                   choices = list(Intensities = intShowAs,
                                  "Normalized Intensities" = intNormShowAs),
                   selected = internalValues$intensitiesSelected,
                   multiple = T,
                   width = '100%')})
  
  observeEvent(c(input$mainSelIntensities),
               {internalValues$intensitiesSelected <- input$mainSelIntensities })
  
  
  output$mainSelOthers <- renderUI({
    baseStats <- values$featureTables$tables[[values$featureTables$active]]$summaryStats
    names(baseStats) <- values$featureTables$tables[[values$featureTables$active]]$summaryStats
    Others <- values$featureTables$tables[[values$featureTables$active]]$others
    names(Others) <- values$featureTables$tables[[values$featureTables$active]]$others
    
    
    selectizeInput(ns('mainSelOthers'), 'other columns', 
                   choices = list("Basic Stats" = baseStats,
                                  "Others" = Others),
                   selected =  internalValues$othersSelected, 
                   multiple = T,
                   width = '100%'
    )})
  
  
  observeEvent(c(internalValues$gPropsSelected,
                 internalValues$sPropsSelected,
                 internalValues$intensitiesSelected,
                 internalValues$othersSelected),{
    
                   
                   TableUpdateChunk()                   
                   
    values$featureTables$selectedCols <- unique(unname(c(values$featureTables$tables[[values$featureTables$active]]$core,
                                                          values$featureTables$tables[[values$featureTables$active]]$comments,
                                                          internalValues$gPropsSelected,
                                                          internalValues$sPropsSelected,
                                                          internalValues$intensitiesSelected,
                                                          internalValues$othersSelected
                                                          )))
  })
  
  observeEvent(c(input$mainSelGroup),
               {
               if(!is.null(input$mainSelGroup)){
               internalValues$selectedGroup <- input$mainSelGroup
               }
               internalValues$gPropsSelected <- values$featureTables$tables[[values$featureTables$active]]$gProps[[internalValues$selectedGroup]]
               internalValues$sPropsSelected <- values$featureTables$tables[[values$featureTables$active]]$sProps[[internalValues$selectedGroup]]
               internalValues$intensitiesSelected <- values$featureTables$tables[[values$featureTables$active]]$anagroupnames[[internalValues$selectedGroup]]})
  
  observeEvent(c(input$mainSelOthers),
               {internalValues$othersSelected <- input$mainSelOthers })
  
  return(internalValues)
  
}


#' columnSelModuleUI
#' 
#' 
#' UI module for columnsel Module
#' 
#' @param id id to be used in ns()
#' 
#' @import shiny
#' 
#' @export 
ColumnSelModuleUI <-  function(id){
ns <- NS(id)

fluidPage(
  htmlOutput(ns('selnormdata')),
  htmlOutput(ns('mainSelGroup')),
  htmlOutput(ns('mainSelgProps')),
  htmlOutput(ns('mainSelsProps')),
  htmlOutput(ns('mainSelIntensities')),
  htmlOutput(ns('mainSelOthers'))
)

}