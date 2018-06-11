#' columnSelModule
#' 
#' 
#' server module for selection of columns to show in main Table
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives import reactive data from the shiny session
#' @param values import reactiveValues from the shiny session
#' @param static import static values
#' @param load data to load from previous session (not implemented)
#' 
#' @import shiny
#' 
#' @export 
columnSelModule <- function(input,output, session,
                       
                       reactives = reactive({(list())}),
                       values = reactiveValues(),
                       static = list(servermode = F,
                                     rootpath = "/"),
                       load = reactive({list()})
){
  ns <- NS(session$ns(NULL))
  
  internalStatic <- c(list(Mversion =  1),
                      static)
  
  
  internalValues <- reactiveValues(featureTables = NULL,
                                   selectedCols = NULL,
                                   gPropsSelected = NULL,
                                   sPropsSelected = NULL,
                                   intensitiesSelected = NULL
  )
  
observeEvent(c(#values$featureTables$active,
               colnames(values$featureTables$tables[[values$featureTables$active]]$df)
               ),{
                 
                 if( !identical(internalValues$colnames, colnames(values$featureTables$tables[[values$featureTables$active]]$df))){
                 internalValues$colnames <- colnames(values$featureTables$tables[[values$featureTables$active]]$df)
                 
                 internalValues$featureTables <- values$featureTables
                 internalValues$gPropsSelected <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$gProps[[internalValues$featureTables$tables[[internalValues$featureTables$active]]$selectedGroup]]
                 internalValues$sPropsSelected <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$sProps[[internalValues$featureTables$tables[[internalValues$featureTables$active]]$selectedGroup]]
                 internalValues$intensitiesSelected <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$anagroupnames[[internalValues$featureTables$tables[[internalValues$featureTables$active]]$selectedGroup]]
                 }
                 })
  
  ###Column Selection
  
  output$mainSelGroup <- renderUI({selectizeInput(ns('mainSelGroup'), 'Group of interest',
                                                  choices = internalValues$featureTables$tables[[internalValues$featureTables$active]]$gNames,
                                                  selected = internalValues$featureTables$tables[[internalValues$featureTables$active]]$selectedGroup,
                                                  multiple = F,
                                                  width = '100%')})
  
  output$mainSelgProps <- renderUI({selectizeInput(ns('mainSelgProps'), 'Group properties', 
                                                   choices = internalValues$featureTables$tables[[internalValues$featureTables$active]]$gProps,
                                                   selected = internalValues$gPropsSelected,
                                                   multiple = T,
                                                   width = '100%')
  })
  
  observeEvent(c(input$mainSelgProps),
               {internalValues$gPropsSelected <- input$mainSelgProps })
  
  
  output$mainSelsProps <- renderUI({selectizeInput(ns('mainSelsProps'), 'Sample properties', 
                                                   choices = internalValues$featureTables$tables[[internalValues$featureTables$active]]$sProps,
                                                   selected = internalValues$sPropsSelected,
                                                   multiple = T,
                                                   width = '100%')})
  
  observeEvent(c(input$mainSelsProps),
               {internalValues$sPropsSelected <- input$mainSelsProps })
  
  output$mainSelIntensities <- renderUI({
    intShowAs <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$anagroupnames
    singlegroups <- which(sapply(intShowAs,length) == 1)
    for (i in singlegroups){
      names(intShowAs[[i]]) <- intShowAs[[i]]
    }
    intNormShowAs <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$anagroupnames_norm
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
    baseStats <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$summaryStats
    names(baseStats) <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$summaryStats
    Others <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$others
    names(Others) <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$others
    
    
    selectizeInput(ns('mainSelOthers'), 'other columns', 
                   choices = list("Basic Stats" = baseStats,
                                  "Others" = Others),
                   selected = internalValues$featureTables$tables[[internalValues$featureTables$active]]$summaryStats, 
                   multiple = T,
                   width = '100%'
    )})
  
  
  observe({internalValues$selectedCols <- unique(unname(c(internalValues$featureTables$tables[[internalValues$featureTables$active]]$core,
                                                          internalValues$featureTables$tables[[internalValues$featureTables$active]]$comments,
                                                          input$mainSelgProps,
                                                          input$mainSelsProps,
                                                          input$mainSelIntensities,
                                                          input$mainSelOthers)))
  })
  
  observeEvent(c(input$mainSelGroup),
               {
               if(!is.null(input$mainSelGroup)){
               internalValues$featureTables$tables[[internalValues$featureTables$active]]$selectedGroup <- input$mainSelGroup
               }
               internalValues$gPropsSelected <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$gProps[[internalValues$featureTables$tables[[internalValues$featureTables$active]]$selectedGroup]]
               internalValues$sPropsSelected <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$sProps[[internalValues$featureTables$tables[[internalValues$featureTables$active]]$selectedGroup]]
               internalValues$intensitiesSelected <- internalValues$featureTables$tables[[internalValues$featureTables$active]]$anagroupnames[[internalValues$featureTables$tables[[internalValues$featureTables$active]]$selectedGroup]]})
  
  observeEvent(c(input$mainSelOthers),
               {internalValues$featureTables$tables[[internalValues$featureTables$active]]$summaryStats <- input$mainSelOthers })
  
  return(internalValues)
  
}


#' xcmsModuleUI
#' 
#' 
#' UI module for xcms Module
#' 
#' @param id id to be used in ns()
#' 
#' @import shiny
#' 
#' @export 
columnSelModuleUI <-  function(id){
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