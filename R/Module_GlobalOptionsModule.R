#' GlobalOptionsModule
#' 
#' Module to change some global options in \code{values$GlobalOpts} from the UI
#' 
#' @inherit MseekModules
#' 
#' @describeIn GlobalOptionsModule server logic
#' 
#' @import shiny
#' @importFrom parallel detectCores
#' 
#' @export 
GlobalOptionsModule <- function(input,output, session, values){
  #### Initialization ####
  
  ns <- NS(session$ns(NULL))
  
   output$EnabledCores <- renderUI({
     
     div(id = ns("EnabledCoresDiv"), title= "How many cores should be used for parallel processing?",
         numericInput(ns('EnabledCores'), 'Enabled cores',
                        value = values$GlobalOpts$enabledCores,
                        min = 1,
                      max = parallel::detectCores(all.tests = FALSE, logical = TRUE),
                      step = 1)
         
     )
   })
   
   output$pAdjustMethod <- renderUI({
      
      div(id = ns("pAdjustMethodDiv"), title= "How to adjust p-values from ANOVA and T-test analyses. Choices are from R's p.adjust.methods",
          selectizeInput(ns('padjustmethod'), 'P-value adjustment Method',
                       choices = c("fdr", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY"),
                         selected = values$GlobalOpts$p.adjust.method,
                       
                       )
          
      )
   })
   
   output$perPage <- renderUI({
     
     div(id = ns("perPageDiv"), 
         title = "How many features should be shown per page?",
         numericInput(ns('perPage'), 
                      'Per page',
                      value = values$GlobalOpts$perPage,
                      min = 1,
                      max = 200,
                      step = 1))
   })
   
   MSFolder <- callModule(FilePathModule, "msFolder",
                          filepaths = reactive({values$GlobalOpts$filePaths}),
                          label = "Database Folder", description= "Select folder for MS annotation database",
                          displayFolder = T)
   
   MSFolder$dir <- .MseekOptions$msdatabaseFolder
     
    observeEvent(input$EnabledCores,{
     values$GlobalOpts$enabledCores <- input$EnabledCores
     MseekOptions(enabledCores = input$EnabledCores)
   })
    
    observeEvent(input$padjustmethod,{
       values$GlobalOpts$p.adjust.method <- input$padjustmethod
       MseekOptions(p.adjust.method = input$padjustmethod)
    })
   
   observeEvent(input$perPage,{
     values$GlobalOpts$perPage <- input$perPage
     MseekOptions(perPage=input$perPage)
   })
  
   observeEvent(MSFolder$dir,{
if(length(SiriusFolder$dir) > 0 
   && !is.na(MSFolder$dir)
   ){
       values$GlobalOpts$msdatabaseFolder <- MSFolder$dir
       MseekOptions(msdatabaseFolder=MSFolder$dir)
     }
   }, ignoreInit =T)
   
 
   

}

#' @describeIn GlobalOptionsModule UI elements
#' @export
GlobalOptionsModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    title = "Global Options Module",
  fluidRow(
    column(3,
          htmlOutput(ns("EnabledCores"))
    ),
    column(3,
          htmlOutput(ns("perPage"))
    ),
    column(6,
           htmlOutput(ns("pAdjustMethod"))
    )
    ),
    fluidRow(
      column(6,
           FilePathModuleUI(ns("msFolder"))

    )
  )
  )
  
}


