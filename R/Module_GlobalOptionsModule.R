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
     
   
   SiriusFolder <- callModule(FilePathModule, "siriusFolder",
                              filepaths = reactive({values$GlobalOpts$filePaths}),
                              label = "Sirius Folder", description= "Select folder that contains the sirius executable (sirius-console-64.exe or sirius in linux/macOS)",
                              displayFolder = T)
   
   SiriusFolder$dir <- .MseekOptions$siriusFolder
   
    observeEvent(input$EnabledCores,{
     values$GlobalOpts$enabledCores <- input$EnabledCores
     MseekOptions(enabledCores = input$EnabledCores)
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
   
   observeEvent(SiriusFolder$dir,{
     if(length(SiriusFolder$dir) > 0 && !is.na(SiriusFolder$dir)){
     values$GlobalOpts$siriusFolder <- SiriusFolder$dir
     MseekOptions(siriusFolder=SiriusFolder$dir)
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
    column(6,
          htmlOutput(ns("EnabledCores"))
    ),
    column(6,
          htmlOutput(ns("perPage"))
    )),
    fluidRow(
      column(6,
           FilePathModuleUI(ns("msFolder"))

    ),
    column(6,
           FilePathModuleUI(ns("siriusFolder")))
  )
  )
  
}


