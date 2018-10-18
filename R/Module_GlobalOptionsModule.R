#' GlobalOptionsModule
#' 
#' 
#' Module to change global options
#' 
#' @param input 
#' @param output 
#' @param session 
#' 
#' @import shiny
#' @importFrom parallel detectCores
#' 
#' @export 
GlobalOptionsModule <- function(input,output, session,
                                values = reactiveValues(GlobalOpts = GlobalOpts)){
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
   
   MSFolder$dir <- .MosaicOptions$msdatabaseFolder
     
   
   SiriusFolder <- callModule(FilePathModule, "siriusFolder",
                              filepaths = reactive({values$GlobalOpts$filePaths}),
                              label = "Sirius Folder", description= "Select folder that contains the sirius executable (sirius-console-64.exe or sirius in linux/macOS)",
                              displayFolder = T)
   
   SiriusFolder$dir <- .MosaicOptions$siriusFolder
   
    observeEvent(input$EnabledCores,{
     values$GlobalOpts$enabledCores <- input$EnabledCores
     MosaicOptions(enabledCores = input$EnabledCores)
   })
   
   observeEvent(input$perPage,{
     values$GlobalOpts$perPage <- input$perPage
     MosaicOptions(perPage=input$perPage)
   })
  
   observeEvent(MSFolder$dir,{
if(length(SiriusFolder$dir) > 0 
   && !is.na(MSFolder$dir)
   ){
       values$GlobalOpts$msdatabaseFolder <- MSFolder$dir
       MosaicOptions(msdatabaseFolder=MSFolder$dir)
     }
   }, ignoreInit =T)
   
   observeEvent(SiriusFolder$dir,{
     if(length(SiriusFolder$dir) > 0 && !is.na(SiriusFolder$dir)){
     values$GlobalOpts$siriusFolder <- SiriusFolder$dir
     MosaicOptions(siriusFolder=SiriusFolder$dir)
     }
   }, ignoreInit =T)
   

}

#' GlobalOptionsModuleUI
#' 
#' @param id id of the shiny module
#' 
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


