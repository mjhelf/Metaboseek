#' WelcomePageModule
#' 
#' 
#' module to welcome Mosaic users.
#' 
#' @param input 
#' @param output 
#' @param session 
#' 
#' 
#' @import shiny shinyFiles
#' 
#' @export
WelcomePageModule <- function(input,output, session,
                              values = reactiveValues(projectData = values$projectData,
                                                      featureTables = values$featureTables,
                                                      MSData = values$MSData,
                                                      GlobalOpts = values$GlobalOpts),
                              show = reactive({T})){
  
  ns <- NS(session$ns(NULL))
  
  StartDataLoad <- callModule(LoadDataModule, "startdataload",
                              values = reactiveValues(projectData = values$projectData,
                                                      featureTables = values$featureTables,
                                                      MSData = values$MSData,
                                                      GlobalOpts = values$GlobalOpts)
  )
  
  internalValues <- reactiveValues(explore = F,
                                   StartDataLoad = StartDataLoad)
  
  observeEvent(c(values$MSData$data, values$featureTables$tables),{
    
    if(!is.null(values$MSData$data) || length(values$featureTables$index) > 1){
      
      internalValues$explore <- T
    }
  })
  
  output$web <- renderUI({
    
    if(show()){
      # div(title= "Welcome to Mosaic!",
      fluidPage(
        fluidRow(
          column(3),
          column(6,
                 img(src = "/img/mosaic_logo.png",
                     alt = "Mosaic", style = "width:100%")
          ),
          column(3)
          ),
        fluidRow(
          shinydashboard::box(status = "primary", width = 12,
                              fluidPage(
                                fluidRow(
                                  h4("Welcome to Mosaic!", style = "text-align:center;"),
                                  p("Go ahead and load your data below. You can load a Feature Table, any number of compatible MS data files, or a Project folder."),
                                hr()
                                  ),
                                fluidRow(
                                 
                                         LoadDataModuleUI(ns("startdataload"))
                                  )
                              )
          )
          
        ),
        
        
        fluidRow(
          tryCatch({
            rl <- readLines(paste0('http://mosaic.bti.cornell.edu/welcome/integrated/', paste(packageVersion("Mosaic")[[1]],collapse = ".")), n = 1)
            
            HTML('
<iframe id="inlineFrameExample"
title="webpage" 
style="border:none;width:100%;height:500px;" ',
                 paste0('src="http://mosaic.bti.cornell.edu/welcome/integrated/', paste(packageVersion("Mosaic")[[1]],collapse = "."),'">'),
                 #paste0('src="http://mosaic.bti.cornell.edu/welcome/">'),
                 '</iframe>
              ')
            
            
          },
          error = function(e){
            
            
            
          })
        )
        
      )
    }
    
  })
  
  return(internalValues)
}


#' WelcomePageModuleUI
#' 
#' 
#' @param id id of the shiny module
#' 
#' @export
WelcomePageModuleUI <- function(id){
  ns <- NS(id)
  
  
  
  htmlOutput(ns("web"))
  
  
}
