#' WelcomePageModule
#' 
#' 
#' module to welcome Mseek users.
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
      # div(title= "Welcome to Mseek!",
      fluidPage(
        # fluidRow(
        #   column(3),
        #   column(6,
        #          img(src = "/img/Mseek_logo.png",
        #              alt = "Mseek", style = "width:100%")
        #   ),
        #   column(3)
        #   ),
        fluidRow(
          shinydashboard::box(status = "primary", width = 12, solidHeader = T,
                              title = "Welcome to METABOseek!",
                              fluidPage(
                                fluidRow(
                                  
                                  p("Load your data below with the buttons below. You can load a Feature Table, any number of compatible MS data files, or a Project folder.", style = "text-align:center;"),
                                hr()
                                  ),
                                fluidRow(
                                 
                                         LoadDataModuleUI(ns("startdataload"))
                                  ),
                                fluidRow(
                                  div(style="height:5px")
                                )
                              )
          )
          
        ),
        fluidRow(
          div(class = "box box-solid box-primary",
              div(class = "box-header",
                  h3(class = "box-title", paste0("This is METABOseek version ",packageVersion("METABOseek")))))
         
        ),
        
        div(style="height:4px;"),
        
        fluidRow(
          tryCatch({
            rl <- readLines(paste0('http://metaboseek.com/integrated/', paste(packageVersion("METABOseek")[[1]],collapse = ".")), n = 1)
            
            HTML('
<iframe id="inlineFrameWelcome"
title="webpage" 
style="border:none;width:100%;height:500px;" ',
                 paste0('src="http://metaboseek.com/integrated/', paste(packageVersion("METABOseek")[[1]],collapse = "."),'">'),
                 #paste0('src="http://metaboseek.com">'),
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
