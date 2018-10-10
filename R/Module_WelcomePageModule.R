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
                              values = reactiveValues(MSData = MSData)){
  
  ns <- NS(session$ns(NULL))
  
  output$web <- renderUI({
    
    if(is.null(values$MSData$data)){
    # div(title= "Welcome to Mosaic!",
    box(width = 12, status= "primary",
      fluidPage(
      fluidRow(
        column(6,
               div(style="background-color:#595959",
               img(src = "/img/mosaic_logo.png",
                   alt = "Mosaic", style = "width:100%"))
               ),
        tryCatch({
          rl <- readLines(paste0('http://mosaic.bti.cornell.edu/welcome/integrated/', paste(packageVersion("Mosaic")[[1]],collapse = ".")), n = 1)
          column(6,
                 HTML('
<iframe id="inlineFrameExample"
title="webpage" 
style="border:none;width:100%;height:265px;" ',
paste0('src="http://mosaic.bti.cornell.edu/welcome/integrated/', paste(packageVersion("Mosaic")[[1]],collapse = "."),'">'),
#paste0('src="http://mosaic.bti.cornell.edu/welcome/">'),
'</iframe>
              ')
          )
          
        },
        error = function(e){
          column(8,
                 h3("Welcome to Mosaic"),
                 p("No MS data loaded")
          )
        })
      )
    )
)
    }
    
  })
}
