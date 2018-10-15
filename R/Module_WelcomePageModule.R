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
                              show = reactive({T})){
  
  ns <- NS(session$ns(NULL))
  
  output$web <- renderUI({
    
    if(show()){
    # div(title= "Welcome to Mosaic!",
    
      fluidRow(
        column(3,
               div(style="background-color:#595959",
               img(src = "/img/mosaic_logo.png",
                   alt = "Mosaic", style = "width:100%"))
               ),
        tryCatch({
          rl <- readLines(paste0('http://mosaic.bti.cornell.edu/welcome/integrated/', paste(packageVersion("Mosaic")[[1]],collapse = ".")), n = 1)
          column(9,
                 HTML('
<iframe id="inlineFrameExample"
title="webpage" 
style="border:none;width:100%;height:130px;" ',
paste0('src="http://mosaic.bti.cornell.edu/welcome/integrated/', paste(packageVersion("Mosaic")[[1]],collapse = "."),'">'),
#paste0('src="http://mosaic.bti.cornell.edu/welcome/">'),
'</iframe>
              ')
          )
          
        },
        error = function(e){
          column(9,
                 h3("Welcome to Mosaic", style="color:#ffffff;")#,
                 #p("No MS data loaded", style="color:#ffffff;")
          )
        })
      )
 
    }
    
  })
}
