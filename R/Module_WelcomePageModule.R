#' WelcomePageModule
#' 
#' Start page module, shows update news and allows loading of data.
#' 
#' @inherit MseekModules
#' @describeIn WelcomePageModule Server logic
#' 
#' @return Returns nothing
#' 
#' @param show a reactive(), if show() is TRUE, will show update news
#' 
#' @import shiny shinyFiles
#' 
#' @examples 
#' \dontrun{
#' library(shiny)
#' 
#' ui <- MseekMinimalUI(WelcomePageModuleUI("examplemodule"), diagnostics = T)
#' 
#' server <- function(input, output, session) {
#'   MseekMinimalServer(diagnostics = T, data = F, tables = F)
#'   
#'   ExampleModule <- callModule(WelcomePageModule, "examplemodule", values)
#' }
#' 
#' # Run Shiny app
#' shinyApp(ui, server)
#' }
#' 
#' @export
WelcomePageModule <- function(input,output, session,
                              values = reactiveValues(projectData = values$projectData,
                                                      featureTables = values$featureTables,
                                                      MSData = values$MSData,
                                                      GlobalOpts = values$GlobalOpts),
                              show = reactive({TRUE})){
  
  ns <- NS(session$ns(NULL))
  
  StartDataLoad <- callModule(LoadDataModule, "startdataload",
                              values = reactiveValues(projectData = values$projectData,
                                                      featureTables = values$featureTables,
                                                      MSData = values$MSData,
                                                      GlobalOpts = values$GlobalOpts)
  )
  
  internalValues <- reactiveValues(explore = F,
                                   xcms_link = 0,
                                   StartDataLoad = StartDataLoad)
  
  observeEvent(c(values$MSData$data, values$featureTables$tables),{
    
    if(!is.null(values$MSData$data) || length(values$featureTables$index) > 1){
      
      internalValues$explore <- T
    }
  })
  
  observeEvent(input$xcms_link, {
    internalValues$xcms_link <-   internalValues$xcms_link + 1
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
                              title = "Welcome to Metaboseek!",
                              fluidPage(
                                fluidRow(
                                  
                                  h3("Load your data with the buttons below. You can load a Feature Table, any number of compatible MS data files, or a Project folder.", style = "text-align:center;"),
                                  h4("No Feature Table? Run an ", actionLink(ns("xcms_link"), "XCMS analysis"), style = "text-align:center;"),
                                  
                                  
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
                  h3(class = "box-title", paste0("This is Metaboseek version ",
                                                 packageVersion("Metaboseek")))))
         
        ),
        
        div(style="height:4px;"),
        
        ##NEWS work now, except when using MseekContainer for a version that does not have a website..
        fluidRow(
          tryCatch({
              
            rl <- readLines(paste0('http://metaboseek.com/integrated/',
                                   paste(strsplit(paste(packageVersion("Metaboseek")[[1]]),
                                                  "\\.")[[1]][1:3],
                                         collapse = ".")), n = 1)

            
    #using only first three numbers of version to determine site to load:
            HTML('
<iframe id="inlineFrameWelcome"
title="webpage" 
style="border:none;width:100%;height:500px;" ',
                 paste0('src="http://metaboseek.com/integrated/',
                        paste(strsplit(paste(packageVersion("Metaboseek")[[1]]),
                                       "\\.")[[1]][1:3],collapse = "."),'">'),
                 '</iframe>
              ')
            
            
          },
          error = function(e){
              if(!file.exists(system.file("app/www/NEWS.html", package = 'Metaboseek'))){
                  file.copy(system.file("NEWS.html", package = 'Metaboseek'),
                            file.path(system.file("app/www", package = 'Metaboseek'),
                                      "NEWS.html"))
              }
              
              HTML('
<iframe id="inlineFrameWelcome"
title="webpage" 
style="border:none;width:100%;height:500px;" ',
                   paste0('src="',"NEWS.html",'">'),
                   '</iframe>
              ')
            
          },
    warning = function(w){
        
        if(!file.exists(system.file("app/www/NEWS.html", package = 'Metaboseek'))){
            file.copy(system.file("NEWS.html", package = 'Metaboseek'),
                      file.path(system.file("app/www", package = 'Metaboseek'),
                                "NEWS.html"))
        }
        
        HTML('
             <iframe id="inlineFrameWelcome"
             title="webpage" 
             style="border:none;width:100%;height:500px;" ',
             paste0('src="',"NEWS.html",'">'),
             '</iframe>
             ')
        
    })
        )
        
      )
    }
    
  })
  
  return(internalValues)
}


#' @describeIn WelcomePageModule UI elements
#' @export
WelcomePageModuleUI <- function(id){
  ns <- NS(id)
  
  
  
  htmlOutput(ns("web"))
  
  
}
