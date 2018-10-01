#' FilePathModule
#' 
#' 
#' module to find filepaths for select directory operations, can display just a button or a button with a displayof the filepath.
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param filepaths Named character vector of parent folder locations
#' @param label button label
#' @param description button description on hover
#' @param displayFolder show filepath (boolean)
#' 
#' 
#' @import shiny shinyFiles
#' 
FilePathModule <- function(input, output, session,
                           filepaths = reactive({.MosaicOptions$filePaths}),
                           label = "Select Folder", description= "Select Folder",
                           displayFolder = F
                           ){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(dir = NULL)
  
  observe({
  shinyDirChoose(input, 'folder', session = session, roots=filepaths())
  })
  
  observeEvent(input$folder,{
               
    internalValues$dir <-  parseDirPath(roots=filepaths(), input$folder)
  })
  
  output$folderButton <- renderUI({
     div(id = ns("div"), title = description,
        shinyDirButton(ns('folder'), label, title = description)
    )
    
  })
  
  output$msfoldertag <- renderUI({
    
    
    HTML('<div class="FolderBox">',
         ifelse(length(internalValues$dir) == 0 || is.na(internalValues$dir), "No Folder selected", internalValues$dir),
         '
 
   </div> ')
    })
  
  
  output$folderUI <- renderUI({
    
     if(displayFolder){
       #fluidPage(
       tagList(
       HTML('<div class="ContextBox">'),
   fluidPage(
      fluidRow(
        
        htmlOutput(ns("folderButton")),
      fluidRow(
        fluidPage(
        htmlOutput(ns("msfoldertag"))
        )
      )
      )
      )
       
    ,
      HTML('</div>')
    )
     }else{fluidPage(
       htmlOutput(ns("folderButton"))
     )}
    
  })
  
  return(internalValues)
}
  
#' FilePathModuleUI
#' 
#' 
#' @param id id of the shiny module
#' 
FilePathModuleUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML('.FolderBox {
 word-wrap: break-word;
background-color: azure;
padding: 5px;

} 
                            .ContextBox {
background-color: gainsboro;
padding: 5px;

} 
                            '))),
    
    htmlOutput(ns("folderUI"))
  )
}



