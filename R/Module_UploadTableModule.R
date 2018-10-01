#' UploadTableModule
#' 
#' 
#' server module for loading Tables
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param static Import data from the shiny session
#' 
#' @export 
UploadTableModule <- function(input,output, session,
                              static = list(title =  NULL,
                                            filetypes = NULL,
                                            format = list(header = T,
                                                          sep = "\t",
                                                          quote = '"',
                                                          stringsAsFactors = F)
                              )
){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(df = NULL,
                                   filename = NULL
  )
  
  #When Load Table button is pressed, initialize internalValues with defaults
  observeEvent(input$file1$datapath,
               {
                 tryCatch({
                   internalValues$df <- read.delim(input$file1$datapath,
                                                   header = if(is.null(static$format$header)){input$header}else{static$format$header},
                                                   sep = if(is.null(static$format$sep)){input$sep}else{static$format$sep}, 
                                                   quote = if(is.null(static$format$quote)){input$quote}else{static$format$quote},
                                                   stringsAsFactors = if(is.null(static$format$stringsAsFactors)){F}else{static$format$stringsAsFactors})
                   
                   internalValues$filename <- input$file1$name #gsub("\\.[^.]*$","",input$file1$name)
                 },
                 error = function(e){
                   showNotification(paste("ERROR: Make sure you selected the correct Table Options (tab or comma separated?) for your input table."), type = "error", duration = 10)
                   
                 })
                 
                 
                 
               })  
  
  # observe({
  #   
  #   anytoggle <- any(c(is.null(static$format$header), is.null(static$format$sep), is.null(static$format$quote)  ))
  #   
  #   
  #   toggleOpt <- (anytoggle && !is.null(input$toggleTabOpts) && input$toggleTabOpts)
  #   
  #   toggle(id = 'toggleTabOpts', condition = anytoggle)
  #   
  #   toggle(id = 'header', condition = (is.null(static$format$header) &&  toggleOpt))
  #   toggle(id = 'sep', condition = (is.null(static$format$sep) && toggleOpt))
  #   toggle(id = 'quote', condition = (is.null(static$format$quote) && toggleOpt))
  # })
  
  output$tableLoad <- renderUI({
    
    fluidRow(
      # checkboxInput(ns('toggleTabOpts'), "Show table Options", value = F),
      
      if(is.null(static$format$header)){
        checkboxInput(ns('header'), 'Header', TRUE)},
      
      if(is.null(static$format$sep)){
        radioButtons(ns('sep'), 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ',')},
      
      if(is.null(static$format$quote)){
        radioButtons(ns('quote'), 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')},
      
      
      fileInput(ns('file1'), if(is.null(static$title)){'Choose File'}
                else{static$title},
                accept=static$filetypes)
      
    )
    
  })
  
  output$tableInfo <- renderUI({
    fluidRow(
      if(!is.null(internalValues$df)){
        p("Loaded a table with", nrow(internalValues$df), "rows and", ncol(internalValues$df), "columns.")
      }else{NULL}
    )
  })
  
  return(internalValues)
  
}

#' UploadTableModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
UploadTableModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns('tableLoad')),
    htmlOutput(ns('tableInfo'))
  )
}