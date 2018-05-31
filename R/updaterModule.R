#' updaterModuleUI
#' 
#' 
#' module to update Mosaic by the click of a button
#' 
#' @param id id to be used in ns()
#' 
#' @export 
updaterModuleUI <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns('updateUI'))
  
}


#' updaterModule
#' 
#' 
#' module to update Mosaic by the click of a button
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param tag id to be used in ns()
#' @param set Import data from the shiny session
#' 
#' @export 
updaterModule <- function(input,output, session, tag, set = list(package = "Mosaic",
                                                                       refs = c("master", "devel", "devel_raw"),
                                                                       active = T
)){
  ns <- NS(tag)
  
  output$updateUI <- renderUI({
    if(set$active){
      fluidPage(
        fluidRow(
         
          p(strong("master:"),'most stable version, updated least frequently.'),
          p(strong("devel:"),'latest stable version (with new, untested features)'),
          p(strong("devel_raw:"),'potentially unstable version (with new, untested features), not recommended! Updated most frequently.')
        ),
        fluidRow(
          column(2,
                 selectizeInput(ns('branch'), "Select version", choices = set$refs)),
          column(3,
                 p(strong("WARNING: This will shut down the current MOSAiC session and unsaved work will be lost!")),
                 actionButton(ns('updatePackage'), "Update!"))
        ))
    }
  })
  
  toggleState(ns('updatePackage'), condition = set$active)
  
  updateVals <- reactiveValues(report = NULL)
  
  output$updateReport <- renderPrint({
    if(!is.null(input$showDetails) && input$showDetails){
      print(updateVals$report)
      #for(k in updateVals$report){
      # cat(k)}
      #print(paste(updateVals$report, collapse = "\n"))
      
    }
  })
  
  # upModal <- function() {
  #   ns <- session$ns
  #   modalDialog(actionButton(ns("closeModalBtn"), "Close Modal"))
  # }
  
  
  observeEvent(input$updatePackage,{
    
    # withProgress(message = "Please wait!",
    #              detail = "Updating Mosaic...",
    #              value = 0.5, {
                   
                   runner <- system.file("MosaicApp", "main","scripts", "update_script.R",package = "Mosaic")
                   rpath <- file.path(R.home(component = "bin"), "Rscript")
                   
                   
        updateVals$report <-  system(paste0(
                                 '"',
                                 rpath,
                                 '" ',
                                 '"',
                                 runner,
                                 '" ',
                                 input$branch
                                 ),
                          intern = T, wait = F)
        
        q()
                   
                   # #scriptPath <- paste0('Rscript "C:/Users/mjh43/OneDrive - Cornell University/R scripts new/devel_update_script.R" ', input$branch)
                   # 
                   # #updateVals$report <- system(scriptPath, intern = T, wait = T)
                   # 
                   # mes <- "See console output for details!"
                   # 
                   # if(length(grep("Installation failed", updateVals$report))>0){
                   #   mes <- p(strong("Installation failed!"), "Dependencies may have changed.
                   #            View details for information on missing packages or download latest version of pre-packaged MOSAiC from",
                   #            a("http://mosaic.bti.cornell.edu", href="http://mosaic.bti.cornell.edu", target="_blank"))
                   # } 
                   # if(length(grep("DONE (Mosaic)", updateVals$report, fixed = T))>0){
                   #   mes <- p(strong("Mosaic updated successfully!"), "Close and restart Mosaic to use the new version.")
                   # }
                   # 
                   # if(length(grep("Skipping install of 'Mosaic'", updateVals$report))>0){
                   #   mes <- p(strong("Mosaic was already up-to-date!"))
                   # }
                   
                 })
    
    # showModal(
    #   #ns <- session$ns,
    #   
    #   modalDialog(
    #     
    #     
    #     mes,
    #     checkboxInput(ns('showDetails'), 'Show details', value = FALSE),
    #     verbatimTextOutput(ns('updateReport')),
    #     title = "Update status",
    #     easyClose = T
    #     
    #   ))
    
                 # })
  
  }