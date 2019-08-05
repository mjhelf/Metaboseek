#' updaterModule
#' 
#' module to update Metaboseek from inside the UI. Currently not working.
#' 
#' @inherit MseekModules
#' @describeIn updaterModule Server logic
#' 
#' @return Returns nothing
#' @param tag id to be used in ns()
#' @param set set which package to update and which branches to allow to be selected
#' 
#' @export 
updaterModule <- function(input,output, session,
                          tag, set = list(package = "Metaboseek",
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
        if(Sys.info()['sysname'] != "Windows"){
          fluidRow(
            p("The updater is currently only available for Windows system. enter the following line into your R terminal after closing all running R sessions:"),
            p("devtools::install_github('mjhelf/Metaboseek', ref = 'master')"),
            p("You can enter 'devel' or 'devel_raw' instead of 'master', but 'master' is strongly recommended."),
            p("If you encounter problems with the installation, try to  enter the following line into your R terminal after closing all running R sessions:"),
            p("source('http://metaboseek.com/files/install_Metaboseek.R')")
            
          )}else{
            fluidRow(
              column(2,
                     selectizeInput(ns('branch'), "Select version", choices = set$refs)),
              column(3,
                     actionButton(ns('updatePackage'), "Update!"))
            )}
      )
    }
  })
  
  toggleState(ns('updatePackage'), condition = set$active)
  
  observeEvent(input$updatePackage,{
    
    showModal(
      modalDialog(
        p(strong("WARNING: This will shut down the current Metaboseek session and unsaved work will be lost!")),
        p("Metaboseek will shut down, and a terminal window will appear with information on the update progress."),
        p("The terminal Window may not show up on Linux or macOS systems. In that case, check after a few minutes if the update has worked."),
        
        actionButton(ns('startUpdate'), 'Start Update Now!'),
        title = paste0('Update to latest "',input$branch,'" branch of Metaboseek?'),
        easyClose = T,
        footer = modalButton("Cancel")
        
      ))
  })
  
  
  observeEvent(input$startUpdate,{
    
    runner <- system.file("scripts", "update_script.R",package = "Metaboseek")
    rpath <- file.path(R.home(component = "bin"), "Rscript")
    
    
    
    tryCatch({
      system(paste0(
        "xterm -e ",
        '"',
        rpath,
        '"  --verbose ',
        '"',
        runner,
        '" ',
        input$branch
      ),
      intern = F, wait = F, invisible = FALSE)
    },
    warning = function(w){
      system(paste0(
        '"',
        rpath,
        '"  --verbose ',
        '"',
        runner,
        '" ',
        input$branch
      ),
      intern = F, wait = F, invisible = FALSE)
    })
    
    
    
    q(save = "no")
    
  })
  
  
  
}


#' @describeIn updaterModule UI elements
#' @export 
updaterModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      shinydashboard::box(title = "Update Metaboseek!",
                          status = "primary", collapsible = F, width = 12,
                          htmlOutput(ns('updateUI'))
      )
    )
  )
}