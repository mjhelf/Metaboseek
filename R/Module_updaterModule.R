#' updaterModule
#' 
#' 
#' module to update Mseek by the click of a button
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param tag id to be used in ns()
#' @param set Import data from the shiny session
#' 
#' @export 
updaterModule <- function(input,output, session, tag, set = list(package = "METABOseek",
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
                 actionButton(ns('updatePackage'), "Update!"))
        ))
    }
  })
  
  toggleState(ns('updatePackage'), condition = set$active)
  
   observeEvent(input$updatePackage,{
    
    showModal(
        modalDialog(
          p(strong("WARNING: This will shut down the current METABOseek session and unsaved work will be lost!")),
          p("Mseek will shut down, and a terminal window will appear with information on the update progress."),
          p("The terminal Window may not show up on Linux or macOS systems. In that case, check after a few minutes if the update has worked."),
          
          actionButton(ns('startUpdate'), 'Start Update Now!'),
          title = paste0('Update to latest "',input$branch,'" branch of Mseek?'),
          easyClose = T,
          footer = modalButton("Cancel")

        ))
  })
  
  
  observeEvent(input$startUpdate,{
    
                   runner <- system.file("scripts", "update_script.R",package = "METABOseek")
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


#' updaterModuleUI
#' 
#' 
#' module to update Mseek by the click of a button
#' 
#' @param id id to be used in ns()
#' 
#' @export 
updaterModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      shinydashboard::box(title = "Update Mseek!", status = "primary", collapsible = F, width = 12,
                          htmlOutput(ns('updateUI'))
      )
    )
  )
}