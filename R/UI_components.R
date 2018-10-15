# Define UI for dataset viewer app ----
#' MosaicMinimalUI
#'
#' A minimal UI for Mosaic that can be extended with additional objects for testing and development purposes
#'
#' @importFrom shinyjs runcodeUI 
#' @importFrom shinydashboard dashboardPage 
#' @importFrom shiny fluidPage verbatimTextOutput
#' 
#' @export
MosaicMinimalUI <- function(..., diagnostics = T, dashboard = F, id = NULL){
  
  if(!is.null(id)){
    ns <- NS(id)
    keyid <- ns("keyd")
  }else{
    keyid <- "keyd"
  }
  
  if(!dashboard){
    fluidPage(
      tags$script(paste0('
                $(document).on("keydown", function (e) {
                Shiny.onInputChange("',keyid,'", e.which);
                });
                $(document).on("keyup", function (e) {
                Shiny.onInputChange("',keyid,'", "NO");
                });
                ')),
      if(diagnostics){
        fluidPage(
          ...,
          runcodeUI(code = "", type = c("text", "textarea", "ace"), width = NULL,
                    height = NULL, includeShinyjs = FALSE),
          verbatimTextOutput('diag'))}
      else{
        ... 
      }
    )}
  else{
    if(diagnostics){
      fluidPage(
        dashboardPage(...),
        runcodeUI(code = "", type = c("text", "textarea", "ace"), width = NULL,
                  height = NULL, includeShinyjs = FALSE),
        verbatimTextOutput('diag')
      )
    }
    else{
      dashboardPage(...) 
    }
  }
  
}

#' MosaicHeader
#'
#' generates the dashboardHeader for Mosaic.
#' 
#' @export
MosaicHeader <- function(...){
dashboardHeader(title = "MOSAiC",
                # dropdownMenu(messageItem("Tip of the day", "Press F11 to enter/exit full screen mode.",
                #                          icon = shiny::icon("fullscreen", lib = "glyphicon"), 
                #                          href = NULL),
                #              type = c("messages"),
                #              badgeStatus = "primary", icon = NULL, headerText = NULL, .list = NULL),
                tags$li(a(
                  icon("fullscreen", lib = "glyphicon"),
                  onclick = "shinyjs.toggleFullScreen();",
                  style="color:#ffffff",
                  title = "Activate/deactivate full-screen mode"),
                  class = "dropdown"),
                tags$li(a(
                  href = 'http://mosaic.bti.cornell.edu/welcome/doc.html',
                  icon("question-circle fa-lg"),
                  title = "Mosaic online help (opens in new window)",
                  target="_blank",
                  style="color:#ffffff"),
                  class = "dropdown"),
                ...
)
}


#' MosaicSidebar
#'
#' generates the dashboardSidebar for Mosaic.
#' 
#' @export
MosaicSidebar <- function(..., id = NULL){
  if(!is.null(id)){
    ns <- NS(id)
    keyid <- ns("keyd")
    SBid <- ns("MosaicSB")
  }else{
    keyid <- "keyd"
    SBid <- "MosaicSB"
  }
  
  dashboardSidebar(
 

  
  sidebarMenu(
    id = SBid,
    useShinyjs(),
    
    shinyjs::extendShinyjs(text = 'shinyjs.toggleFullScreen = function() {
    var element = document.documentElement,
      enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
      exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
    if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
      enterFS.call(element);
    } else {
      exitFS.call(document);
    }
  }', functions = "toggleFullScreen"),
    
    
    ##DETECT KEYBOARD ACTIONS
    ##key being held down
    tags$script(paste0('
                $(document).on("keydown", function (e) {
                       Shiny.onInputChange("',keyid,'", e.which);
});
                       $(document).on("keyup", function (e) {
                       Shiny.onInputChange("',keyid,'", "NO");
                       });
                       ')),
    
    
    menuItem("Start", tabName = "start", icon = icon("home")),
    menuItem("Data Explorer", tabName = "exploredata", icon = icon("area-chart")),
    menuItem("XCMS analysis", tabName = "XCMSrunpanel", icon = icon("file-text-o")),
    
    # menuItem("Workflows", tabName = "processdata", icon = icon("desktop"),
    #          menuSubItem("Coming soon", tabName = "workflow1")),
    menuItem("Update", tabName = "updateTab", icon = icon("upload")),
    ...,
    #bookmarkButton(label ="Bookmark this session"),
    #SelectActiveTableModuleUI("selectactivetable"),
    hr(),
    h5(a(paste0("MOSAiC version ",packageVersion("Mosaic")), 
         href="https://github.com/mjhelf/Mosaic", target="_blank"), align = "center")
    
  )
  
  
  
)
}



#' mActionButton
#'
#' An actionButton that can optionally be colored in Cornell red
#' 
#' @param ... passed to shiny::actionButton
#' @param red if True, button will be red
#'
mActionButton <- function(..., red = F){
  
  shiny::actionButton(..., style = if(red){"color: #fff; background-color: #C41230; border-color: #595959"}else{""})
  
}