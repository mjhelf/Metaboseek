
runcodeUIns <- function (code = "", type = c("text", "textarea", 
                              "ace"), width = NULL, height = NULL, includeShinyjs = FALSE, id = NULL) 
{
    ns <- NS(id)
    type <- match.arg(type)
    if (type == "ace") {
        if (!requireNamespace("shinyAce", quietly = TRUE)) {
            shinyjs:::errMsg("You need to install the 'shinyAce' package in order to use 'shinyAce' editor.")
        }
    }
    placeholder <- "Enter R code"
    shiny::singleton(shiny::tagList(if (includeShinyjs) 
        useShinyjs(), if (type == "text") 
            shiny::textInput(ns("runcode_expr"), label = NULL, 
                             value = code, width = width, placeholder = placeholder), 
        if (type == "textarea") 
            shiny::textAreaInput(ns("runcode_expr"), label = NULL, 
                                 value = code, width = width, height = height, 
                                 placeholder = placeholder), if (type == "ace") 
                                     shinyAce::aceEditor(ns("runcode_expr"), mode = "r", 
                                                         value = code, height = height, theme = "github", 
                                                         fontSize = 16), shiny::actionButton(ns("runcode_run"), 
                                                                                             "Run", class = "btn-success"), shinyjs::hidden(shiny::div(id = ns("runcode_error"), 
                                                                                                                                                       style = "color: red; font-weight: bold;", shiny::div("Oops, that resulted in an error! Try again."), 
                                                                                                                                                       shiny::div("Error: ", shiny::br(), shiny::tags$i(shiny::span(id = ns("runcode_errorMsg"), 
                                                                                                                                                                                                                    style = "margin-left: 10px;")))))))
}

# Define UI for dataset viewer app ----
#' MseekMinimalUI
#'
#' A minimal UI for Mseek that can be extended with additional objects for 
#' testing and development purposes
#'
#' @return UI elements for Metaboseek
#' 
#' @param ... UI elements to embed in the Metaboseek app
#' @param diagnostics if TRUE, will render diagnostics UI elements
#' @param dashboard if TRUE, will render a dashboard layout
#' @param id id for namespacing - must be the id of the enclosing module or NULL
#' 
#' @importFrom shinyjs runcodeUI useShinyjs
#' @importFrom shinydashboard dashboardPage 
#' @importFrom shiny fluidPage verbatimTextOutput
#' 
#' @export
MseekMinimalUI <- function(..., diagnostics = T, dashboard = F, id = NULL){
  
  #if(!is.null(id)){
    ns <- NS(id)
    keyid <- ns("keyd")
 # }else{
  #  keyid <- "keyd"
  #}
  
  if(!dashboard){
      tagList(
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
          runcodeUIns(code = "", type = c("ace"), width = NULL,
                    height = 500, includeShinyjs = FALSE, id = id),
          verbatimTextOutput('diag'))}
      else{
        tagList(...) 
      }
    )}
  else{
    if(diagnostics){
      fluidPage(
        dashboardPage(...),
        runcodeUIns(code = "", type = c("ace"), width = NULL,
                  height = 500, includeShinyjs = FALSE, id = id),
        verbatimTextOutput('diag')
      )
    }
    else{
      dashboardPage(...) 
    }
  }
  
}


#' dashboardHeaderM
#'
#' Metaboseek-specific, modified version of \code{\link[shinydashboard]{dashboardHeader}()} 
#' that allows placing icons left and right in the navbar
#' 
#' @param ... additional UI elements for the header
#' @param title App title
#' @param titleWidth custom width of title
#' @param disable if TRUE, header will not be shown
#' @param .list additional uI elements for the header, passed as a list
#' @param left list of UI elements for the left side of the header
#'
dashboardHeaderM <- function (..., title = NULL,
                              titleWidth = NULL,
                              disable = FALSE, 
                              .list = NULL, left = NULL) 
{
  items <- c(list(...), .list, left)
  lapply(items, shinydashboard:::tagAssert, type = "li", class = "dropdown")
  titleWidth <- shiny::validateCssUnit(titleWidth)
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    custom_css <- tags$head(tags$style(HTML(gsub("_WIDTH_", 
                                                 titleWidth, fixed = TRUE, "\n      @media (min-width: 768px) {\n        .main-header > .navbar {\n          margin-left: _WIDTH_;\n        }\n        .main-header .logo {\n          width: _WIDTH_;\n        }\n      }\n    "))))
  }
  tags$header(class = "main-header", custom_css, style = if (disable) "display: none;",
              span(class = "logo", title), 
              tags$nav(class = "navbar navbar-static-top", 
                       role = "navigation",
                       span(shiny::icon("bars"), style = "display:none;"), 
                       a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas", 
                         role = "button", span(class = "sr-only", "Toggle navigation")), 
                       div(class = "navbar-custom-menu", tags$ul(class = "nav navbar-nav", 
                                                                 left),
                           style = "float:left"),
                       div(class = "navbar-custom-menu", tags$ul(class = "nav navbar-nav", 
                                                                 c(list(...), .list)))
              ))
}


#' @describeIn MseekMinimalUI generates the dashboardHeader for Mseek.
#' @export
MseekHeader <- function(..., id = NULL){
  
  #if(!is.null(id)){
    ns <- NS(id)
 # }else{
#    ns <- function(id){return(id)}
 # }
  
  dashboardHeaderM(title = "Metaboseek",
                   #style = "width:95%",
                   # dropdownMenu(messageItem("Tip of the day", "Press F11 to enter/exit full screen mode.",
                   #                          icon = shiny::icon("fullscreen", lib = "glyphicon"),
                   #                          href = NULL),
                   #              type = c("messages"),
                   #              badgeStatus = "primary", icon = NULL, headerText = NULL, .list = NULL),
                   left = list(
                     tags$li(actionLink(ns("loadAll"), "",
                                        icon = icon("folder-open"), style="color:#ffffff;border-left-width:0;border-right:1px solid #eee",
                                        title = "Load Projects, MS data or feature tables into Metaboseek" ),
                             class = "dropdown",
                             style = "float:left"),
                     ...),
                   
                   
                   tags$li(a(
                     icon("fullscreen", lib = "glyphicon"),
                     onclick = "shinyjs.toggleFullScreen();",
                     style="color:#ffffff",
                     title = "Activate/deactivate full-screen mode"),
                     class = "dropdown"),
                   tags$li(a(
                     href = 'http://metaboseek.com/doc',
                     icon("question-circle fa-lg"),
                     title = "Mseek online help (opens in new window)",
                     target="_blank",
                     style="color:#ffffff"),
                     class = "dropdown")
  )
}


#' @describeIn MseekMinimalUI generates the dashboardSidebar for Mseek.
#' @export
MseekSidebar <- function(..., id = NULL){
 # if(!is.null(id)){
    ns <- NS(id)
    keyid <- ns("keyd")
    SBid <- ns("MseekSB")
  # }else{
  #   keyid <- "keyd"
  #   SBid <- "MseekSB"
  # }
  
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
      #menuItem("Update", tabName = "updateTab", icon = icon("upload")),
      ...,
      #bookmarkButton(label ="Bookmark this session"),
      #SelectActiveTableModuleUI("selectactivetable"),
      hr(),
      h5(a(paste0("Metaboseek version ",packageVersion("Metaboseek")), 
           href="https://github.com/mjhelf/Metaboseek", target="_blank"), align = "center")
      
    )
    
    
    
  )
}



#' mActionButton
#'
#' An actionButton that can optionally be colored in Cornell red
#' 
#' @return a \code{\link[shiny]{actionButton}} that can be turned red easily
#' 
#' @param ... passed to shiny::actionButton
#' @param red if True, button will be red
#'
mActionButton <- function(..., red = FALSE){
  
  shiny::actionButton(..., style = if(red){"color: #fff; background-color: #C41230; border-color: #595959"}else{""})
  
}


#' mDownloadButton
#'
#' A \code{\link[shiny]{downloadButton}} with the option to set the icon
#' 
#' @return a \code{\link[shiny]{actionButton}} that can be turned red easily
#' 
#' @param outputId The name of the output slot that the \code{downloadHandler} is assigned to.
#' @param label The label that should appear on the button.
#' @param class Additional CSS classes to apply to the tag, if any.
#' @param icon icon to use on this button (an object generated by \code{\link[shiny]{icon}()})
#' @param ... Other arguments to pass to the container tag function.
#'
mDownloadButton <- function (outputId,
                             label = "Download",
                             class = NULL,
                             icon = icon("download"),
                             ...) 
{
    aTag <- tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", 
                                                class), href = "", target = "_blank", download = NA, 
                   icon, label, ...)
}