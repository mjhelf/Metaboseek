function(request){
  
  MosaicMinimalUI(skin = "black",
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
                                    class = "dropdown")
                  ),
                  dashboardSidebar(
                    
                    sidebarMenu(
                      useShinyjs(),
                      
                      extendShinyjs(text = 'shinyjs.toggleFullScreen = function() {
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
                      tags$script('
                                  $(document).on("keydown", function (e) {
                                  Shiny.onInputChange("keyd", e.which);
                                  });
                                  $(document).on("keyup", function (e) {
                                  Shiny.onInputChange("keyd", "NO");
                                  });
                                  '),
                      
                      
                      
                      menuItem("Data Explorer", tabName = "exploredata", icon = icon("area-chart")),
                      menuItem("XCMS analysis", tabName = "XCMSrunpanel", icon = icon("file-text-o")),
                      
                      # menuItem("Workflows", tabName = "processdata", icon = icon("desktop"),
                      #          menuSubItem("Coming soon", tabName = "workflow1")),
                      menuItem("Update", tabName = "updateTab", icon = icon("upload")),
                      
                      #bookmarkButton(label ="Bookmark this session"),
                      htmlOutput("activeTable"),
                      hr(),
                      h5(a(paste0("MOSAiC version ",packageVersion("Mosaic")), 
                           href="https://github.com/mjhelf/Mosaic", target="_blank"), align = "center")
                      
                      )
                    
                    
                    
                  ),
                  dashboardBody(
                    # Also add some custom CSS to make the title background area the same
                    # color as the rest of the header.
                    tags$head(tags$style(HTML(
                      readChar(system.file("config", "Mosaic_styles.css", package = "Mosaic"),
                               file.info(system.file("config", "Mosaic_styles.css", package = "Mosaic"))$size)))),
                tabItems(
                  tabItem(tabName = "updateTab",
                          fluidPage(
                          fluidRow(
                            box(title = "Update mosaic!", status = "primary", collapsible = T, width = 12,
                                updaterModuleUI('update')
                            )
                          )
                          )
                  ),
                  
                  tabItem(tabName = "XCMSrunpanel",
                          xcmsModuleUI("xcmsMod")
                  ),
                  tabItem(tabName = "exploredata",
                          source(file.path("modules_nonformal", "exploreData_main_ui.R"), local = TRUE)$value
                  ),
                  tabItem(tabName = "workflow1",
                          p("In the making")
                  )
                )
                
                
                
              ) ,
                  diagnostics = .MosaicOptions$develMode,
                  dashboard = T)
  
  
}