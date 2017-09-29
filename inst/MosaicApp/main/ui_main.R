dashboardPage(skin = "black",
              dashboardHeader(title = "MOSAiC"),
              dashboardSidebar(#width = 250,
                #imageOutput("logo"),  
                
                sidebarMenu(
                  p(paste0("version ",packageVersion("Mosaic"))),
                  source(file.path("modules_nonformal", "background_ui.R"), local = TRUE)$value,
                     
                  # menuItem("Load Data", tabName = "loaddata", icon = icon("files-o"),
                  #         menuSubItem("Feature Tables", tabName = "loadtables"),
                  #        menuSubItem("MS Data", tabName = "rawfiles")
                  #       ),
                  menuItem("Explore Data", tabName = "exploredata", icon = icon("area-chart")),
                  
                  menuItem("Workflows", tabName = "processdata", icon = icon("desktop"),
                           # menuSubItem("Feature Tables", tabName = "processTableData")
                  menuItem("Help", tabName = "start", icon = icon("paper-plane"))
                  ),
                  # menuSubItem("MS Data", tabName = "rawfiles")),
                  #  div(title = "Run basic statistical analyses", menuItem("Process Data", tabName = "processdata", icon = icon("desktop"))),
                  
                  
                  
                  bookmarkButton(label ="Bookmark this session"),
                  htmlOutput("activeTable")
                )
                
                
                
              ),
              dashboardBody(
                # Also add some custom CSS to make the title background area the same
                # color as the rest of the header.
                tags$head(tags$style(HTML('
                                          /* logo */
                                          .skin-black .main-header .logo {
                                          background-color: #C41230;
                                          color: #ffffff;
                                          }
                                          
                                          /* logo when hovered */
                                          .skin-black .main-header .logo:hover {
                                          background-color: #C41230;
                                          }
                                          
                                          /* navbar (rest of the header) */
                                          .skin-black .main-header .navbar {
                                          background-color: #C41230;
                                          color: #ffffff;
                                          }        
                                          
                                          /* main sidebar */
                                          .skin-black .main-sidebar {
                                          background-color: #595959;
                                          }
                                          
                                          /* active selected tab in the sidebarmenu */
                                          .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                          background-color: #404040;
                                          color: #ffffff;
                                          }
                                          
                                          /* other links in the sidebarmenu */
                                          .skin-black .main-sidebar .sidebar .sidebar-menu a{
                                          background-color: #595959;
                                          color: #ffffff;
                                          }
                                          
                                          /* other links in the sidebarmenu when hovered */
                                          .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                                          background-color: #404040;
                                          }
                                          /* toggle button  */                    
                                          .skin-black .main-header .navbar .sidebar-toggle{
                                          background-color: #C41230;
                                          color: #ffffff;
                                          }
                                          /* toggle button when hovered  */                    
                                          .skin-black .main-header .navbar .sidebar-toggle:hover{
                                          background-color: #595959;
                                          color: #ffffff;
                                          }
                                          
                                          
                                          .box .box-primary .box-header {
                                          color:#fff;
                                          background:#595959
                                          }
                                          
                                          .box .box-primary .box-primary{
                                          border-bottom-color:#666666;
                                          border-left-color:#666666;
                                          border-right-color:#666666;
                                          border-top-color:#666666;
                                          }
                                          '))),
            tabItems(
                tabItem(tabName = "start",
                        source(file.path("modules_nonformal", "start_ui.R"), local = TRUE)$value
                ),
                
               # tabItem(tabName = "loadtables",
                #        source(file.path("modules_nonformal", "loadtables_ui.R"), local = TRUE)$value
                 #       ),
              #  tabItem(tabName = "rawfiles",
               #         source(file.path("modules_nonformal", "loadMSdata_ui.R"), local = TRUE)$value
                #),
              #  tabItem(tabName = "processTableData",
               #         source(file.path("modules_nonformal", "processTableData_ui.R"), local = TRUE)$value
                #),
                tabItem(tabName = "exploredata",
                        source(file.path("modules_nonformal", "exploreData_main_ui.R"), local = TRUE)$value
                )
            )


    
        )
        )