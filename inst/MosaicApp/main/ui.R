function(request){
  
  MosaicMinimalUI(skin = "black",
                  MosaicHeader(),
                  MosaicSidebar(),
                  dashboardBody(
                    
                    # Load custom CSS
                    tags$head(tags$style(HTML(
                      readChar(system.file("config", "Mosaic_styles.css", package = "Mosaic"),
                               file.info(system.file("config", "Mosaic_styles.css", package = "Mosaic"))$size)))),
                    tabItems(
                      tabItem(tabName = "updateTab",
                              updaterModuleUI('update')
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