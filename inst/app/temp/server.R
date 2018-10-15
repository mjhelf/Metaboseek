function(input, output, session) {
  MosaicMinimalServer(diagnostics = .MosaicOptions$develMode, data = .MosaicOptions$loadExampleData, tables = .MosaicOptions$loadExampleTable)
  
  output$allUI <- renderUI({
    
    tagList(
                    MosaicHeader(),
                    MosaicSidebar(id = NULL, explore = StartPage$explore),
                    dashboardBody(
                      
                      # Load custom CSS
                      tags$head(tags$style(HTML(
                        readChar(system.file("config", "Mosaic_styles.css", package = "Mosaic"),
                                 file.info(system.file("config", "Mosaic_styles.css", package = "Mosaic"))$size)))),
                      
                      tabItems(
                        tabItem(tabName = "start",
                                WelcomePageModuleUI("startpage")
                        ),
                        tabItem(tabName = "updateTab",
                                updaterModuleUI('update')
                        ),
                        
                        tabItem(tabName = "XCMSrunpanel",
                                xcmsModuleUI("xcmsMod")
                        ),
                        tabItem(tabName = "exploredata",
                                MainPageContainerUI("mainpagecontent")
                        ),
                        tabItem(tabName = "workflow1",
                                p("In the making")
                        )
                      )
                      
                      
                      
                    )
    )
  })
  StartPage <- callModule(WelcomePageModule,"startpage",
                          show = reactive({T}))
  
  MainPageContent <- callModule(MainPageContainer, "mainpagecontent",
                                values = reactiveValues(featureTables = featureTables,
                                                        MSData = MSData,
                                                        GlobalOpts = GlobalOpts,
                                                        projectData = projectData),
                                keys = reactive({keyin$keyd})
  )
  
  xcmsOut <- callModule(xcmsModule, "xcmsMod",
                        values = list(MSData = MSData),
                        static = list(servermode = .MosaicOptions$serverMode,
                                      activateXCMS = .MosaicOptions$activateXCMS,
                                      rootpath = .MosaicOptions$filePaths,
                                      filePattern = .MosaicOptions$filePattern)
  )
  
  callModule(updaterModule, 'update', tag = 'update', set =list(package = "Mosaic",
                                                                    refs = c("master", "devel", "devel_raw"),
                                                                    active = !.MosaicOptions$serverMode))
  
  
  
  
  }