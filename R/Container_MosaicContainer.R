#' MosaicContainer
#' 
#' Module that contains the entire Mosaic program
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values Import data from the shiny session
#' 
#' @export 
MosaicContainer <- function(input,output, session){
  
  ns <- NS(session$ns(NULL))
  
  MosaicMinimalServer(diagnostics = .MosaicOptions$develMode, data = .MosaicOptions$loadExampleData, tables = .MosaicOptions$loadExampleTable)
  
  StartPage <- callModule(WelcomePageModule,"startpage",
                          values = reactiveValues(projectData = projectData,
                                                  featureTables = featureTables,
                                                  MSData = MSData,
                                                  GlobalOpts = GlobalOpts),
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
  
  callModule(updaterModule, 'update', tag = ns('update'), set =list(package = "Mosaic",
                                                                refs = c("master", "devel", "devel_raw"),
                                                                active = !.MosaicOptions$serverMode))
  
  
 observeEvent(StartPage$explore,{
   if(StartPage$explore){
        updateTabItems(session, "MosaicSB", "exploredata")
   }
   
   
 })
    
  
}

#' MosaicContainerUI
#' 
#' Module that contains the entire Mosaic program
#' 
#' @param id
#' 
#' @export
MosaicContainerUI <- function(id){
  ns <- NS(id)
  
  MosaicMinimalUI(skin = "black",
                  MosaicHeader(),
                  MosaicSidebar(id = id),
                  dashboardBody(
                    
                    # Load custom CSS
                    tags$head(tags$style(HTML(
                      readChar(system.file("config", "Mosaic_styles.css", package = "Mosaic"),
                               file.info(system.file("config", "Mosaic_styles.css", package = "Mosaic"))$size)))),
                    
                    tabItems(
                      tabItem(tabName = "start",
                              WelcomePageModuleUI(ns("startpage"))
                              ),
                      tabItem(tabName = "updateTab",
                              updaterModuleUI(ns('update'))
                      ),
                      
                      tabItem(tabName = "XCMSrunpanel",
                              xcmsModuleUI(ns("xcmsMod"))
                      ),
                      tabItem(tabName = "exploredata",
                              MainPageContainerUI(ns("mainpagecontent"))
                      ),
                      tabItem(tabName = "workflow1",
                              p("In the making")
                      )
                    )
                    
                    
                    
                  ) ,
                  diagnostics = .MosaicOptions$develMode,
                  dashboard = T,
                  id = id)
  
}