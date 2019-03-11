#' MseekContainer
#' 
#' Module that contains the entire METABOseek program
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values Import data from the shiny session
#' 
#' @export 
MseekContainer <- function(input,output, session){
  
  ns <- NS(session$ns(NULL))
  
  MseekMinimalServer(diagnostics = .MseekOptions$develMode, data = .MseekOptions$loadExampleData, tables = .MseekOptions$loadExampleTable)
  
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
                        static = list(servermode = .MseekOptions$serverMode,
                                      activateXCMS = .MseekOptions$activateXCMS,
                                      rootpath = .MseekOptions$filePaths,
                                      filePattern = .MseekOptions$filePattern)
  )
  
  callModule(updaterModule, 'update', tag = ns('update'), set =list(package = "METABOseek",
                                                                refs = c("master", "devel", "devel_raw"),
                                                                active = !.MseekOptions$serverMode))
  
  
 observeEvent(StartPage$explore,{
   if(StartPage$explore){
        updateTabItems(session, "MseekSB", "exploredata")
   }
   
   
 })
 
 HeaderDataLoad <- callModule(LoadDataModule, "modaldataload",
                             values = reactiveValues(projectData = projectData,
                                                     featureTables = featureTables,
                                                     MSData = MSData,
                                                     GlobalOpts = GlobalOpts)
 )
 
 callModule(GlobalOptionsModule, "globaloptshe", values = reactiveValues(GlobalOpts = GlobalOpts))
 
    
 
 observeEvent(input$loadAll,{
   showModal(
     modalDialog(
       fluidPage(
         fluidRow(
           LoadDataModuleUI(ns("modaldataload"))
           
           )),
       title = "Load data",
       easyClose = T,
       fade = F,
       size = "l",
       footer = modalButton("Cancel") 
     ))
   
 })
 
 observeEvent(input$globaloptshead,{
   showModal(
     modalDialog(
       fluidPage(
         fluidRow(
           GlobalOptionsModuleUI(ns("globaloptshe"))
         )),
       title = "Change global settings",
       easyClose = T,
       fade = F,
       size = "l",
       footer = modalButton("Close") 
     ))
   
 })
  
}

#' MseekContainerUI
#' 
#' Module that contains the entire METABOseek program
#' 
#' @param id
#' 
#' @export
MseekContainerUI <- function(id){
  ns <- NS(id)
  
  MseekMinimalUI(skin = "black",
                  MseekHeader(id = id,
                              tags$li(actionLink(ns("globaloptshead"), "",
                                                 icon = icon("cog"), style="color:#ffffff;border-left-width:0;border-right:1px solid #eee",
                                                 title = "Global settings for METABOseek" ),
                                      class = "dropdown",
                                      style = "float:left")),
                  MseekSidebar(id = id),
                  dashboardBody(
                    
                    # Load custom CSS
                    tags$head(tags$style(HTML(
                      readChar(system.file("config", "METABOseek_styles.css", package = "METABOseek"),
                               file.info(system.file("config", "METABOseek_styles.css", package = "METABOseek"))$size)))),
                    
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
                  diagnostics = .MseekOptions$develMode,
                  dashboard = T,
                  id = id)
  
}