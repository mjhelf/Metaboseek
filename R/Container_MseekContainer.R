#' MseekContainer
#' 
#' Module that contains the entire Metaboseek program
#' 
#' @return The server module for this container returns nothing
#' 
#' @inherit MseekContainers
#' @describeIn MseekContainer server logic module, to be called with \link[shiny]{callModule}()
#' 
#' @export 
MseekContainer <- function(input,output, session, values){
  
  ns <- NS(session$ns(NULL))
  
  MseekMinimalServer(diagnostics = .MseekOptions$develMode, ## NOTE: server part of develMode now handled in server.R because of scoping issues with MseekContainer #.MseekOptions$develMode,
                     
                     data = .MseekOptions$loadExampleData,
                     tables = .MseekOptions$loadExampleTable)
  
  
  
  StartPage <- callModule(WelcomePageModule,"startpage",
                          values = values,
             show = reactive({T}))
  
  callModule(MainPageContainer, "mainpagecontent", values)
  
  xcmsOut <- callModule(xcmsWidget, "xcmsMod",
                        externalFilegroups = reactive({values$MSData$layouts[[values$MSData$active]]$rawgrouptable}),
                        static = list(servermode = .MseekOptions$serverMode,
                                      activateXCMS = .MseekOptions$activateXCMS,
                                      rootpath = .MseekOptions$filePaths,
                                      filePattern = .MseekOptions$filePattern)
  )
  
  callModule(updaterModule, 'update', tag = ns('update'), set =list(package = "Metaboseek",
                                                                refs = c("master", "devel", "devel_raw"),
                                                                active = FALSE))#!.MseekOptions$serverMode))
  
  
 observeEvent(StartPage$explore,{
   if(StartPage$explore){
        updateTabItems(session, "MseekSB", "exploredata")
   }
   
   
 })
 
 HeaderDataLoad <- callModule(LoadDataModule, "modaldataload",
                             values = values
 )
 
 callModule(GlobalOptionsModule, "globaloptshe", values = values)#reactiveValues(GlobalOpts = GlobalOpts) )
 
    
 
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

#' @describeIn MseekContainer returns the \code{shiny} UI elements for the entire Metaboseek program
#' 
#' @export
MseekContainerUI <- function(id){
  ns <- NS(id)
  
  MseekMinimalUI(skin = "black",
                  MseekHeader(id = id,
                              tags$li(actionLink(ns("globaloptshead"), "",
                                                 icon = icon("cog"), style="color:#ffffff;border-left-width:0;border-right:1px solid #eee",
                                                 title = "Global settings for Metaboseek" ),
                                      class = "dropdown",
                                      style = "float:left")),
                  MseekSidebar(id = id),
                  dashboardBody(
                    
                    # Load custom CSS
                    tags$head(tags$style(HTML(
                      readChar(system.file("config", "Metaboseek_styles.css", package = "Metaboseek"),
                               file.info(system.file("config", "Metaboseek_styles.css", package = "Metaboseek"))$size)))),
                    
                    tabItems(
                      tabItem(tabName = "start",
                              WelcomePageModuleUI(ns("startpage"))
                              ),
                      tabItem(tabName = "updateTab",
                              updaterModuleUI(ns('update'))
                      ),
                      
                      tabItem(tabName = "XCMSrunpanel",
                              xcmsWidgetUI(ns("xcmsMod"))
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