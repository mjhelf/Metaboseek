
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)

MseekOptions()



ui <- MseekMinimalUI(
    dashboardPage(
        skin = "black",
        dashboardHeader(title = "PTModseek"),
        dashboardSidebar(  
            sidebarMenu(
                menuItem("Start", tabName = "PeptideAnnotation", icon = icon("home"))),
            disable =T),
        dashboardBody(
            # Load custom CSS
            tags$head(tags$style(HTML(
                readChar(system.file("config", "Metaboseek_styles.css", package = "Metaboseek"),
                         file.info(system.file("config", "Metaboseek_styles.css", package = "Metaboseek"))$size)))),
            tabItems(
                tabItem(tabName = "PeptideAnnotation",
                        box(width = 5,
                            title = "Load data",
                            LoadDataModuleUI("dataload")),
                        box(width = 7,
                            title = "Set modifications",
                            
                            PeptideModificationsModuleUI("pepmods")),
                        box(width = 12,
                            title = "Feature Table",
                            MainTableModuleUI("maintable")),
                        box(width = 12,
                            
                            PeptideAnnotationModuleUI("pepmodule"))
                        
                ) 
            )
        )
    ),
    diagnostics = F)

server <- function(input, output, session) {
    
    
    
    MseekMinimalServer(diagnostics = F, data = F, tables = F)
    
    
    HeaderDataLoad <- callModule(LoadDataModule, "dataload",
                                 values = values)
    
    
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
    
    observeEvent(FeatureTable(values)$df, {
        
        
    }, once = T)
    
    
    callModule(PeptideModificationsModule, "pepmods", values)
    
    callModule(PeptideAnnotationModule, "pepmodule", values)
    
    
    callModule(MainTableModule, "maintable", values)
    
    
    
    observe({values$featureTables$selectedCols <- colnames(values$featureTables$tables[[values$featureTables$active]]$df)})
    
}

# Create Shiny app ----
shinyApp(ui, server)