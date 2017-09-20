library(shiny)

source("./modules/tableModule.R")
df1 <- data.frame(Parameter=c("a","b","c"), Value = c(1,2,3))

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
tableModuleUI("soso"),
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary")
      
 
    )
  


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  
   internalValues <- reactiveValues(dframe = df1)
 
  callModule(tableModule, "soso",df = reactive({df1}))
  # Generate a text output ----
  output$summary <- renderPrint({
    print(df1)
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)