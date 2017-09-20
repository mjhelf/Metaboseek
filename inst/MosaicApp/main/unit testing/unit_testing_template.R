library(shiny)


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
      numericInput("tester","test",0),
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary")
      
 
    )
  


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  

  # Generate a text output ----
  output$summary <- renderPrint({
    print(input$tester)
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)