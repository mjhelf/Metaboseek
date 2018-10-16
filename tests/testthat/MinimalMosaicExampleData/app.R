library(Mosaic)
library(shiny)
library(shinyjs)

#MosaicExamplePreload()

ui <- MosaicMinimalUI(diagnostics = T)

server <- function(input, output) {
  MosaicMinimalServer(diagnostics = T, data = T, tables = T)
  
  output$diag <- renderPrint({
    print(featureTables$tables$table0)
    print(featureTables$tables$table1)
  })
  
  }

# Create Shiny app ----
shinyApp(ui, server)