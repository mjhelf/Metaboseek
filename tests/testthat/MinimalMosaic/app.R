library(Mosaic)
library(shiny)
library(shinyjs)

#MosaicExamplePreload()

ui <- MosaicMinimalUi(diagnostics = T)

server <- function(input, output) {
  MosaicMinimalServer(diagnostics = T, data = F, tables = F)
  
  output$diag <- renderPrint({
    print(featureTables$tables$table0)
  })
  
  }

# Create Shiny app ----
shinyApp(ui, server)