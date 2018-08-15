library(Mosaic)
library(shiny)
library(shinyjs)

#MosaicExamplePreload()

ui <- MosaicMinimalUI(
  TableModule2UI("Testtab")
  ,diagnostics = T)

server <- function(input, output) {
  MosaicMinimalServer(diagnostics = T, data = F, tables = T)
  
  TTab <- callModule(TableModule2, "Testtab",
                     reactives = reactive({reactiveValues(df = featureTables$tables[[featureTables$active]]$df,
                                                          rowFilters = NULL,
                                                          colFilters = NULL)}),
                     values = NULL,
                     static = list(perpage = 100,
                                   height = 300,
                                   readOnly = T,
                                   contextMenu = T,
                                   fixedColumnsLeft = 1,
                                   invertReadOnly = "comments",
                                   format = list(col = NULL,
                                                 format = NULL))
  )
  
  output$diag <- renderPrint({
    print(TTab$df)
    print(TTab$showTable)
  })
  
  }

# Create Shiny app ----
shinyApp(ui, server)