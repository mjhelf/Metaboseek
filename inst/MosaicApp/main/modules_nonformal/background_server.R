keyin <- reactiveValues(keyd = "NO")

observeEvent(input$keyd,{keyin$keyd <- input$keyd})