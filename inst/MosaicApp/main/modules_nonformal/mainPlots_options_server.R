output$TICtoggle <- renderUI({
  checkboxInput("TICtoggle","TIC", value = F)
})
output$RTtoggle <- renderUI({
  checkboxInput("RTtoggle","Full RT range", value = F)
})

#output$RTwindow <- renderUI({
# numericInput("RTwindow","RT window (sec): ", value = 30, min = 0, width =60)
#})
observeEvent(input$RTwindow,{  if(!is.null(MSData$active)){
  MSData$layouts[[MSData$active]]$settings$rtw <- input$RTwindow
}
})

output$PPMwindow <- renderUI({
  numericInput("PPMwindow","Mass tol (ppm): ", value = 5, min = 0)
})
observeEvent(input$PPMwindow,{
  if(!is.null(MSData$active)){
    MSData$layouts[[MSData$active]]$settings$ppm <- input$PPMwindow
  }
})

output$plotCols <- renderUI({
  numericInput("plotCols","Plots per row: ", value = 1, min = 1)
})
observeEvent(input$plotCols,{
  if(!is.null(MSData$active)){
    MSData$layouts[[MSData$active]]$settings$cols <- input$plotCols
  }
})

output$plotYzoom <- renderUI({
  numericInput("plotYzoom","Y-axis zoom: ", value = 1, min = 0.1)
})

output$plotLw <- renderUI({
  numericInput("plotLw","Line width: ", value = 1, min = 0)
})

output$MLtoggle <- renderUI({
  checkboxInput("MLtoggle","Mark feature RT", value = F)
})

output$plotCx <- renderUI({
  numericInput("plotCx","Font size: ", value = 1, min = 0.1)
})

output$plotAdducts <- renderUI({
  textInput("plotAdducts","Mass shifts: ", value = "")
})
