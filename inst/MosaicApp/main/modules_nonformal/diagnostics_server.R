runcodeServer()

output$diag <- renderPrint({
 

})

output$tester <- renderPlot({
  print(input$tester$click)
  plot(1:5,c(1,3,2,1,4))
})

observeEvent(input$tester_click,{
  print("click")
})
observeEvent(input$tester_dblclick,{
  print("dblclick")
})