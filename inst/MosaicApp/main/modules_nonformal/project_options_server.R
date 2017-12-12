
 

output$projectName <- renderUI({
    textInput('projectNamex', 'Project Name:', value = projectData$projectName)
})


output$rootfol <- renderUI({
  if(!servermode){
  textInput('rootfol', 'Root folder', value = "")
}
    })

observeEvent(input$changeRoot,{
  if(input$rootfol != "" & !servermode){
    
    MSData$rootfolder <- gsub("\\\\","/",input$rootfol)
    
  }
  
  
})

toggle('changeRoot', condition = !servermode)