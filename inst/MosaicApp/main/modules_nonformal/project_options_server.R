
 

output$projectName <- renderUI({
    textInput('projectNamex', 'Project Name:', value = projectData$projectName)
})


output$rootfol <- renderUI({
  if(!.MosaicOptions$serverMode){
  textInput('rootfol', 'Root folder', value = "")
}
    })

observeEvent(input$changeRoot,{
  if(input$rootfol != "" & !.MosaicOptions$serverMode){
    
    MSData$rootfolder <- gsub("\\\\","/",input$rootfol)
    
  }
  
  
})

toggle('changeRoot', condition = !.MosaicOptions$serverMode)