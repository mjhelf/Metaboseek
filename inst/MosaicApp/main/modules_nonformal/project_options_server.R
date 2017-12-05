
 

output$projectName <- renderUI({
    textInput('projectNamex', 'Project Name:', value = paste0("MOSAiC_session_",timeStamp))
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