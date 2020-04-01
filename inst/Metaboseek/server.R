function(input, output, session) {
    
    if(.MseekOptions$develMode){
        shinyjs::runcodeServer()
        }
    
  callModule(MseekContainer, "Mseek")
}