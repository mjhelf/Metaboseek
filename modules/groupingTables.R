FileUploadUI <- function(id, ...){
    ns <- NS(id)
    
    fileInput(ns('fileUp'), ...) 
    
}

FileUpload <- function(input, output, session){
    userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$fileUp, message = FALSE))
        input$fileUp
    })
    return(userFile())
}