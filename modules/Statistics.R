NormalizeModuleUI <- function(id){
    ns <- NS(id)
    
   actionButton(ns('normbutton'),"Normalize data") 
    
}

NormalizeModule <- function(input, output, session, mx){

    observeEvent(input$normbutton,{
        norm_mx <- featureTableNormalize(mx())
        return(norm_mx)
    })
}





densplotModuleUI <- function(id){
    ns <- NS(id)
    tagList(
    checkboxInput(ns('log'),'log10 scale'),
    plotOutput(ns('dplot')),
    verbatimTextOutput(ns('info'))
    )
    
}

densplotModule <- function(input, output, session, mx, heading = "Default"){
    
    mx2 <- reactive({if (input$log){return(log10(mx()))}else{
        return(mx())}
        })
    
    
    output$dplot <- renderPlot({if(!is.null(mx2())){
                         densplot(mx2(), main = heading)}
        })
    output$info <- renderPrint({if(!is.null(mx2())){
                         summary(as.vector(mx2()))}   
                })
}