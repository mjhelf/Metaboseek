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

featurePlotModuleUI <- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(ns('ptype'), "Plot type", choices = c("none", "boxplot", "barplot"), selected = "barplot"),
    selectizeInput(ns('errorbar'), "Error bar", choices = c("none", "Standard Deviation", "95% Confidence Interval"), selected = "none"),
    selectizeInput(ns('mark'), "Plot additional value", choices = c("none", "mean", "median"), selected = "none"),
    checkboxInput(ns('usenorm'),'use normalized data', value = F),
    checkboxInput(ns('pdots'),'Plot sample values', value = T),
    checkboxInput(ns('rot'),'rotate axis labels', value = F),
    plotOutput(ns('fplot')),
    verbatimTextOutput(ns('info'))
  )
  
}

featurePlotModule <- function(input, output, session, FT, rname, heading = "Default"){
  
  sam <- character(0)
  group <- character(0)
  
  suffix <- ""
  
  
  mx2 <- reactive({if (!is.null(rname())){
    if(input$usenorm && length(grep("__norm", colnames(FT()$df))) > 0){
      suffix <- "__norm"
    }
    
    for(n in 1:length(FT()$sNames)){
      sam <- c(sam, paste0(FT()$sNames[[n]], "__norm"))
      group <- c(group, rep(names(FT()$sNames)[n],length(FT()$sNames[[n]])))
    }
    
    intens <- data.frame(sam,
                         group,
                         stringsAsFactors = T)
    
    intens$values <- t(FT()$df[rname(),intens$sam])
    return(intens)}
  })
  
  
  output$fplot <- renderPlot({if(!is.null(mx2())){
    groupedplot(mx2(), ggplot2::aes(x=group, y=values),
                main = input$ptype,
                dotplot = input$pdots,
                dsize = (max(mx2()$values)-min(mx2()$values))/50,
                mark = input$mark,
                errorbar = input$errorbar,
                rotate = input$rot)
  }
  })
  output$info <- renderPrint({if(!is.null(mx2())){
    summary(as.vector(mx2()$values))}   
  })
}