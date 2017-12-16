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
    selectizeInput(ns('gtype'), "Plot type", choices = c("by group", "by sample"), selected = "by group"),
    selectizeInput(ns('ptype'), "Plot type", choices = c("none", "boxplot", "barplot", "violinplot"), selected = "barplot"),
    selectizeInput(ns('errorbar'), "Error bar", choices = c("none", "Standard Deviation", "95% Confidence Interval"), selected = "none"),
    selectizeInput(ns('mark'), "Plot additional value", choices = c("none", "mean", "median"), selected = "none"),
    checkboxInput(ns('usenorm'),'use normalized data', value = F),
    checkboxInput(ns('pdots'),'Plot sample values', value = T),
    checkboxInput(ns('rot'),'rotate axis labels', value = F),
    checkboxInput(ns('multidata'),'Combine data from all filtered features', value = F),
    checkboxInput(ns('log10'),'log10 scale', value = F),
    plotOutput(ns('fplot')),
    verbatimTextOutput(ns('info'))
  )
  
}

featurePlotModule <- function(input, output, session, FT, rname, heading = "Default"){
  
  sam <- character(0)
  group <- character(0)
  
  gn <- reactive({
                 if(input$usenorm && length(grep("__norm", colnames(FT()$df))) > 0){
      FT()$anagroupnames_norm
    }else{
      FT()$anagroupnames
    }
    })
  
  
  mx2 <- reactive({if (!is.null(rname()) | input$multidata){
    
    
    for(n in 1:length(gn())){
      sam <- c(sam, paste0(gn()[[n]]))
      group <- c(group, rep(names(gn())[n],length(gn()[[n]])))
    }
    
    if(input$multidata){
    intens <- data.frame(sam = rep(sam,length(FT()$filters$sele)),
                         group = rep(group,length(FT()$filters$sele)),
                         stringsAsFactors = T)
    intens$values <- as.vector(t(FT()$df[FT()$filters$sele,sam]))
    
    }else{
      intens <- data.frame(sam,
                           group,
                           stringsAsFactors = T)
      
      intens$values <- as.vector(t(FT()$df[rname(),sam]))
    }
    #print(FT()$df[rname(),intens$sam])
    
    return(intens)}
  })
  
  
  output$fplot <- renderPlot({if(!is.null(rname())){
    p <- groupedplot(data = mx2(), 
                mapping = switch(input$gtype,
                                 "by group" = ggplot2::aes(x=group, y=values),
                                 "by sample" = ggplot2::aes(x=sam, y=values)),
                main = input$ptype,
                dotplot = input$pdots,
                dsize = max(mx2()$values)/50,
                mark = input$mark,
                errorbar = input$errorbar,
                rotate = input$rot)
    if(input$log10){ p <- p + ggplot2::scale_y_continuous(na.value = 0, trans = "log10")}
    p
    
  }
  })
  output$info <- renderPrint({if(!is.null(rname())){
    summary(as.vector(mx2()$values))}   
  })
}