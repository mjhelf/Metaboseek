#' featurePlotModule
#' 
#' Quickplots (ggplot-based plots of feature properties)
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param FT Mseek Feature Table object
#' @param rname row names of rows in FT selected for plotting
#' 
#' @import ggplot2
#' 
#' @export
featurePlotModule <- function(input, output, session, FT, rname,
                              values = reactiveValues( featureTables = featureTables)
){
  
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
    p <- p + 
      ggplot2::ggtitle(paste0("m/z: ",
                              round(FT()$df[rname(),"mz"],5),
                              " @",round(FT()$df[rname(),"rt"],1),
                              "sec (", round(FT()$df[rname(),"rt"]/60,2)," min)")) +
      ggplot2::theme(plot.title = element_text(size=22, hjust = 0.5))
    
    
    if(input$log10){ p <- p + ggplot2::scale_y_continuous(na.value = 0, trans = "log10")}
    p
    
  }
  })
  output$info <- renderPrint({if(!is.null(rname())){
    summary(as.vector(mx2()$values))}   
  })

    Freeview <- callModule(PlotBrowserModule, "freeview",
                         reactives = reactive({reactiveValues(PCAtable =FT()$df[values$featureTables$row_filters,],
                                                              active = T)}),
                         values = NULL,
                         static = list(patterns = list(axis = "",
                                                       color = "",
                                                       hover = "")))
}

#' featurePlotModuleUI
#' 
#' Quickplots (ggplot-based plots of feature properties) UI
#' 
#' @param id 
#' 
#' @export
featurePlotModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    column(6,
    fluidRow(
      column(3,
             selectizeInput(ns('gtype'), "Plot type", choices = c("by group", "by sample"), selected = "by group"),
             selectizeInput(ns('ptype'), "Plot type", choices = c("none", "boxplot", "barplot", "violinplot"), selected = "barplot")),
      column(3,
             selectizeInput(ns('errorbar'), "Error bar", choices = c("none", "Standard Deviation", "95% Confidence Interval"), selected = "none"),
             selectizeInput(ns('mark'), "Plot additional value", choices = c("none", "mean", "median"), selected = "none")),
      column(3,
             checkboxInput(ns('usenorm'),'use normalized data', value = F),
             checkboxInput(ns('pdots'),'Plot sample values', value = T),
             checkboxInput(ns('rot'),'rotate axis labels', value = F),
             checkboxInput(ns('multidata'),'Combine data from all filtered features', value = F),
             checkboxInput(ns('log10'),'log10 scale', value = F)),
      column(3,
             verbatimTextOutput(ns('info')))),
    fluidRow(
     
      plotOutput(ns('fplot'), height = "550px"))),
    column(6,
           h4("Freestyle data plotting"),
           p("uses entire feature table"),
           PlotBrowserModuleUI(ns("freeview"))
           )
    )
  
}

