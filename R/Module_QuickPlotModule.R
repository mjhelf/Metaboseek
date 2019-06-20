#' QuickPlotsModule
#' 
#' Show summary data plots of selected features using ggplot plotting devices
#' 
#' @inherit MseekModules
#' 
#' @import ggplot2
#' 
#' @describeIn QuickPlotsModule server logic for QuickPlotsModule
#' @export
QuickPlotsModule <- function(input, output, session, 
                              values = reactiveValues( featureTables = featureTables)){
  
  
  
  gn <- reactive({
    if(input$usenorm && length(grep("__norm", colnames(FeatureTable(values)$df))) > 0){
      FeatureTable(values)$anagroupnames_norm
    }else{
      FeatureTable(values)$anagroupnames
    }
  })
  
  
  mx2 <- reactive({if ((!is.null(FTselection(values)) 
                        && is.data.frame(FTselection(values)) 
                        && nrow(FTselection(values)) > 0)
                       || (input$multidata
                           && !is.null(FeatureTable(values)$df))){
    
    sam <- character(0)
    group <- character(0)
    
    for(n in 1:length(gn())){
      sam <- c(sam, paste0(gn()[[n]]))
      group <- c(group, rep(names(gn())[n],length(gn()[[n]])))
    }
    
    if(input$multidata){
      intens <- data.frame(sam = rep(sam,nrow(FeatureTable(values)$df[values$featureTables$row_filters,])),
                           group = rep(group,nrow(FeatureTable(values)$df[values$featureTables$row_filters,])),
                           stringsAsFactors = T)
      intens$values <- as.vector(t(FeatureTable(values)$df[values$featureTables$row_filters,sam]))
      
    }else{
      intens <- data.frame(sam,
                           group,
                           stringsAsFactors = T)
      
      intens$values <- as.vector(t(FTselection(values)[1,sam]))
    }

    return(intens)}
  })
  
  
  output$fplot <- renderPlot({if(!is.null(FTselection(values)) 
                                 && is.data.frame(FTselection(values)) 
                                 && nrow(FTselection(values)) > 0){
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
      ggplot2::ggtitle(if(input$multidata){paste0("Showing data for ",
                                                  nrow(FeatureTable(values)$df[values$featureTables$row_filters,]),
                                                  " features")}
                       else{paste0(
                              "m/z: ",
                              round(FTselection(values)[1,"mz"],5),
                              " @",round(FTselection(values)[1,"rt"],1),
                              "sec (", round(FTselection(values)[1,"rt"]/60,2)," min)")}) +
      ggplot2::theme(plot.title = element_text(size=22, hjust = 0.5))
    
    
    if(input$log10){ p <- p + ggplot2::scale_y_continuous(na.value = 0, trans = "log10")}
    p
    
  }
  })
  output$info <- renderPrint({if(!is.null(mx2())
                                 &&!is.null(mx2()$values)){
    summary(as.vector(mx2()$values))}   
  })
  
  Freeview <- callModule(PlotBrowserModule, "freeview",
                         reactives = reactive({reactiveValues(PCAtable =FeatureTable(values)$df[values$featureTables$row_filters,],
                                                              active = T)}),
                         static = list(patterns = list(axis = "",
                                                       color = "",
                                                       hover = "")))
}


#' @describeIn QuickPlotsModule UI elements for QuickPlotsModule
#'  @export
QuickPlotsModuleUI <- function(id){
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

