#' PlotBrowserModule
#' 
#' 
#' prepare text to be shown in plotly through aes(text)
#' 
#' @param df data.frame
#' @param cols character() of column names to display
#' 
plotlyTextFormatter <- function(df, cols){
  
  collect <- list()
  
  for(i in cols){
    collect[[i]] <- paste(i, df[[i]], sep = ": ")
  }
  
  collect2 <- collect[[1]]
  
  if(length(collect) >1){
    for (i in 2:length(collect)){
      collect2 <- paste(collect2, collect[[i]], sep = "\n")
    }
  }
  return(collect2)
  
}




#' PlotBrowserModule
#' 
#' 
#' server module to customize ggplot views
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @import shiny
#' @importFrom shinyjs toggle
#' 
#' @export 
PlotBrowserModule <- function(input,output, session,
                              reactives = reactive({reactiveValues(PCAtable = df,
                                                                   active = T)}),
                              values = NULL,
                              static = list(patterns = list(axis = "PCA__",
                                                            color = "",
                                                            hover = ""))
){
  #### Initialization ####
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(interactive = F,
                                   plot = NULL,
                                   x = NULL,
                                   y = NULL,
                                   axisChoices = NULL,
                                   color = NULL,
                                   shape = NULL,
                                   text = NULL,
                                   textChoices = NULL)
  

  
  plot1 <- callModule(PlotModule, "Plot1",
                      reactives = reactive({reactiveValues(plot = internalValues$plot,
                                                           interactive = (!is.null(reactives()$PCAtable) && nrow(reactives()$PCAtable) <1000) && internalValues$interactive
                      )
                      }))
  
    # observeEvent(c(internalValues$x,
  #                internalValues$y,
  #                internalValues$text,
  #                internalValues$color,
  #                reactives()$PCAtable,
  #                reactives()$active),
               observe({
                   
                   if(!is.null(reactives()$PCAtable) && reactives()$active){
                     
                     
                     internalValues$axisChoices <- grep(static$patterns$axis,colnames(reactives()$PCAtable), value = T)
                     internalValues$colorChoices <- grep(static$patterns$color,colnames(reactives()$PCAtable), value = T)
                     internalValues$textChoices <- grep(static$patterns$hover,colnames(reactives()$PCAtable), value = T)
                     
                     
                     
                     if(((is.null(internalValues$x) || internalValues$x == "")
                         ||(is.null(internalValues$y) || internalValues$y == ""))
                        && length(internalValues$axisChoices) >0){
                       internalValues$x <- internalValues$axisChoices[1]
                       internalValues$y <- internalValues$axisChoices[2]
                     }
                     
                     
                     if((is.null(internalValues$text) || internalValues$text == "")
                        && length(internalValues$textChoices) >0){
                       internalValues$text <- internalValues$textChoices[1]
                     }
                     
                     if((is.null(internalValues$color) || internalValues$color == "")
                        && length(internalValues$colorChoices) >0){
                       internalValues$color <- internalValues$colorChoices[1]
                     }
                     
                     if(length(internalValues$axisChoices != 0)
                        && !is.null(internalValues$x) 
                        && !is.null(internalValues$y)
                        && internalValues$x != ""
                        && internalValues$y != ""){
                       # print(plotlyTextFormatter(reactives()$PCAtable, internalValues$text))
                       #  print(length(internalValues$x))
                       # print(internalValues$y)
                       txtmake <- "data"
                       if(internalValues$interactive && !is.null(internalValues$text) && internalValues$text != ""){
                         txtmake <- plotlyTextFormatter(reactives()$PCAtable, internalValues$text)
                       }
                       colvec <- reactives()$PCAtable[[internalValues$color]]

                       internalValues$plot <- ggplot(reactives()$PCAtable,
                                                     aes_string(x=internalValues$x,
                                                                y=internalValues$y)) + 
                         geom_point(aes_string(col = internalValues$color,
                                               #shape = 1,
                                               text = as.factor(txtmake) 
                         )) +
                         if(is.factor(colvec) || is.character(colvec)){
                           scale_color_hue()
                         }
                       else{
                         scale_color_gradientn(colours=rainbow(4))}
                       
                     }else{
                       internalValues$plot <- NULL
                     }
                     toggleElement(id = 'Text', condition = !is.null(internalValues$interactive) && internalValues$interactive)
                   }
                 
                 toggleElement(id = 'PlotBrowserAll', condition = reactives()$active)
                 
                 })
  
  output$interactiveCheck <- renderUI({
    div(title= "Use interactive plot. Will automatically be disabled for plots with more than 1000 data points due to performance issues. Filter your data to decrease number of data points.",
        checkboxInput(ns('interactivecheck'), 'interactive', 
                      value = internalValues$interactive))
  })
  
  observeEvent(input$interactivecheck,{
    internalValues$interactive <- input$interactivecheck
  })
  
  
  
  output$xSelect <- renderUI({
    
    selectizeInput(ns('xselect'), 'Select x axis value',
                   choices = internalValues$axisChoices,
                   selected = internalValues$x,
                   multiple = F)
    
  })
  
  observeEvent(input$xselect,{
    internalValues$x <- input$xselect
  })
  
  output$ySelect <- renderUI({
    
    selectizeInput(ns('yselect'), 'Select y axis value',
                   choices = internalValues$axisChoices,
                   selected = internalValues$y,
                   multiple = F)
    
  })
  
  observeEvent(input$yselect,{
    internalValues$y <- input$yselect
  })
  
  output$Color <- renderUI({
    
    selectizeInput(ns('color'), 'Color by',
                   choices = internalValues$colorChoices,
                   selected = internalValues$color,
                   multiple = F)
    
  })
  
  observeEvent(input$color,{
    internalValues$color <- input$color
  })
  
  output$Text <- renderUI({
   
      div(id = ns("hoverTextDiv"), title= "Show info on these fields when hovering over a data point in interactive mode.",
          selectizeInput(ns('text'), 'hover info',
                         choices = internalValues$textChoices,
                         selected = internalValues$text,
                         multiple = T)
      
    )
  })
  
  observeEvent(input$text,{
    internalValues$text <- input$text
  })
  
  output$PlotBrowserAll <- renderUI({
    
    fluidPage(
      fluidRow(
        column(1,
               htmlOutput(ns('interactiveCheck'))
        ),
        column(3,
               htmlOutput(ns("xSelect"))
        ),
        column(3,
               htmlOutput(ns("ySelect"))
        ),
        column(3,
               htmlOutput(ns("Color"))
        ),
        column(2,
               htmlOutput(ns("Text"))
        )
      ),
      fluidRow(
        PlotModuleUI(ns('Plot1'))
      )
    )
    
  })
  
  # output$Shape <- renderUI({
  #   
  #   selectizeInput(ns('shape'), 'Color by',
  #                  choices = internalValues$shapeChoices,
  #                  selected = internalValues$shape,
  #                  multiple = F)
  #    
  # })
  # 
  # observeEvent(input$shape,{
  #   internalValues$shape <- input$shape
  # }) 
  
  
  
  
  
  
  
  return(internalValues)
  
}

#' PlotBrowserModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
PlotBrowserModuleUI <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns('PlotBrowserAll'))
  
}


#' PlotModule
#' 
#' 
#' server module for ggplot2 handler
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' 
#' @import shiny ggplot2
#' @importFrom shinyjs hideElement showElement toggle
#' @importFrom plotly renderPlotly
#' 
#' @export 
PlotModule <- function(input,output, session,
                       reactives = reactive({list(reactiveValues(plot = p,
                                                                 interactive = F))})
){
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(interactive = F)
  
  # observeEvent(c(reactives()$interactive,
  #                reactives()$plot),{
  #    toggle(id = 'fplot', condition = !is.null(reactives()$plot) && !reactives()$interactive)
  #   toggle(id = 'iplot', condition = !is.null(reactives()$plot) && reactives()$interactive)
  #   
  # }, ignoreNULL = F)
  
  output$fplot <- renderPlot({
   
    
    if(!is.null(reactives()$plot) && !is.null(reactives()$interactive) && !reactives()$interactive){
      hideElement(id = 'iplot')
      
      reactives()$plot
    }
    else{
      showElement(id = 'iplot')
      NULL
    }
    
  })
  
  output$iplot <- renderPlotly({
    if(!is.null(reactives()$plot) && !is.null(reactives()$interactive) && reactives()$interactive){
      hideElement(id = 'fplot')
      
      reactives()$plot
      
    }
    else{
      showElement(id = 'fplot')
      
      NULL
    }
  })
  
  observe({
    
  
    
  })
  
  
  
}

#' PCABrowserModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @import shiny
#' @importFrom plotly plotlyOutput
#' 
#' @export
PlotModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      plotOutput(ns('fplot'), height = "550px"),
      plotlyOutput(ns('iplot'), height = "550px")
      
    )
  )
  
  
}