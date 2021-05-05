#' PlotBrowserModule
#' 
#' MseekWidget to customize ggplot views
#' 
#' @inherit MseekWidgets
#' 
#' @return Returns its internalValues
#' 
#' @describeIn PlotBrowserModule Server logic
#' 
#' @import shiny
#' @importFrom shinyjs toggle toggleElement
#' 
#' @export 
PlotBrowserModule <- function(input,output, session,
                              reactives = reactive({reactiveValues(PCAtable = df,
                                                                   active = T)}),
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
                                   textChoices = NULL,
                                   volcano = F,
                                   logCheck = TRUE)
  

  
  plot1 <- callModule(PlotModule, "Plot1",
                      reactives = reactive({reactiveValues(plot = internalValues$plot,
                                                           interactive = (!is.null(reactives()$PCAtable) && nrow(reactives()$PCAtable) <2000) && internalValues$interactive
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
      
      
      internalValues$axisChoices <- grep(static$patterns$axis,
                                         colnames(reactives()$PCAtable),
                                         value = T)
      internalValues$colorChoices <- grep(static$patterns$color,
                                          colnames(reactives()$PCAtable),
                                          value = T)
      internalValues$textChoices <- grep(static$patterns$hover,
                                         colnames(reactives()$PCAtable),
                                         value = T)
      
      
      
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
       
        
        colvec <- reactives()$PCAtable[[internalValues$color]]
        try({
            suppressWarnings({
          
          if(internalValues$interactive){
            txtmake <- ""
            if(length(internalValues$text) 
               && internalValues$text != ""
               && !all(internalValues$text %in% c(internalValues$x, internalValues$y, internalValues$color))){
              txtmake <- plotlyTextFormatter(reactives()$PCAtable,
                                             internalValues$text[!internalValues$text %in% c(internalValues$x, internalValues$y, internalValues$color)])
            }
            
            #this is to avoid code exposure in hovers with renderPlotly 
            #(where unlike in ggplotly() tooltip cannot be restricted to text only)
            datacopy <- reactives()$PCAtable[,unique(c(internalValues$text,internalValues$x,internalValues$y,internalValues$color))]
            
            if(internalValues$logCheck){
              datacopy[[internalValues$color]] <- safelog(datacopy[[internalValues$color]])
            }
              
            p <- ggplot(reactives()$PCAtable,
                        aes(x=!!rlang::sym(internalValues$x),
                            y=!!rlang::sym(internalValues$y))) + 
              theme_light() +
              geom_point(
                aes(color= !!rlang::sym(internalValues$color),
                text= txtmake
                ),
                # size = 1,
                alpha = 0.8
              )
            if(is.factor(colvec) || is.character(colvec)){
              p <- p +  scale_color_hue()
            }else{
              p <- p + scale_color_gradient2(low = "blue",
                                             mid = "gray",
                                             high = "red",
                                             midpoint = if(internalValues$logCheck){0}else{mean(c(min(reactives()$PCAtable[[internalValues$color]], na.rm = TRUE),
                                                                                                  max(reactives()$PCAtable[[internalValues$color]], na.rm = TRUE)),
                                                                                                  na.rm = TRUE)},
                                             na.value = "white") + 
                labs(color=if(internalValues$logCheck){paste0('log10(',internalValues$color,')')}else{paste0(internalValues$color)})
            }
            
            
          }else{
            
            p <- ggplot(reactives()$PCAtable,
                        aes(x=!!rlang::sym(internalValues$x),
                            y=!!rlang::sym(internalValues$y))) + 
              theme_light() +
              geom_point(
                aes(color=if(internalValues$logCheck){
                  safelog(!!rlang::sym(internalValues$color))
                }else{
                  !!rlang::sym(internalValues$color)}),
                size = 2.5,
               # stroke = 1,
               # shape = 21,
                alpha = 0.8,
               # color = "black"
               ) 
            if(is.factor(colvec) || is.character(colvec)){
              p <- p +  scale_color_hue()
            }else{
              p <- p + scale_color_gradient2(low = "blue",
                                            mid = "gray",
                                            high = "red",
                                            midpoint = if(internalValues$logCheck){0}else{mean(c(min(reactives()$PCAtable[[internalValues$color]], na.rm = TRUE),
                                                                                                 max(reactives()$PCAtable[[internalValues$color]], na.rm = TRUE)),
                                                                                               na.rm = TRUE)},
                                            na.value = "white") + 
                labs(fill=if(internalValues$logCheck){paste0('log10(',internalValues$color,')')}else{paste0(internalValues$color)})
            }
            
          }
          
          if(internalValues$volcano){
            
            p <- p + scale_y_continuous(trans=reverselog_trans(10))  + scale_x_continuous(trans = "log2") + geom_hline(yintercept=0.05, color = "red")
          }
          
          internalValues$plot <- p
           })
        }, silent = F)
        
      }else{
        internalValues$plot <- NULL
      }
      toggleElement(id = 'Text', condition = !is.null(internalValues$interactive) && internalValues$interactive)
    }
    
    toggleElement(id = 'PlotBrowserAll', condition = reactives()$active)
    
  })
  
  output$interactiveCheck <- renderUI({
    div(title= "Use interactive plot. Will automatically be disabled for plots with more than 2000 data points due to performance issues. Filter your data to decrease number of data points.",
        checkboxInput(ns('interactivecheck'), 'interactive', 
                      value = internalValues$interactive))
  })
  
  observeEvent(input$interactivecheck,{
    internalValues$interactive <- input$interactivecheck
  })
  
  output$logCheck <- renderUI({
    div(title= "Use log10-scaled color range for numeric values.",
        checkboxInput(ns('logcheck'), 'log', 
                      value = internalValues$logCheck))
  })
  
  observeEvent(input$logcheck,{
    internalValues$logCheck <- input$logcheck
  })
  
  output$volcanoCheck <- renderUI({
    div(title= "Use volcano style. Will apply log10 to y-axis, and log2 to x-axis. Please select appropriate columns to be plotted as x- and y- values.",
        checkboxInput(ns('volcanocheck'), 'Volcano style', 
                      value = internalValues$volcano))
  })
  
  observeEvent(input$volcanocheck,{
    internalValues$volcano <- input$volcanocheck
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
        column(1,
               htmlOutput(ns('logCheck'))
        ),
        column(2,
               htmlOutput(ns('volcanoCheck'))
        ),
        column(2,
               htmlOutput(ns("xSelect"))
        ),
        column(2,
               htmlOutput(ns("ySelect"))
        ),
        column(2,
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
  
  
  return(internalValues)
  
}

#' @describeIn PlotBrowserModule UI elements
#' @export
PlotBrowserModuleUI <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns('PlotBrowserAll'))
  
}