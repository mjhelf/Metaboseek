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
       
        txtmake <- "data"
        if(internalValues$interactive && !is.null(internalValues$text) && internalValues$text != ""){
          txtmake <- plotlyTextFormatter(reactives()$PCAtable, internalValues$text)
        }
        colvec <- reactives()$PCAtable[[internalValues$color]]
        suppressWarnings({
          
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
          
        })
        
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