#' PlotModule
#' 
#' Module for ggplot2 handler
#' 
#' @inherit MseekWidgets
#' 
#' @return Returns nothing but provides a ggplot GUI output
#' 
#' @describeIn PlotModule Server logic
#' 
#' @import shiny ggplot2
#' @importFrom shinyjs hideElement showElement toggle
#' @importFrom plotly renderPlotly plotlyOutput
#' 
#' @export 
PlotModule <- function(input,output, session,
                       reactives = reactive({list(plot = NULL,
                                                                 interactive = F)})
){
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(interactive = F)
  
  
 callModule(DownloadPlotWidget, "dlbutton",
            R.filename = reactive({paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"), "_","plotoutput")}),
            R.plot = reactive({reactives()$plot}),
            static = list(tooltip = "Download this plot",
                          title = "Download Options",
                          label = "",
                          icon = icon("download", lib = "font-awesome"))
                      )
  
  # observeEvent(c(reactives()$interactive,
  #                reactives()$plot),{
  #    toggle(id = 'fplot', condition = !is.null(reactives()$plot) && !reactives()$interactive)
  #   toggle(id = 'iplot', condition = !is.null(reactives()$plot) && reactives()$interactive)
  #   
  # }, ignoreNULL = F)
 
# observeEvent(reactives()$plot,{hideElement(id = 'iplot');hideElement(id = 'fplot')}, ignoreNULL = FALSE, ignoreInit = FALSE, once = TRUE)
  
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
  
  
  
}

#' @describeIn PlotModule UI elements
#' @export
PlotModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    div(class = "flex-container",
        style = "display: flex; flex-direction: row",
        div(class = "column",
            DownloadPlotWidgetUI(ns("dlbutton"))),
        div(style = "width:100%",
            plotOutput(ns('fplot'), height = "550px"),
            plotlyOutput(ns('iplot'), height = "550px")
    )
    )
  #  fluidRow(
  #    plotOutput(ns('fplot'), height = "550px"),
  #    plotlyOutput(ns('iplot'), height = "550px")
      
  #  )
  )
  
  
}