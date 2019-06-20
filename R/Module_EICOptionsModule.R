#' EICOptionsModule
#'
#' Module to change EIC options
#' 
#' @inherit MseekModules
#' 
#' @details 
#' Returns nothing, but writes to \code{values$GlobalOpts}
#' 
#' @describeIn EICOptionsModule server logic for the EICOptionsModule
#' 
#' @import shiny
#' @importFrom parallel detectCores
#' 
#' @export 
EICOptionsModule <- function(input,output, session,
                             values = reactiveValues(GlobalOpts = GlobalOpts)){
  #### Initialization ####
  
  ns <- NS(session$ns(NULL))
  
  
  
  output$TICtoggle <- renderUI({
    checkboxInput(ns("TICtoggle"),"TIC", value = values$GlobalOpts$TICtoggle)
  })
  
  observeEvent(input$TICtoggle,{
    values$GlobalOpts$TICtoggle <- input$TICtoggle
    MseekOptions(TICtoggle = input$TICtoggle)
  })
  
  observeEvent(input$RTwindow,{
    values$GlobalOpts$RTwindow <- input$RTwindow
    MseekOptions(RTwindow=input$RTwindow)
  })
  
  output$RTwindow <- renderUI({
    div(title= "Retention time window in EICs",
        numericInput(ns("RTwindow"), "RT window (sec):", value =  values$GlobalOpts$RTwindow)
    )
  })
  
  output$RTtoggle <- renderUI({
    checkboxInput(ns("RTtoggle"),"Full RT range", value = values$GlobalOpts$RTtoggle)
  })
  
  observeEvent(input$RTtoggle,{
    values$GlobalOpts$RTtoggle <- input$RTtoggle
    MseekOptions(RTtoggle=input$RTtoggle)
  })
  
  output$PPMwindow <- renderUI({
    numericInput(ns("PPMwindow"),"Mass tol (ppm): ", value = values$GlobalOpts$PPMwindow, min = 0)
  })
  # observeEvent(input$PPMwindow,{
  #   if(!is.null(MSData$active)){
  #     MSData$layouts[[MSData$active]]$settings$ppm <- input$PPMwindow
  #   }
  # })
  observeEvent(input$PPMwindow,{
    values$GlobalOpts$PPMwindow <- input$PPMwindow
    MseekOptions(PPMwindow=input$PPMwindow)
  })
  
  output$plotCols <- renderUI({
    numericInput(ns("plotCols"),"Plots per row: ", value = values$GlobalOpts$plotCols, min = 1)
  })
  # observeEvent(input$plotCols,{
  #   if(!is.null(MSData$active)){
  #     MSData$layouts[[MSData$active]]$settings$cols <- input$plotCols
  #   }
  # })
  observeEvent(input$plotCols,{
    values$GlobalOpts$plotCols <- input$plotCols
    MseekOptions(plotCols=input$plotCols)
  })
  
  output$plotYzoom <- renderUI({
    numericInput(ns("plotYzoom"),"Y-axis zoom: ", value = values$GlobalOpts$plotYzoom, min = 0.1)
  })
  observeEvent(input$plotYzoom,{
    values$GlobalOpts$plotYzoom <- input$plotYzoom
    
    #Don't want this to be a default setting when loading new session
    # MseekOptions(plotYzoom=input$plotYzoom)
  })
  
  output$plotLw <- renderUI({
    numericInput(ns("plotLw"),"Line width: ", value = values$GlobalOpts$plotLw, min = 1, step = 1)
  })
  observeEvent(input$plotLw,{
    values$GlobalOpts$plotLw <- input$plotLw
    MseekOptions(plotLw=input$plotLw)
  })
  
  output$MLtoggle <- renderUI({
    checkboxInput(ns("MLtoggle"),"Mark feature RT", value = values$GlobalOpts$MLtoggle)
  })
  observeEvent(input$MLtoggle,{
    values$GlobalOpts$MLtoggle <- input$MLtoggle
    MseekOptions(MLtoggle=input$MLtoggle)
  })
  
  output$raisetoggle <- renderUI({
    checkboxInput(ns("raiseToggle"),"Raise EICs", value = values$GlobalOpts$raiseToggle)
  })
  observeEvent(input$raiseToggle,{
    values$GlobalOpts$raiseToggle <- input$raiseToggle
    MseekOptions(raiseToggle = values$GlobalOpts$raiseToggle)
  })
  
  output$relplottoggle <- renderUI({
    checkboxInput(ns("relPlotToggle"),"Relative intensities", value = values$GlobalOpts$relPlotToggle)
  })
  observeEvent(input$relPlotToggle,{
    values$GlobalOpts$relPlotToggle <- input$relPlotToggle
    MseekOptions(relPlotToggle= values$GlobalOpts$relPlotToggle)
  })
  
  
  output$plotCx <- renderUI({
    numericInput(ns("plotCx"),"Font size: ", value = values$GlobalOpts$plotCx, min = 0.1, step = 0.1)
  })
  observeEvent(input$plotCx,{
    values$GlobalOpts$plotCx <- input$plotCx
    MseekOptions(plotCx=input$plotCx)
  })
  
  
  output$colorscheme <- renderUI({
    selectizeInput(ns("colorscheme"),"Color palette: ", 
                   choices= c("Mseek.colors", "topo.colors", "rainbow", "heat.colors", "terrain.colors", "cm.colors"),
                   selected = NULL
    )
  })
  # observeEvent(input$colorscheme,{
  #   if(!is.null(MSData$active)){
  #     MSData$layouts[[MSData$active]]$settings$colr <- input$colorscheme
  #   }
  # })
  observeEvent(input$colorscheme,{
    values$GlobalOpts$colorscheme <- input$colorscheme
    MseekOptions(colorscheme=input$colorscheme)
  })
  
  output$groupby <- renderUI({
    selectizeInput(ns("groupBy"),"Group by: ", 
                   choices= list("Group"= "grouping",
                              "Group2" = "grouping2"),
                   selected = values$GlobalOpts$groupBy
    )
  })
  # observeEvent(input$colorscheme,{
  #   if(!is.null(MSData$active)){
  #     MSData$layouts[[MSData$active]]$settings$colr <- input$colorscheme
  #   }
  # })
  observeEvent(input$groupBy,{
    values$GlobalOpts$groupBy <- input$groupBy
    # MseekOptions(colorscheme=input$colorscheme)
  })
  
  output$colorby <- renderUI({
    selectizeInput(ns("colorBy"),"Color by: ", 
                   choices= list("File" = "file",
                                 "Group"= "grouping",
                                 "Group2" = "grouping2",
                                 "Mass shift"),
                   selected = values$GlobalOpts$colorBy
    )
  })
  # observeEvent(input$colorscheme,{
  #   if(!is.null(MSData$active)){
  #     MSData$layouts[[MSData$active]]$settings$colr <- input$colorscheme
  #   }
  # })
  observeEvent(input$colorBy,{
    values$GlobalOpts$colorBy <- input$colorBy
    # MseekOptions(colorscheme=input$colorscheme)
  })
}

#' @describeIn EICOptionsModule UI elements for the EICOptionsModule
#' @export 
EICOptionsModuleUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    title = "EIC Options Module",
    # htmlOutput(ns("TICtoggle")),
    fluidRow(
      column(2,
             htmlOutput(ns("PPMwindow"))
      ),
      column(2,
             htmlOutput(ns("plotCols"))
      ),
      column(1,
             htmlOutput(ns("TICtoggle")
             )),
      column(1,
             htmlOutput(ns("relplottoggle")
             )),
      
      column(2,
             htmlOutput(ns("RTwindow"))
             #     HTML('
             #         <div class="form-group shiny-input-container" style="display:inline-block">
             #         <label>RT window (sec): </label>
             #         <input id="RTwindow" type="number" class="form-control" value="30" min="0"/>
             #         </div>
             #         ')
      ),
      column(2,
             htmlOutput(ns("RTtoggle"))
      ),
      
      column(2,
             htmlOutput(ns("groupby"))
      )
    ),
    
    fluidRow(
      column(2,
             htmlOutput(ns("plotYzoom"))),
      
      column(2,
             htmlOutput(ns("plotLw"))),
      
      column(1,
             htmlOutput(ns("MLtoggle"))),
      column(1,
             htmlOutput(ns("raisetoggle"))),
      
      column(2,
             htmlOutput(ns("plotCx"))),
      
      column(2,
             htmlOutput(ns("colorscheme"))),
      
      column(2,
             htmlOutput(ns("colorby"))
      )
    ))}
