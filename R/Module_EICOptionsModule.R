#' EICOptionsModule
#' 
#' 
#' Module to change EIC options
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
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
    MseekOptions(plotYzoom=input$plotYzoom)
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
  
  
}

#' EICOptionsModuleUI
#' 
#' 
#' Module to change EIC options
#' 
#' @param id id of the module
#' 
#' @export 
EICOptionsModuleUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    title = "EIC Options Module",
    # htmlOutput(ns("TICtoggle")),
    fluidRow(
      column(3,
             htmlOutput(ns("PPMwindow"))
      ),
      column(3,
             htmlOutput(ns("plotCols"))
      ),
      column(2,
             htmlOutput(ns("TICtoggle")
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
      )
    ),
    
    fluidRow(
      column(3,
             htmlOutput(ns("plotYzoom"))),
      
      column(3,
             htmlOutput(ns("plotLw"))),
      
      column(2,
             htmlOutput(ns("MLtoggle"))),
      
      column(2,
             htmlOutput(ns("plotCx"))),
      
      column(2,
             htmlOutput(ns("colorscheme"))
      )))}
