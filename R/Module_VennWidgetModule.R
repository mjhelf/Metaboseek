
#' VennWidgetModule
#' 
#' 
#' server module for Venn diagrams in shiny
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' 
#' @importFrom grid grid.draw
#' @importFrom  VennDiagram venn.diagram
#' 
#' @export 
VennWidgetModule <- function(input,output, session, 
                             reactives = reactive({  
                               list( x = list(
                                 A = 1:105,
                                 B = 101:115),
                               ext.percent = c(0.01,0.01,0.01),
                               filename = NULL)        }),
                             static = list()
){

output$vennout <- renderPlot({
  
  try({
  grid.draw(
    do.call(venn.diagram, reactives())
   )
  }, silent = T)

  
}, height = 500, width = 600)

output$pdfButton <- downloadHandler(filename= function(){
  titleout <- "VennDiagram"
  return(paste0(titleout,".pdf"))}, 
  content = function(file){
 pdf(file,
        12,10
    )
    
    try({
    grid.draw(
      do.call(venn.diagram, reactives()
    ))
    }, silent = T)
    #replayPlot(selections$plots$spec$fullplot)
    dev.off()
    
  },
  contentType = "application/pdf")

}

#' VennWidgetModuleUI
#' 
#' 
#' UI module for Venn diagrams in shiny
#' 
#' @param id id to be used in ns()
#' 
#' @export
VennWidgetModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(downloadButton(ns("pdfButton"))),
    fluidRow(column(12, align = "center",
                    plotOutput(ns("vennout"), height = 500, width = 600)
                    )
             )
    )
  
  
}
