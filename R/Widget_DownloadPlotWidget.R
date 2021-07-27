#' DownloadPlotWidget
#' 
#' Generic module for modal dialogs that are launched from a button.
#' 
#' @inherit MseekWidgets
#' 
#' @param R.filename File name, without extension.
#' @param R.plot The plot object (will be evaluated in plotting device of download handler)
#' @param static passed on to \code{ModalWidget}
#' 
#'  
#' @details Because the UI elements of the modal dialog are passed in as 
#' \code{reactives()$fp}, they can be namespaced and easily accessed in the 
#' parent module that can then handle the input from the modal dialog.
#' \describe{
#' \item{static}{
#' \itemize{
#' \item \code{tooltip} tooltip when hovering over the button
#' \item \code{title} title of the modal dialog
#' \item \code{label} label of the button opening the modal dialog
#' \item \code{icon} \code{\link[shiny]{icon}()} of the button opening the modal dialog
#' }
#' }
#' }
#' 
#' @return returns its internalValues
#' 
#' @examples
#' \dontrun{
#' ui <- function(request){
#' DownloadPlotWidgetUI("PlotDownload")
#' }
#' server <- function(input, output, session) {
#'    callModule(DownloadPlotWidget, "PlotDownload")
#' }
#' #Create Shiny app ----
#' shinyApp(ui, server)
#' }
#' 
#' @describeIn DownloadPlotWidget Server logic
#' 
#' @export 
DownloadPlotWidget <- function(input,output, session,
                             R.filename = reactive({paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"), "_","plotoutput")}),
                             R.plot = reactive({plot(1:3)}),
                               static = list(tooltip = "Download this plot",
                                             title = "Download Options",
                                             label = "",
                                             icon = icon("download", lib = "font-awesome"))
){
  ns <- NS(session$ns(NULL))
  

  dialog <- callModule(ModalWidget, "getbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                           fluidRow(
                             column(3,
                                    textInput(ns("fname"), "File Name:",
                                              value = R.filename())
                                              ),
                             column(3,
                                    selectizeInput(ns("fformat"), "Format", choices = c("pdf"))),
                             
                             
                             column(3,
                                    downloadButton(ns("downloadPlot"), "Download Image")),
                            
                             column(3,
                                    if(ggplot2::is.ggplot(R.plot())){downloadButton(ns("downloadData"), "Download Data")}
                                    )
                             
                              
                           ),
                           fluidRow(
                             column(4,
                                    numericInput(ns("inchwidth"), "Width (inches):",
                                              value = 5,
                                              min = 0
                                              )
                                    ),
                             column(4,
                                    numericInput(ns("inchheight"), "Height (inches):",
                                                 value = 5,
                                                 min = 0
                                    )
                             )),
                           )
                           
                         ) }),
                       static = list(tooltip = "Download plot as pdf file",
                                     title = "Download Plot", 
                                     label = static$label,
                                     icon = static$icon))
  

  
  output$downloadPlot <- downloadHandler(filename= function(){
    paste0(input$fname,".", input$fformat)
  }, 
    content = function(file){
      tryCatch({
        
        showNotification(paste("Downloading plot: ", 
                               paste0(input$fname,".", input$fformat)),
                         #paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))),
                         duration = 10)
        
        
        pdf(file,
            input$inchwidth, input$inchheight
        )
        
        if(ggplot2::is.ggplot(R.plot())){plot(R.plot())
          }else{
        R.plot()
          }
        
        #replayPlot(selections$plots$spec$fullplot)
        dev.off()
        
        removeModal()
        
      },
      error = function(e){
        showNotification(paste("ERROR:  Download failed. Error message: ",e), type = "error", duration = NULL)
        
      })
      
    },
    contentType = "application/pdf")
  
  output$downloadData <- downloadHandler(filename= function(){
    paste0(input$fname,".tsv")
  }, 
  content = function(file){
    tryCatch({
      
      showNotification(paste("Downloading plot data: ", 
                             paste0(input$fname,".tsv")),
                       #paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))),
                       duration = 10)
      
      
     if(ggplot2::is.ggplot(R.plot())){
       fwrite(R.plot()$data,file,sep ='\t')
     }else{
         writeLines("This did not work because this plot is not ggplot-based.", file)
       }
      
    },
    error = function(e){
      showNotification(paste("ERROR:  Download failed. Error message: ",e), type = "error", duration = NULL)
      
    })
    
  },
  contentType = "text/tab-separated-values")
  
  
  }

#' @describeIn DownloadPlotWidget UI elements
#' @export
DownloadPlotWidgetUI <- function(id)
{
  ns <- NS(id)
  
  ModalWidgetUI(ns("getbutton"))
  
}