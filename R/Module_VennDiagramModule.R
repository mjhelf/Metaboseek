#' @title VennDiagramModule
#' 
#' @description server module for Venn diagrams in Metaboseek
#' 
#' @inherit MseekModules
#' @describeIn VennDiagramModule Server logic
#' 
#' @return Returns its internalValues
#' 
#' 
#' @export 
VennDiagramModule <- function(input,output, session, values,
                         reactives = reactive({list(active = T,
                                                    highlights = integer(0) # fixed__id values of nodes to be highlighted
                         )
                         }),
                         static = list(noSelection = T),
                         keys
){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(
    plotprops = NULL,
  
  Venn1 =  callModule(MultiFilterModule, "venn1", values = values,
                             static = list(lab = "VennFil1",
                                           modFeatureTable = F,
                                           name = "Group A",
                                           activate = T)),
  
  Venn2 =  callModule(MultiFilterModule, "venn2", values = values,
                       static = list(lab = "VennFil2",
                                     modFeatureTable = F,
                                     name = "Group B",
                                     activate = T)),
  
  Venn3 =  callModule(MultiFilterModule, "venn3", values = values,
                       static = list(lab = "VennFil3",
                                     modFeatureTable = F,
                                     name = "Group C",
                                     activate = T))
  )
  
  observeEvent(c(internalValues$Venn1$results, 
                 internalValues$Venn2$results,
                 internalValues$Venn3$results,
                 internalValues$Venn1$active,
                 internalValues$Venn2$active,
                 internalValues$Venn3$active,
                 internalValues$Venn1$name,
                 internalValues$Venn2$name,
                 internalValues$Venn3$name,
                 values$GlobalOpts$colorscheme,
                 input$scaleVenn,
                 input$eulerCheck,
                 values$featureTables$row_filters,
                 input$preFilter),{
                   
                   groups <- list()
                   fillcol <- character(0)
                   lwd <- integer(0)
                   
                   availablecols <- do.call(values$GlobalOpts$colorscheme, list(n = 3, alpha = 0.6))
                   
                   for( i in grep("Venn", names(internalValues), value = T)){
                     
                     if(!is.null(internalValues[[i]]$results) && !is.null(internalValues[[i]]$active) && internalValues[[i]]$active){
                       
                       thisres <- if(length(internalValues[[i]]$results) == 1){rep(TRUE,nrow(values$featureTables$tables[[values$featureTables$active]]$df))}else{internalValues[[i]]$results}

                       
                       if(input$preFilter && !is.null(values$featureTables$row_filters)){
                         
                         thisres <- thisres & values$featureTables$row_filters
                         
                       }
                       
                       thisres <- which(thisres)
                       
                       
                       #this handling effectively allows for an OR filter setup
                       
                       if(internalValues[[i]]$name %in% names( groups)){
                         
                         groups[[internalValues[[i]]$name]] <- unique(c(groups[[internalValues[[i]]$name]], thisres))  
                         
                       }else{
                         groups[[internalValues[[i]]$name]] <- thisres
                         
                         fillcol <- c(fillcol,availablecols[length(fillcol)+1])
                         
                         lwd <- c(lwd,values$GlobalOpts$plotLw)
                         
                       }
                       
                     }
                     
                     
                   }
                   
                 internalValues$plotprops <- list( x = groups,
                         ext.percent = c(0.01,0.01,0.01),
                         lwd = lwd,
                         fill = fillcol,
                         filename = NULL,
                         scaled = input$scaleVenn && input$eulerCheck,
                         euler.d = input$eulerCheck,
                         cat.fontfamily = "sans",
                         fontfamily = "sans",
                         main.fontfamily = "sans",
                         sub.fontfamily = "sans",
                         cat.fontface = "bold",
                         height = 3000,
                         width = 3000)
                   
                   
                 })
  
  VennPlot <- callModule(VennWidgetModule, "vennplot", reactives = reactive({  
    
     internalValues$plotprops
    }))
   
  return(internalValues)
  
}

#' @describeIn VennDiagramModule UI elements
#' @export
VennDiagramModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(VennWidgetModuleUI(ns("vennplot"))),
    fluidRow(column(4,
                    
      checkboxInput(ns("scaleVenn"), "Scale circle size", value = T)
    ),
    column(4,
           
           checkboxInput(ns("eulerCheck"), "Euler Diagram", value = T)
    ),
    column(4,
           
           checkboxInput(ns("preFilter"), "Apply global table filter", value = T)
    )
    ),
    hr(),
    fluidRow(
      column(4,
             box(width = 12,
             fluidRow(
             MultiFilterModuleUI(ns("venn1"))
             )
             )
             ),
      column(4,
             box(width = 12,
             fluidRow(
               MultiFilterModuleUI(ns("venn2"))
             )
             )
      ),
      column(4,
             box(width = 12,
             fluidRow(
               MultiFilterModuleUI(ns("venn3"))
             )
             )
      )
    )
  )
  
}