#' SpecplotWidget
#' 
#' 
#' Module for interactive mass spectrum view
#' @inheritParams SpecModule2
#' @param keys a \link[shiny]{reactive} object registering keystrokes
#' @decribeIn SpecplotWidget Server module, to be called with \link[shiny]{callModule}()
#' 
#' @export 
SpecplotWidget <- function(input,output, session, reactives = reactive({list(x=sc[,1],
                                                                   y=sc[,2],
                                                                   norm=max(y)/100,
                                                                   cx=1.5,
                                                                   k = 10,
                                                                   fileName = "title",
                                                                   yrange = c(0,100),
                                                                   xrange = range(x),
                                                                   maxi = max(y),
                                                                   labels = NULL,
                                                                   mar = c(4,6,6,2),
                                                                   ylab = "Relative Intensity (%)",
                                                                   ylabshift = 0)}),
                           layout = reactive({list(active = T,
                                                                            height = 550,
                                         selectCallback = T)}),
                           
                           
                       keys  = reactive({"NO"})){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(plotArgs = NULL,
                                   maxxrange = NULL, #maximum x axis range
                                   maxyrange = NULL, #maximum y axis range
                                   
                                   marker = NULL, #selected peak with $mz and $intensity
                                   highlights = NULL, #peaks to be highlighted with $mz and $intensity
                                   hover = NULL, #peak hovered over with $mz and $intensity
                                   
                                   data = NULL,
                                   ymax = 100
                                   
  )
  
 # observeEvent(layout(),{
 #    if(!is.null(layout())){
 #      if(length(layout()$highlights) > 0){
 #        internalValues$highlights <- layout()$highlights
 #      }else{
 #        internalValues$highlights <- NULL
 #      }
 #      
 #      
 #      
 #    }
 #  })
 
 observeEvent(reactives(),{
   if(!is.null(reactives()) 
      && !is.null(reactives()$spectrum)
      &&  !is.null(layout()$active) 
      && layout()$active){
     internalValues$plotArgs <- reactives()
     
     #assume that entire data range is passed in with reactives():
     
     if(!is.null(internalValues$plotArgs$norm)){
       norm <- internalValues$plotArgs$norm
     }else if(!is.null(internalValues$plotArgs$xrange)
              &&nrow(internalValues$plotArgs$spectrum) > 0){
       inrange <- internalValues$plotArgs$spectrum[internalValues$plotArgs$spectrum[,1] >= internalValues$plotArgs$xrange[1]
                                                   & internalValues$plotArgs$spectrum[,1] <= internalValues$plotArgs$xrange[2],2]
       
         norm <-  if(length(inrange)>0){max(inrange)/100}else{1}
       
     }else if(nrow(internalValues$plotArgs$spectrum) > 0){
       norm <-  max(internalValues$plotArgs$spectrum)/100
     }else{
       norm <- 1
     }
     
     if(norm != 0){internalValues$norm <- norm}else{internalValues$norm <- 1}
     

     internalValues$data <- data.frame(mz = internalValues$plotArgs$spectrum[,1],
                                       intensity = internalValues$plotArgs$spectrum[,2]/internalValues$norm)
     
   
    
     
   }else{
     
     internalValues$plotArgs <- NULL
     internalValues$highlights <- NULL
     
   }
   internalValues$marker <- NULL
   
   internalValues$click <- NULL
   internalValues$dblclick <- NULL
   internalValues$hover <- NULL
   
   
 })
 
  #encapsulates the plot with a div to switch out tooltips 
  #without rerendering the plot
  output$specAll <- renderUI({
    if(
       !is.null(layout()) && layout()$active
    ){
      scallback <- !is.null(layout()$selectCallback) && layout()$selectCallback
      
      div(title = if(!is.null(internalValues$tooltip)){
        internalValues$tooltip
        }else if(!is.null(internalValues$hover)){
        round(internalValues$hover$mz,5)
        }else{""},# layout()$tooltip,
          htmlOutput(ns("specPlotOnly"))
      )
    }
  })
  
  output$specPlotOnly <- renderUI({
    if(!is.null(internalValues$plotArgs)
      && !is.null(layout()) && layout()$active
    ){
      scallback <- !is.null(layout()$selectCallback) && layout()$selectCallback
      
          plotOutput(ns("Mspec"),
                     click = if(scallback){clickOpts(ns("Mspec_click"), 
                                                     clip = T)}else{NULL},
                     
                     hover = if(scallback){hoverOpts(id = ns("Mspec_hover"),
                                                     delay = 10)}else{NULL},
                     
                     dblclick = if(scallback){dblclickOpts(id = ns("Mspec_dblclick"),
                                                           clip = F, 
                                                           delay = 400)}else{NULL},
                     
                     brush = if(scallback){brushOpts(id = ns("Mspec_brush"),
                                                     direction = "x",
                                                     resetOnNew = TRUE,
                                                     delayType = "throttle"
                     )}else{NULL},
                     
                     height = if(is.null(layout()$height)){"550px"}else{paste0(layout()$height,"px")}
          )
      
    }
  })
  
  
  output$Mspec <- renderPlot({
    if(!is.null(internalValues$plotArgs)){
      
      if(!is.null(internalValues$marker)){
        
        internalValues$plotArgs$highlights <- data.frame(mz = internalValues$marker$mz[1],
                                                         intensity = internalValues$plotArgs$spectrum[match(internalValues$marker$mz[1],internalValues$plotArgs$spectrum[,1]),2],
                                                         color =  "#FFAB3680",
                                                         stringsAsFactors = F)
        
        # points(internalValues$marker$mz,
        #        internalValues$marker$intensity,
        #        bty = "n", type = "h", lwd = 5, col = "#FFAB3680")
        
      }
     
      do.call(specplot2,internalValues$plotArgs)
    
      par(xpd = FALSE)
      # if(!is.null(internalValues$hover)){
      #   
      #   points(internalValues$hover$mz,
      #          internalValues$hover$intensity,
      #          bty = "n", type = "h", lwd = 5, col = "#00FF0080")
      #   
      # }
      
      
      
      
      
      if(!is.null(internalValues$highlights)){
        
        points(internalValues$highlights$mz,
               internalValues$highlights$intensity/(internalValues$ymax/100),
               bty = "n", type = "h", lwd = 5, col = "#6A88C380")
        
      }
      

    }
  })#, height = if(is.null(set()$layout$height)){550}else{set()$layout$height})
  
  

  observeEvent(input$Mspec_click,{

    if (length(keys()) >0 && keys() == 16) {
      internalValues$click <- input$Mspec_click

      internalValues$marker <- nearPoints(internalValues$data,
                                                 input$Mspec_click,
                                                 xvar = "mz",
                                                 yvar = "intensity",
                                                 threshold = 100,
                                                 maxpoints = 1)


    }

    ###TEMPORARY FIX FOR BROKEN DOUBLECLICK, Ctrl + click
    if (length(keys()) >0 && keys() == 17) {
      internalValues$dblclick <- input$Mspec_dblclick

      if (!is.null(input$Mspec_brush)) {
  
      internalValues$plotArgs$yrange <- NULL
      internalValues$plotArgs$xrange <-  c(input$Mspec_brush$xmin, input$Mspec_brush$xmax)

      } else {
        
        internalValues$plotArgs$yrange <- NULL
        internalValues$plotArgs$xrange <- NULL

      }
      
      #update accordingly:
      
      internalValues$hover <- NULL
      
      if(!is.null(internalValues$plotArgs$norm)){
        norm <- internalValues$plotArgs$norm
      }else if(!is.null(internalValues$plotArgs$xrange)
               && nrow(internalValues$plotArgs$spectrum) > 0){
        inrange <- internalValues$plotArgs$spectrum[internalValues$plotArgs$spectrum[,1] >= internalValues$plotArgs$xrange[1]
                                                    & internalValues$plotArgs$spectrum[,1] <= internalValues$plotArgs$xrange[2],2]
        
        norm <-  if(length(inrange)>0){max(inrange)/100}else{1}
        
      }else if(nrow(internalValues$plotArgs$spectrum) > 0){
        norm <-  max(internalValues$plotArgs$spectrum)/100
      }else{
        norm <- 1
      }
      
      if(length(norm >0) && norm != 0){internalValues$norm <- norm}else{internalValues$norm <- 1}
      
      
      internalValues$data$intensity <- internalValues$plotArgs$spectrum[,2]/internalValues$norm
      
      
    }


  })



  observeEvent(input$Mspec_hover,{
    if(is.null(input$Mspec_brush) && !is.null(internalValues$data)){

      internalValues$hover <- nearPoints(internalValues$data,
                                                input$Mspec_hover,
                                                xvar = "mz",
                                                yvar = "intensity",
                                                threshold = 100,
                                                maxpoints = 1)
    }

  })
  
  # observeEvent(input$Mspec_brush, {
  #   
  #   print(paste("x",input$Mspec_click$x, "y",input$Mspec_click$y))
  #   
  # })

  observeEvent(input$Mspec_dblclick, {internalValues$dblclick <- input$Mspec_dblclick
  
  if (!is.null(input$Mspec_brush)) {
    
    internalValues$plotArgs$yrange <- NULL
    internalValues$plotArgs$xrange <-  c(input$Mspec_brush$xmin, input$Mspec_brush$xmax)
    
  } else {
    
    internalValues$plotArgs$yrange <- NULL
    internalValues$plotArgs$xrange <- NULL
    
  }
  
  #update accordingly:
  
  internalValues$hover <- NULL
  
  if(!is.null(internalValues$plotArgs$norm)){
    norm <- internalValues$plotArgs$norm
  }else if(!is.null(internalValues$plotArgs$xrange)
           && nrow(internalValues$plotArgs$spectrum) > 0){
    inrange <- internalValues$plotArgs$spectrum[internalValues$plotArgs$spectrum[,1] >= internalValues$plotArgs$xrange[1]
                                                & internalValues$plotArgs$spectrum[,1] <= internalValues$plotArgs$xrange[2],2]
    
    norm <-  if(length(inrange)>0){max(inrange)/100}else{1}
    
  }else if(nrow(internalValues$plotArgs$spectrum) > 0){
    norm <-  max(internalValues$plotArgs$spectrum)/100
  }else{
    norm <- 1
  }
  
  if(length(norm >0) && norm != 0){internalValues$norm <- norm}else{internalValues$norm <- 1}
  
  
  internalValues$data$intensity <- internalValues$plotArgs$spectrum[,2]/internalValues$norm
    })
  
  
  return(internalValues)
}



#' @describeIn SpecplotWidget UI function for SpecplotWidget
#' @export
SpecplotWidgetUI <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("specAll"))
  
  
}