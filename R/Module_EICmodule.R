#' EICmodule
#' 
#' server module for interactive EIC view
#' 
#' @inherit MseekModules
#' 
#' @details 
#' Returns its internalValues, a \code{reactivevalues} object
#' 
#' @describeIn EICmodule server logic for the EICmodule
#' @export 
EICmodule <- function(input, output, session, 
                      values){
  
  ####Initialization####
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(controls = reactiveValues(xrange = NULL,
                                                             yrange = NULL,
                                                             maxxrange = NULL,
                                                             maxyrange = NULL,
                                                             marker = NULL,
                                                             mz = NULL,
                                                             tic = F,
                                                             rtcorrSet = F,
                                                             hotlink = T,
                                                             nearpoints = NULL),
                                   files = reactiveValues(choices = NULL,
                                                          selected = NULL),
                                   EICs = NULL,
                                   recordedPlot = NULL,
                                   active = T,
                                   removable = F
                                   
  )
  
  
  ####External updates####
  observeEvent(c(values$MSData$layouts[[values$MSData$active]]$grouping, internalValues$active),{
    
    if(internalValues$active  && !is.null(values$MSData$layouts[[values$MSData$active]]$grouping)){
      
      #avoid issues with single file in a group:
      
      tempchoices <- lapply(values$MSData$layouts[[values$MSData$active]]$grouping,basename)
      
      for(i in seq(length(tempchoices))){
        names(tempchoices[[i]]) <- tempchoices[[i]]
      }
      
      internalValues$files$choices <- tempchoices
      if(is.null(internalValues$files$selected)){
        internalValues$files$selected <- internalValues$files$choices[[1]]
      }
    }
  })
  
  observeEvent(c(values$featureTables$Maintable$selected_rows),{
    
    if((is.null(input$hotl) || input$hotl)  && !is.null(values$featureTables$Maintable$selected_rows)){
      internalValues$controls$mz <- values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]
      
      internalValues$controls$xrange <- c(max(0,values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"rt"] - values$GlobalOpts$RTwindow),
                                          values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"rt"] + values$GlobalOpts$RTwindow)/60
      
    }
  })
  
  
  
  #### UI Elements and their single observers ----
  
  
  #### ** MZ selection input####
  output$mzin <- renderUI({
    if(internalValues$active){
      numericInput(ns('selMZ'), "m/z", 
                   value = internalValues$controls$mz,#NULL,#internalValues$controls$mz,
                   min = 0, 
                   step = 0.01)
    }
  })
  
  observeEvent(input$selMZ,{
    if(internalValues$active && !input$hotl){
      internalValues$controls$mz <-  input$selMZ
      # values$MSData$selectedFeats$mz <-  input$selMZ
    }
  })
  
  #### ** REMOVE BUTTON ####
  output$remover <- renderUI({
    if(internalValues$active && internalValues$removable){
      actionButton(ns('removeMe'), "Remove")
    }
  })
  
  observeEvent(input$removeMe,{
    if(internalValues$active && !is.null(input$removeMe)){
      internalValues$active <- FALSE
    }
  })
  
  
  #### ** TIC checkbox ####
  output$tic <- renderUI({
    if(internalValues$active){
      checkboxInput(ns('EicTic'), "TIC",
                    value = internalValues$controls$tic)
    }
  })
  observeEvent(input$EicTic,{
    if(internalValues$active){
      internalValues$controls$tic <-  input$EicTic
    }
  })
  
  
  #### ** RTcorr checkbox####
  output$rtcorr <- renderUI({
    if(internalValues$active){
      checkboxInput(ns('interactiveRTcorr'), "RT correction", value = internalValues$controls$rtcorrSet)
    }
  })
  
  observeEvent(input$interactiveRTcorr,{
    if(internalValues$active){
      internalValues$controls$rtcorrSet <-  input$interactiveRTcorr
    }
  })
  
  #### ** Hotlink checkbox####
  output$hotlink <- renderUI({
    if(internalValues$active){
      checkboxInput(ns('hotl'), "Hotlink mz and rt ranges",
                    value = internalValues$controls$hotlink)
    }
  })
  
  observeEvent(input$hotl,{
    if(internalValues$active){
      internalValues$controls$hotlink <-  input$hotl
    }
  })
  
  ### ** EIC selection####
  output$EICsel <- renderUI({
    selectizeInput(ns('selEICs'),
                   "select files", 
                   choices = internalValues$files$choices,
                   selected = internalValues$files$selected,
                   multiple = TRUE,
                   options = list(maxOptions = 10000))
  })
  
  output$pdfButton <- downloadHandler(filename= function(){
    titleout <- "EIC"
    
    return(paste0(titleout,".pdf"))}, 
    content = function(file){
      
      pdf(file,
          14,6
      )
      
    
            if(!is.null(internalValues$recordedPlot)){
              replayPlot(internalValues$recordedPlot)
            }
       
      #replayPlot(selections$plots$spec$fullplot)
      dev.off()
      
    },
    contentType = "application/pdf")
  
  
  observeEvent(input$selEICs,{
    if(internalValues$active){
      internalValues$files$selected <- input$selEICs
    }
  })
  
  
  
  #### ** Plotting####
  
  output$plainplot <- renderPlot({
    if(internalValues$active && !is.null(internalValues$EICs)){
      
      EICplot(EICs = internalValues$EICs, cx = 1,#input$plotCx, 
              ylim = if(!is.null(internalValues$controls$yrange)){internalValues$controls$yrange}else{internalValues$controls$maxyrange}, 
              xlim = if(!is.null(internalValues$controls$xrange)){internalValues$controls$xrange}else{internalValues$controls$maxxrange},
              legendtext = paste(sub("^([^.]*).*", "\\1",basename(row.names(internalValues$EICs[[1]])))),
              colr = do.call(values$GlobalOpts$colorscheme, list(n = nrow(internalValues$EICs[[1]]), alpha=0.8)),
              heading = if(input$EicTic){"TICs"}else{paste0("EIC for m/z ", round(input$selMZ,5), " +/- ",round(values$GlobalOpts$PPMwindow,1), " ppm")},
              relto = NULL,
              TIC = input$EicTic,
              single = T,
              midline = if(length(internalValues$controls$marker$rt)!=0){internalValues$controls$marker$rt*60}else{NULL},
              lw = 2#input$plotLw
      )
      
      if(!is.null(internalValues$controls$hover)){
        
        points(internalValues$controls$hover$rt,
               internalValues$controls$hover$intensity, cex =2)
        
      }
      
      if(!is.null(internalValues$controls$marker)){
        
        points(internalValues$controls$marker$rt,
               internalValues$controls$marker$intensity,
               col = "red", cex =2)
        
      }    
      internalValues$recordedPlot <- recordPlot()
    }
    
    
    
  })
  
  observeEvent(input$plainplot_dblclick,{
    if(internalValues$active){
      internalValues$controls$dblclick <- input$plainplot_dblclick
    }
  })  
  
  observeEvent(input$plainplot_click,{
    #print("click")
    if(internalValues$active){
      if (values$GlobalOpts$keyinput.keydown == 16) {
        internalValues$controls$click <- input$plainplot_click
        internalValues$controls$marker <- nearPoints(internalValues$controls$nearpoints,
                                                     input$plainplot_click,
                                                     xvar = "rt",
                                                     yvar = "intensity",
                                                     threshold = 20,
                                                     maxpoints = 1)
        
      }
      
      
      ###TEMPORARY FIX FOR BROKEN DOUBLECLICK, Ctrl + click
      
      if (values$GlobalOpts$keyinput.keydown == 17) {
        if (!is.null(input$plainplot_brush)) {
          
          
          internalValues$controls$xrange <- c(input$plainplot_brush$xmin, input$plainplot_brush$xmax)
          
          internalValues$controls$yrange <- c(input$plainplot_brush$ymin, min(max(internalValues$controls$nearpoints$intensity[which(internalValues$controls$nearpoints$rt >= input$plainplot_brush$xmin
                                                                                                                                     & internalValues$controls$nearpoints$rt <= input$plainplot_brush$xmax)]),
                                                                              input$plainplot_brush$ymax))
          
        } else {
          internalValues$controls$xrange <- internalValues$controls$maxxrange
          internalValues$controls$yrange <- internalValues$controls$maxyrange
        }
        
      }
    }
  }
  )
  
  
  observeEvent(input$plainplot_hover,{
    if(internalValues$active && is.null(input$plainplot_brush) && !is.null(internalValues$controls$nearpoints)){
      internalValues$controls$hover <- nearPoints(internalValues$controls$nearpoints,
                                                  input$plainplot_hover,
                                                  xvar = "rt",
                                                  yvar = "intensity",
                                                  threshold = 20,
                                                  maxpoints = 1)
      
    }
    
  })
  
  observeEvent(input$plainplot_dblclick, {
    if(internalValues$active){
      
      if (!is.null(input$plainplot_brush)) {
        
        
        internalValues$controls$xrange <- c(input$plainplot_brush$xmin, input$plainplot_brush$xmax)
        
        internalValues$controls$yrange <- c(input$plainplot_brush$ymin, min(max(internalValues$controls$nearpoints$intensity[which(internalValues$controls$nearpoints$rt >= input$plainplot_brush$xmin
                                                                                                                                   & internalValues$controls$nearpoints$rt <= input$plainplot_brush$xmax)]),
                                                                            input$plainplot_brush$ymax))
        
      } else {
        internalValues$controls$xrange <- internalValues$controls$maxxrange
        internalValues$controls$yrange <- internalValues$controls$maxyrange
      }}
  }
  )
  
  
  
  #### ** Below plot####  
  
  output$chrominfo <- renderUI({ 
    if(internalValues$active){
      p(if(!is.null(internalValues$controls$hover$file)){
        paste0("Cursor on: ", basename(internalValues$controls$hover$file[1]),
               " @ ", round(as.numeric(internalValues$controls$hover$rt[1]), 3),
               " min / ", round(as.numeric(internalValues$controls$hover$rt[1]*60), 2), " sec")}
        else{""},
        if(!is.null(internalValues$controls$marker$file)){
          paste0("Marker on: ", basename(internalValues$controls$marker$file[1]),
                 " @ ", round(as.numeric(internalValues$controls$marker$rt[1]), 3),
                 " min / ", round(as.numeric(internalValues$controls$marker$rt[1]*60), 2), " sec")}
        else{""}
      )
    }
  })
  
  #### Multi observers ####
  
  observeEvent(c(input$hotl, input$selMZ),{
    toggleState(id = "selMZ", condition = if(is.null(input$hotl)){F}else{!input$hotl})
  })
  
  observeEvent(c(internalValues$controls$mz, internalValues$files$selected, internalValues$active, values$GlobalOpts$PPMwindow, values$MSData$data),{ 
    if(internalValues$active && length(internalValues$files$selected)>0 && !is.null(input$EicTic)){
      
      internalValues$EICs <- multiEICplus(adducts = c(0),
                                          mz = data.frame(mzmin = max(1,internalValues$controls$mz-internalValues$controls$mz*values$GlobalOpts$PPMwindow*1e-6),
                                                          mzmax = max(1,internalValues$controls$mz+internalValues$controls$mz*values$GlobalOpts$PPMwindow*1e-6)),
                                          rt = NULL, #data.frame(rtmin = min(internalValues$controls$xrange)*60, rtmax = min(internalValues$controls$xrange)*60),#  NULL,#data.frame(rtmin = FT$df$rt[1]-5, rtmax= FT$df$rt[1]+5),
                                          rnames = 1:length(internalValues$controls$mz),
                                          rawdata= values$MSData$data[which(basename(names(values$MSData$data)) %in% internalValues$files$selected)],
                                          RTcorr = if(is.null(input$interactiveRTcorr) || !input$interactiveRTcorr){NULL}else{values$MSData$RTcorr}
      )
      
      internalValues$controls$maxxrange <- range(sapply(lapply(values$MSData$data[which(basename(names(values$MSData$data)) %in% internalValues$files$selected)], slot, "scantime"),range))/60
      
      # print(names(internalValues$EICs[[1]][1,"rt"]))
      # print(unlist(mapply(rep,row.names(internalValues$EICs[[1]]), sapply(internalValues$EICs[[1]][,"scan"], length))))
      # 
      intens <- if(input$EicTic){
        unlist(internalValues$EICs[[1]][,"tic"])
      }else{
        unlist(internalValues$EICs[[1]][,"intensity"])
      }
      
      rts <- unlist(internalValues$EICs[[1]][,"rt"])
      
      internalValues$controls$nearpoints <- data.frame(rt = rts/60,
                                                       intensity = intens,
                                                       scan = unlist(internalValues$EICs[[1]][,"scan"]),
                                                       file = unname(unlist(mapply(rep,row.names(internalValues$EICs[[1]]), sapply(internalValues$EICs[[1]][,"scan"], length)))),
                                                       stringsAsFactors = F)
      
      
    }else{
      internalValues$EICs <- NULL
    }
  })
  
  observeEvent(c(internalValues$EICs, input$EicTic),{
    if(internalValues$active && length(input$selMZ)>0 && length(internalValues$EICs)>0){
      
      intens <- if(input$EicTic){
        unlist(internalValues$EICs[[1]][,"tic"])
      }else{
        unlist(internalValues$EICs[[1]][,"intensity"])
      }
      
      rts <- internalValues$controls$nearpoints$rt
      
      internalValues$controls$maxyrange <- c(0,max(intens))
      
      if(!is.null(internalValues$controls$xrange)){
        internalValues$controls$yrange <- c(0, max(intens[which(rts <= max(internalValues$controls$xrange)
                                                                & rts >= min(internalValues$controls$xrange))]  ))
      }
      
      internalValues$controls$nearpoints$intensity <- intens
      
    }
  })
  
  #### Compounded UI ####
  
  output$controls <- renderUI({ 
    if(internalValues$active){
      fluidRow(
        column(1,
               htmlOutput(ns("remover"))),
        column(2,
               downloadButton(ns("pdfButton"), label = "Download EIC")
               ),
        column(1,
               htmlOutput(ns('rtcorr')) 
        ),
        column(1,
               htmlOutput(ns('tic')) 
        ),
        column(2,
               htmlOutput(ns('hotlink')) 
        ),
        column(2,
               htmlOutput(ns('mzin')) 
               
        ),
        column(3,
               htmlOutput(ns('EICsel')) 
               
        ))
    }
  })
  
  output$EICout <- renderUI({
    if(internalValues$active){
      fluidPage(
        htmlOutput(ns("controls")),
        
        fluidRow(
          
          #p("Brush and double click to zoom in, double click to zoom out, press SHIFT and click to select a data point to view Spectrum. SHIFT + click in Spectrum generates EIC for selected peak."),
          # EICinteractiveUI("chrom1"),
          plotOutput(ns("plainplot"),
                     click = ns("plainplot_click"),
                     hover = hoverOpts(id = ns("plainplot_hover"),
                                       delay = 50),
                     dblclick = ns("plainplot_dblclick"),
                     brush = brushOpts(
                       id = ns("plainplot_brush"),
                       #direction = "x",
                       resetOnNew = TRUE)
          )
        ),
        fluidRow(htmlOutput(ns('chrominfo')))
      )
    }
  })
  
  return(internalValues)
}

#' @describeIn EICmodule UI elements for the EICmodule
#' @export 
EICmoduleUI <- function(id){
  ns <- NS(id)
  fluidRow(
    useShinyjs(),
    
    htmlOutput(ns('EICout'))
  )
}



