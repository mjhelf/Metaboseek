#' Specmodule
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param tag id to be used in ns()
#' @param set Import data from the shiny session
#' 
#' @export 
Specmodule <- function(input,output, session, tag, set = list(spec = list(xrange = NULL,
                                                                          yrange = NULL,
                                                                          maxxrange = NULL,
                                                                          maxyrange = NULL,
                                                                          sel = NULL,
                                                                          mz = NULL,
                                                                          data = NULL),
                                                              layout = list(lw = 1,
                                                                            cex = 1,
                                                                            controls = F,
                                                                            active = T),
                                                              msdata = NULL),
                                                    keys){
  
  ns <- NS(tag)
  selections <- reactiveValues(plots = list(spec = list(xrange = NULL, #the x axis range
                                                         yrange = NULL, #the y axis range
                                                         maxxrange = NULL, #maximum x axis range
                                                         maxyrange = NULL, #maximum y axis range
                                                         sel = NULL, # selected feature from set() (if used)
                                                        marker = NULL, #selected peak with $mz and $intensity
                                                        hover = NULL, #peak hovered over with $mz and $intensity
                                                        mz = NULL, 
                                                         data = NULL,
                                                         ymax = 100),
                                            set = NULL #copy of set() to check if set() has changed
  )
  )
  
 # observeEvent(set(),{sc()})
  sc <- reactive({
    # observe({
    if(set()$layout$active && !is.null(set()$spec$sel$File) && !identical(selections$plots$set,set()$spec )){

      
      
      #get the MS scan
      res <- getScan(set()$msdata[[set()$spec$sel$File]], set()$spec$sel$scan)
      #print("sc")
      #set the maximum x axis range to cover the spectrum data
      selections$plots$spec$maxxrange <- c(min(res[,1])-1,
                                                 max(res[,1])+1)
      
      #set the actual x-axis view range
      if(is.null(selections$plots$spec$xrange) || !identical(selections$plots$set,set()$spec )){
      selections$plots$spec$xrange <- set()$spec$xrange
      }
      
     # if(is.null(selections$plots$spec$yrange)){
      #selections$plots$spec$yrange <- set()$spec$yrange}
      
      
      #maximum y axis range
      selections$plots$spec$maxyrange <- c(0, 100)
      
      #save the spectrum so it can be accessed without another call to sc()
      selections$plots$spec$data <- res
      
      #find maximum Y axis value (absolute intensity) within visible x- axis range
      xr <- if(!is.null(selections$plots$spec$xrange)){selections$plots$spec$xrange}else{selections$plots$spec$maxxrange}
      selections$plots$spec$ymax <- max(selections$plots$spec$data[,2][which(selections$plots$spec$data[,1]>= min(xr) 
                                                                                         & selections$plots$spec$data[,1]<= max(xr))])
      
      if(is.null(selections$plots$spec$marker) || !identical(selections$plots$set,set()$spec )){
        vispoints <- (selections$plots$spec$data[which(selections$plots$spec$data[,1]>= min(xr) 
                                                       & selections$plots$spec$data[,1]<= max(xr)),])
        
        if(min(abs(vispoints[,1] - set()$spec$mz)) <=  set()$spec$mz*5e-6 ){
                selections$plots$spec$marker <- data.frame(mz = vispoints[which.min(abs(vispoints[,1] - set()$spec$mz)),1],
                                                                 intensity = (vispoints[which.min(abs(vispoints[,1] - set()$spec$mz)),2]/selections$plots$spec$ymax)*100
                                                                 )
        }else{
          selections$plots$spec$marker <- NULL
        }
      }
      #print(identical(selections$plots$set,set()$spec ))
     # print(selections$plots$spec$marker)
      selections$plots$set <- set()$spec
      
      return(res)
      
    }
  })
  
  output$specinfo <- renderUI({ 
    if(set()$layout$active && !is.null(set()$spec$sel$File)){
      
      p(paste0("Marker on: ", round(as.numeric(selections$plots$spec$marker), 5),
        ", Cursor on: ", round(as.numeric(selections$plots$spec$hover$mz),5),
        if(length(selections$plots$spec$hover$mz) !=0 && length(selections$plots$spec$marker) != 0){paste0(" (", 
        if(as.numeric(selections$plots$spec$hover$mz) > as.numeric(selections$plots$spec$marker$mz)){"+"}else{""},
        round(as.numeric(selections$plots$spec$hover$mz) - as.numeric(selections$plots$spec$marker),5), ")")}else{""}  ))
      
    }
    
    
      })
  
  
  output$specAll <- renderUI({
    if(set()$layout$active){
    fluidPage(
      fluidRow(
        plotOutput(ns("Mspec"),
                   click = ns("Mspec_click"),
                   hover = hoverOpts(id = ns("Mspec_hover"),
                                     delay = 150),
                   dblclick = ns("Mspec_dblclick"),
                   brush = brushOpts(
                     id = ns("Mspec_brush"),
                     #direction = "x",
                     resetOnNew = TRUE),
                   height = "550px"
        ),
        htmlOutput(ns("specinfo"))
        
      )
    )
    }
  })
  
  
  output$Mspec <- renderPlot({
    if(set()$layout$active && !is.null(set()$spec$sel$File)){
      sc()
      
      xr <- if(!is.null(selections$plots$spec$xrange)){selections$plots$spec$xrange}else{selections$plots$spec$maxxrange}
      
      #intensity values in the current x-axis range view 
      yinview <- selections$plots$spec$data[,2][which(selections$plots$spec$data[,1]>= min(xr) 
                                                          & selections$plots$spec$data[,1]<= max(xr))]
      
      specplot(x=selections$plots$spec$data[,1],
               y=selections$plots$spec$data[,2],
               norm=selections$plots$spec$ymax/100,
               cx=set()$layout$cex/1.5,
               k = 20,
               fileName = paste0(basename(set()$spec$sel$File), "#", set()$spec$sel$scan,
                                 " (", round(as.numeric(set()$spec$sel$rt)/60,3), " min / ", round(as.numeric(set()$spec$sel$rt),1), " sec)"),
               yrange = if(!is.null(selections$plots$spec$yrange)){selections$plots$spec$yrange}else{selections$plots$spec$maxyrange},
               xrange = if(!is.null(selections$plots$spec$xrange)){selections$plots$spec$xrange}else{selections$plots$spec$maxxrange},
               maxi = selections$plots$spec$ymax
      ) 
      
      
      if(!is.null(selections$plots$spec$hover)){
        
        points(selections$plots$spec$hover$mz,
               selections$plots$spec$hover$intensity,
               bty = "n", type = "h", lwd = 5, col = "#00FF0080")
        
      }
      
      if(!is.null(selections$plots$spec$marker)){
        
        points(selections$plots$spec$marker$mz,
               selections$plots$spec$marker$intensity,
               bty = "n", type = "h", lwd = 5, col = "#FFAB3680")
        
      }
      
      
      
    }
  }, height = 550)
  
  
  
  observeEvent(input$Mspec_dblclick,{
    selections$plots$spec$dblclick <- input$Mspec_dblclick
    #selections$lastChangedEIC <- "spec1"
  })  
  
  observeEvent(input$Mspec_click,{
    if (keys() == 16) {
      #print("MSmarker")
      selections$plots$spec$click <- input$Mspec_click
      
      temp <- as.data.frame(selections$plots$spec$data)
      temp$intensity <- temp$intensity/(selections$plots$spec$ymax/100)
      
     # selections$plots$spec$marker <- temp[which.min(abs(temp$mz - input$Mspec_click$x)),]
      selections$plots$spec$marker <- nearPoints(temp,
                                                input$Mspec_click,
                                                xvar = "mz",
                                                yvar = "intensity",
                                                threshold = 100,
                                                maxpoints = 1)
      
      # datadf
      
      #selections$lastChangedEIC <- "spec1"
    }
  })
  
  
  
  observeEvent(input$Mspec_hover,{
    if(is.null(input$Mspec_brush)){
      #print(input$Mspec_hover)
      temp <- as.data.frame(selections$plots$spec$data)
      temp$intensity <- temp$intensity/(selections$plots$spec$ymax/100)
      
      #selections$plots$spec$hover <- temp[which.min(abs(temp$mz - input$Mspec_hover$x)),]
      
      selections$plots$spec$hover <- nearPoints(temp,
                                                      input$Mspec_hover,
                                                      xvar = "mz",
                                                      yvar = "intensity",
                                                      threshold = 100,
                                                      maxpoints = 1)
     # print(selections$plots$spec$hover)
    }
    
  })
  
  observeEvent(input$Mspec_dblclick, {
    
       ymax_old <- selections$plots$spec$ymax

    if (!is.null(input$Mspec_brush)) {
     
     
      
      
      selections$plots$spec$xrange <- c(input$Mspec_brush$xmin, input$Mspec_brush$xmax)

      #intensity values in the current x-axis range view 
      xr <- if(!is.null(selections$plots$spec$xrange)){selections$plots$spec$xrange}else{selections$plots$spec$maxxrange}
      
      #intensity values in the current x-axis range view - avoiding div by 0
      selections$plots$spec$ymax <- max(c(1,selections$plots$spec$data[,2][which(selections$plots$spec$data[,1]>= min(xr) 
                                                            & selections$plots$spec$data[,1]<= max(xr))]))
      
      
      
      selections$plots$spec$yrange <- c(0, min(c(100,input$Mspec_brush$ymax*(ymax_old/selections$plots$spec$ymax))))
      if(!is.null(selections$plots$spec$marker)){
      selections$plots$spec$marker$intensity <-min(c(100,selections$plots$spec$marker$intensity*(ymax_old/selections$plots$spec$ymax)))
      }
    } else {
      selections$plots$spec$xrange <- selections$plots$spec$maxxrange
      selections$plots$spec$yrange <- selections$plots$spec$maxyrange
      
      #intensity values in the current x-axis range view - avoiding div by 0
      selections$plots$spec$ymax <- max(c(1,selections$plots$spec$data[,2]))
      
      if(!is.null(selections$plots$spec$marker)){
        selections$plots$spec$marker$intensity <-min(c(100,selections$plots$spec$marker$intensity*(ymax_old/selections$plots$spec$ymax)))
      }
      
      
    }})
  
  return(
    reactive({
    selections$plots
      })
  )
  }






#' EICmodule
#' 
#' 
#' server module for interactive EIC view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param tag id to be used in ns()
#' @param set Import data from the shiny session
#' 
#' @export 
EICmodule <- function(input, output, session, tag, set, keys){
  
  
  ns <- NS(tag)
  selectionsEIC <- reactiveValues(plots = list(chrom1 = list(xrange = NULL,
                                                             yrange = NULL,
                                                             maxxrange = NULL,
                                                             maxyrange = NULL,
                                                             marker = NULL,
                                                             mz = NULL,
                                                             tic = T)
  )
  )
  
  # observe({selectionsEIC$plots[["chrom1"]]$mz <- set()$mz})
  
  
  output$controls <- renderUI({ 
    
    fluidRow(
      column(2,
             
             
             checkboxInput(ns('interactiveRTcorr'), "RT correction", value = F)
             
             
      ),
      column(2,
             checkboxInput(ns('EicTic'), "TIC",
                           value = T)
      ),
      column(3,
             selectizeInput(ns('selEICs'),
                            "select files", 
                            choices = lapply(set()$layouts[[set()$active]]$grouping,basename),
                            selected = lapply(set()$layouts[[set()$active]]$grouping,basename)[[1]],
                            multiple = TRUE)  
             
      ),
      column(3,
             numericInput(ns('selMZ'), "m/z", 
                          value = set()$mz,#NULL,#selectionsEIC$plots[["chrom1"]]$mz,
                          min = 0, 
                          step = 0.01)   
      ))
    
  })
  
  
  #observeEvent(input$selEICs,{
  #initialize sEICs
  # sEICs()
  
  #})
  
  observeEvent(input$EicTic,{
    selectionsEIC$plots[["chrom1"]]$tic <-  input$EicTic
    #  sEICs()
  })
  
  observeEvent(input$selMZ,{
  # print("mz changed")
  selectionsEIC$plots[["chrom1"]]$mz <-  input$selMZ
  #sEICs()
  })
  
  #observeEvent(selectionsEIC$plots$spec$marker$mz,{
  # selectionsEIC$plots[["chrom1"]]$mz <-  selectionsEIC$plots$spec$marker$mz
  #selectionsEIC$plots[["chrom1"]]$tic <-  F
  
  #})
  
  sEICs <- reactive ({
    if(length(input$selMZ)>0){
      #print(input$selMZ)
      r1 <- multiEICplus(adducts = c(0),
                         mz = data.frame(mzmin = max(1,input$selMZ-input$selMZ*set()$layouts[[set()$active]]$settings$ppm*1e-6),
                                         mzmax=max(1,input$selMZ+input$selMZ*set()$layouts[[set()$active]]$settings$ppm*1e-6)),
                         rt = NULL,#data.frame(rtmin = FT$df$rt[1]-5, rtmax= FT$df$rt[1]+5),
                         rnames = 1:length(input$selMZ),
                         rawdata= set()$data,
                         RTcorr = if(is.null(input$interactiveRTcorr) || !input$interactiveRTcorr){NULL}else{set()$RTcorr}
      )
      
      
      res <- subsetEICs(r1, set()$filelist[which(basename(set()$filelist) %in% input$selEICs)])
      
      selectionsEIC$plots[['chrom1']]$maxxrange <- c(min(unlist(res$EIClist[[1]][,'rt'])),
                                                     max(unlist(res$EIClist[[1]][,'rt'])))/60
      
      selectionsEIC$plots[['chrom1']]$maxyrange <- if(input$EicTic){c(0,res$maxTIC)}else{c(0,res$maxEIC)}
      #print("sEICs")
      
      return (res)
    }
  })
  
  #neccessary to run shiny nearPoints to determine data point nearest to pointer
  sEICsDF <- reactive({
    
    f <- as.data.frame(sEICs()$EIClist[[1]][1,], stringsAsFactors = F)
    f$File <- row.names(sEICs()$EIClist[[1]])[1]
    if(nrow(sEICs()$EIClist[[1]])>1){
      for( r in 2:nrow(sEICs()$EIClist[[1]])){
        f2 <- as.data.frame(sEICs()$EIClist[[1]][r,], stringsAsFactors = F)
        f2$File <- row.names(sEICs()$EIClist[[1]])[r]
        f <- rbind(f,f2)
      }
    }
    
    f$rt <- f$rt/60
    #print("sEICsDF")
    return(f)
    
  })
  
  output$plainplot <- renderPlot({
    if(!is.null(sEICs())){
      #sEICs()
      # pt1<- proc.time()
      
      EICplot(EICs = sEICs()$EIClist, cx = 1,#input$plotCx, 
              ylim = if(!is.null(selectionsEIC$plots[['chrom1']]$yrange)){selectionsEIC$plots[['chrom1']]$yrange}else{selectionsEIC$plots[['chrom1']]$maxyrange}, 
              xlim = if(!is.null(selectionsEIC$plots[['chrom1']]$xrange)){selectionsEIC$plots[['chrom1']]$xrange}else{selectionsEIC$plots[['chrom1']]$maxxrange},
              legendtext = paste(sub("^([^.]*).*", "\\1",basename(row.names(sEICs()$EIClist[[1]])))),
              colr = do.call(set()$layouts[[set()$active]]$settings$colr, list(n = nrow(sEICs()$EIClist[[1]]), alpha=0.8)),
              heading = if(input$EicTic){"TICs"}else{paste0("EIC for m/z ", round(input$selMZ,5), " +/- ",round(set()$layouts[[set()$active]]$settings$ppm,1), " ppm")},
              relto = NULL,
              TIC = input$EicTic,
              single = T,
              midline = if(length(selectionsEIC$plots[["chrom1"]]$marker$rt)!=0){selectionsEIC$plots[["chrom1"]]$marker$rt*60}else{NULL},
              lw = 2#input$plotLw
      )
      
      if(!is.null(selectionsEIC$plots[["chrom1"]]$hover)){
        if(input$EicTic){
          points(selectionsEIC$plots[["chrom1"]]$hover$rt,
                 selectionsEIC$plots[["chrom1"]]$hover$tic)
        }else{
          points(selectionsEIC$plots[["chrom1"]]$hover$rt,
                 selectionsEIC$plots[["chrom1"]]$hover$intensity)
        }
      }
      
      if(!is.null(selectionsEIC$plots[["chrom1"]]$marker)){
        if(input$EicTic){
          points(selectionsEIC$plots[["chrom1"]]$marker$rt,
                 selectionsEIC$plots[["chrom1"]]$marker$tic,
                 col = "red")
        }else{
          points(selectionsEIC$plots[["chrom1"]]$marker$rt,
                 selectionsEIC$plots[["chrom1"]]$marker$intensity,
                 col = "red")
        }
      }
    }
    
    
    
    #pt1 <- proc.time() - pt1
    #print(pt1)
    
    
  })
  
  observeEvent(input$plainplot_dblclick,{
    selectionsEIC$plots[["chrom1"]]$dblclick <- input$plainplot_dblclick
    selectionsEIC$lastChangedEIC <- "chrom1"
  })  
  
  observeEvent(input$plainplot_click,{
    #print("click")
    
    if (keys() == 16) {
      
      selectionsEIC$plots[["chrom1"]]$click <- input$plainplot_click
      selectionsEIC$plots[["chrom1"]]$marker <- nearPoints(sEICsDF(),
                                                           input$plainplot_click,
                                                           xvar = "rt",
                                                           yvar = if(input$EicTic){"tic"}else{"intensity"},
                                                           threshold = 20,
                                                           maxpoints = 1)
      
      selectionsEIC$lastChangedEIC <- "chrom1"
      
      #initiate sc()
      #sc()
      
    }
  })
  
  
  observeEvent(input$plainplot_hover,{
    if(is.null(input$plainplot_brush)){
      selectionsEIC$plots[["chrom1"]]$hover <- nearPoints(sEICsDF(),
                                                          input$plainplot_hover,
                                                          xvar = "rt",
                                                          yvar = if(input$EicTic){"tic"}else{"intensity"},
                                                          threshold = 20,
                                                          maxpoints = 1)
    }
    
  })
  
  observeEvent(input$plainplot_dblclick, {
    
    if (!is.null(input$plainplot_brush)) {
      #print("dblcklick plus brushing")
      selectionsEIC$plots[['chrom1']]$xrange <- c(input$plainplot_brush$xmin, input$plainplot_brush$xmax)
      selectionsEIC$plots[['chrom1']]$yrange <- c(input$plainplot_brush$ymin, input$plainplot_brush$ymax)
      
    } else {
     # print("dblcklick")
      
      selectionsEIC$plots[['chrom1']]$xrange <- selectionsEIC$plots[['chrom1']]$maxxrange
      selectionsEIC$plots[['chrom1']]$yrange <- selectionsEIC$plots[['chrom1']]$maxyrange
    }})
  
  return(reactive({
    
    selectionsEIC$plots
    
    
  })
  )
  
  
  
  
}

#' SpecmoduleUI
#' 
#' 
#' UI module for interactive spectrum view
#' 
#' @param id id to be used in ns()
#' 
#' @export
SpecmoduleUI <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("specAll"))
  
  
}


#' EICmoduleUI
#' 
#' 
#' UI module for interactive EIC view
#' 
#' @param id id to be used in ns()
#' 
#' @export 
EICmoduleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns("controls")),
    
    fluidRow(
      
      p("Brush and double click to zoom in, double click to zoom out, press SHIFT and click to select a data point to view Spectrum. SHIFT + click in Spectrum generates EIC for selected peak."),
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
    )
  )
  
}