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
#' @importFrom xcms getMsnScan
#' @importFrom xcms getScan
#' @export 
Specmodule <- function(input,output, session, tag, set = list(spec = list(xrange = NULL,
                                                                          yrange = NULL,
                                                                          maxxrange = NULL,
                                                                          maxyrange = NULL,
                                                                          sel = NULL,
                                                                          mz = NULL,
                                                                          data = NULL,
                                                                          MS2 = T),
                                                              layout = list(lw = 1,
                                                                            cex = 1,
                                                                            controls = F,
                                                                            ppm = 5,
                                                                            active = T,
                                                                            highlights = NULL,
                                                                            height = 550),
                                                              msdata = NULL),
                       keys){
  
  ns <- NS(tag)
  selections <- reactiveValues(plots = list(spec = list(xrange = NULL, #the x axis range
                                                        yrange = NULL, #the y axis range
                                                        maxxrange = NULL, #maximum x axis range
                                                        maxyrange = NULL, #maximum y axis range
                                                        sel = NULL, # selected feature from set() (if used)
                                                        marker = NULL, #selected peak with $mz and $intensity
                                                        highlights = NULL, #peaks to be highlighted with $mz and $intensity
                                                        hover = NULL, #peak hovered over with $mz and $intensity
                                                        mz = NULL, 
                                                        data = NULL,
                                                        ymax = 100,
                                                        MSmerge = NULL,
                                                        fullplot = NULL),
                                            set = NULL #copy of set() to check if set() has changed
  )
  )
  
  # observeEvent(set(),{sc()})
  sc <- reactive({
    # observe({
    if(set()$layout$active && !is.null(set()$spec$sel$File) && !identical(selections$plots$set, set()$spec )){
      
      #select file based on basename rather than full path
      datasel <- which(basename(set()$spec$sel$File)  %in% basename(names(set()$msdata)))
      
      
      if(length(datasel) > 0){
        
        if(length(set()$spec$sel$File) == 1){
          #get the MS scan
          
          filesel <- which(basename(names(set()$msdata)) == basename(set()$spec$sel$File[datasel]))
          
          #make sure signal to other functions that the spectrum is NOT a merge product
          selections$plots$spec$MSmerge <- NULL
          
          if(!is.null(set()$spec$MS2) && set()$spec$MS2){
            res <- getMsnScan(set()$msdata[[filesel]], set()$spec$sel$scan[datasel])
          }else{
            res <- getScan(set()$msdata[[filesel]], set()$spec$sel$scan[datasel])
          }
        }
        #if there is input information on more than one scan, and more than one file matches the input
        else{
          
          fx <- function(MSfile, scan, MS2 = F){
            # print(MSfile@filepath)
            tryCatch({
              if(MS2){
                return(getMsnScan(MSfile, scan))
              }else{
                return(getScan(MSfile, scan))
              }
            },
            error = function(e){NULL})
            
          }
          
          speclist <- mapply(fx,
                             MSfile = xcmsRaws[match(basename(set()$spec$sel$File[datasel]),basename(names(set()$msdata)))],
                             scan = set()$spec$sel$scan[datasel],
                             MoreArgs = list(MS2 = (!is.null(set()$spec$MS2) && set()$spec$MS2)))
          
          #remove NULL results (from errors)
          speclist <- speclist[which(!sapply(speclist, is.null))]
          
          #make sure signal to other functions that the spectrum IS a merge product
          if(length(speclist) > 1){
            selections$plots$spec$MSmerge <- mergeMS(speclist)
            
            res <- selections$plots$spec$MSmerge$merged}
          else{
            selections$plots$spec$MSmerge <- NULL
          }
          
        }          
        #print("sc")
        #set the maximum x axis range to cover the spectrum data
        if(!is.null(set()$spec$maxxrange) && !identical(selections$plots$set$maxxrange,set()$spec$maxxrange)){
          selections$plots$spec$maxxrange <- set()$spec$maxxrange
        }else{
          selections$plots$spec$maxxrange <- c(min(res[,1])-1,
                                               max(res[,1])+1)
        }
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
          
          #set marker to peak closest to requested mz if within set ppm window
          if(!is.na(set()$spec$mz) && !is.null(set()$spec$mz) && min(abs(vispoints[,1] - set()$spec$mz)) <=  set()$spec$mz*set()$layout$ppm*1e-6 ){
            selections$plots$spec$marker <- data.frame(mz = vispoints[which.min(abs(vispoints[,1] - set()$spec$mz)),1],
                                                       intensity = (vispoints[which.min(abs(vispoints[,1] - set()$spec$mz)),2]/selections$plots$spec$ymax)*100
            )
          }else{
            selections$plots$spec$marker <- NULL
          }
        }
        #print(identical(selections$plots$set,set()$spec ))
        # print(selections$plots$spec$marker)
        
        if(length(set()$layout$highlights) > 0){
          selections$plots$spec$highlights <- set()$layout$highlights
        }else{
          selections$plots$spec$highlights <- NULL
          
        }
        
        selections$plots$set <- set()$spec
        
        return(res)
      }
    }
  })
  
  observeEvent(set(),{
    
    if(length(set()$layout$highlights) > 0){
      selections$plots$spec$highlights <- set()$layout$highlights
    }else{
      selections$plots$spec$highlights <- NULL
      
    }
    
  })
  
  output$specinfo <- renderUI({ 
    if(set()$layout$active && !is.null(set()$spec$sel$File)){
      
      p(paste0("Marker on: ", round(as.numeric(selections$plots$spec$marker[1]), 5),
               ", Cursor on: ", round(as.numeric(selections$plots$spec$hover$mz[1]),5),
               if(length(selections$plots$spec$hover$mz) !=0 && length(selections$plots$spec$marker) != 0){paste0(" (", 
                                                                                                                  if(as.numeric(selections$plots$spec$hover$mz) > as.numeric(selections$plots$spec$marker$mz)){"+"}else{""},
                                                                                                                  round(as.numeric(selections$plots$spec$hover$mz[1]) - as.numeric(selections$plots$spec$marker[1]),5), ")")}else{""}  ))
      
    }
    
    
  })
  
  
  output$specAll <- renderUI({
    if(length(set()$layout$active) > 0 && set()$layout$active){
      fluidPage(
        fluidRow(
          plotOutput(ns("Mspec"),
                     click = clickOpts(ns("Mspec_click"), clip = T),
                     hover = hoverOpts(id = ns("Mspec_hover"),
                                       delay = 150),
                     dblclick = dblclickOpts(id = ns("Mspec_dblclick"),clip = F, delay = 400),
                     brush = brushOpts(
                       id = ns("Mspec_brush"),
                       #direction = "x",
                       resetOnNew = TRUE),
                     height = if(is.null(set()$layout$height)){"550px"}else{paste0(set()$layout$height,"px")}
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
      
      #select file based on basename rather than full path
      filesel <- match(basename(set()$spec$sel$File), basename(names(set()$msdata)))
      
      label <- if(length(set()$spec$sel$File) >0){
        if(length(set()$spec$sel$File) == 1){
        paste0(basename(set()$spec$sel$File), "#", 
               if(!is.null(set()$spec$MS2) && set()$spec$MS2){
                 set()$msdata[[filesel]]@msnAcquisitionNum[set()$spec$sel$scan]
               }
               else{set()$msdata[[filesel]]@acquisitionNum[set()$spec$sel$scan]},
               " (", round(as.numeric(set()$spec$sel$rt)/60,3), " min / ", round(as.numeric(set()$spec$sel$rt),1), " sec)",
               if(!is.null(set()$spec$MS2) && set()$spec$MS2){
                 paste0("\nParent m/z:", round(as.numeric(set()$msdata[[filesel]]@msnPrecursorMz[set()$spec$sel$scan]),5))}else{""},
                 collapse = " ")}
      else{"merged spectra"}
      }else{""}
      
      
      
      specplot(x=selections$plots$spec$data[,1],
               y=selections$plots$spec$data[,2],
               norm=selections$plots$spec$ymax/100,
               cx=set()$layout$cex/1.5,
               k = 20,
               fileName = label,
               yrange = if(!is.null(selections$plots$spec$yrange)){selections$plots$spec$yrange}else{selections$plots$spec$maxyrange},
               xrange = if(!is.null(selections$plots$spec$xrange)){selections$plots$spec$xrange}else{selections$plots$spec$maxxrange},
               maxi = selections$plots$spec$ymax
      ) 
      
      par(xpd = FALSE)
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
      
      
      
      if(!is.null(selections$plots$spec$highlights)){
        
        points(selections$plots$spec$highlights$mz,
               selections$plots$spec$highlights$intensity/(selections$plots$spec$ymax/100),
               bty = "n", type = "h", lwd = 5, col = "#6A88C380")
        
      }
      
      selections$plots$spec$fullplot <- recordPlot()
      
    }
  })#, height = if(is.null(set()$layout$height)){550}else{set()$layout$height})
  
  
  
  # observeEvent(input$Mspec_dblclick,{
  #   #selections$lastChangedEIC <- "spec1"
  # })  
  
  observeEvent(input$Mspec_click,{
   #print("click!")
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
    
    ###TEMPORARY FIX FOR BROKEN DOUBLECLICK, Ctrl + click
    if (keys() == 17) {
    selections$plots$spec$dblclick <- input$Mspec_dblclick
    
    ymax_old <- selections$plots$spec$ymax
    
    if (!is.null(input$Mspec_brush)) {
      
      #print(input$Mspec_brush)
      
      
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
    
    }
    }
    
    
  })
  
  
  
  observeEvent(input$Mspec_hover,{
    #print("hover!")
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
   # print("doubleclick!")
    selections$plots$spec$dblclick <- input$Mspec_dblclick
    
    ymax_old <- selections$plots$spec$ymax
    
    if (!is.null(input$Mspec_brush)) {
      
      print(input$Mspec_brush)
      
      
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
EICmodule <- function(input, output, session, tag, set = list(layouts = MSData$layouts, #List of rawfile paths (unsorted)
                                                              RTcorr = NULL,
                                                              activeGroup = "Group1",
                                                              filelist = MSData$filelist,
                                                              data = MSData$data,
                                                              mz = 542.32405482,
                                                              rtr = NULL,
                                                              active = T),
                      keys){
  
  
  ns <- NS(tag)
  selectionsEIC <- reactiveValues(plots = list(chrom1 = list(xrange = NULL,
                                                             yrange = NULL,
                                                             maxxrange = NULL,
                                                             maxyrange = NULL,
                                                             marker = NULL,
                                                             mz = NULL,
                                                             tic = F,
                                                             rtcorrSet = F,
                                                             hotlink = T),
                                               files = list(choices = NULL,
                                                            selected = NULL)),
                                  set = NULL
                                  
  )
  
  observeEvent(c(set()),{
    if(set()$active && is.null(selectionsEIC$plots$files$choices) && !is.null(set()$layouts[[set()$activeGroup]]$grouping)){
      selectionsEIC$plots$files$choices <- lapply(set()$layouts[[set()$activeGroup]]$grouping,basename)
      selectionsEIC$plots$files$selected <- selectionsEIC$plots$files$choices[[1]]
      
    }
    
    
    
  })
  
  
  output$controls <- renderUI({ 
    if(set()$active){
      fluidRow(
        column(2,
               
               
               htmlOutput(ns('rtcorr')) 
               
               
        ),
        column(2,
               htmlOutput(ns('tic')) 
        ),
        column(2,
               htmlOutput(ns('hotlink')) 
        ),
        column(3,
               selectizeInput(ns('selEICs'),
                              "select files", 
                              choices = selectionsEIC$plots$files$choices,
                              selected = selectionsEIC$plots$files$selected,
                              multiple = TRUE)  
               
        ),
        column(3,
               htmlOutput(ns('mzin'))   
        ))
    }
  })
  
  output$mzin <- renderUI({
    if(set()$active){
      numericInput(ns('selMZ'), "m/z", 
                   value = selectionsEIC$plots[["chrom1"]]$mz,#NULL,#selectionsEIC$plots[["chrom1"]]$mz,
                   min = 0, 
                   step = 0.01)
    }
  })
  
  
  
  output$tic <- renderUI({
    if(set()$active){
      checkboxInput(ns('EicTic'), "TIC",
                    value = selectionsEIC$plots[["chrom1"]]$tic)
    }
  })
  observeEvent(input$EicTic,{
    if(set()$active){
      selectionsEIC$plots[["chrom1"]]$tic <-  input$EicTic
    }
    #  sEICs()
  })
  
  output$rtcorr <- renderUI({
    if(set()$active){
  checkboxInput(ns('interactiveRTcorr'), "RT correction", value = selectionsEIC$plots[["chrom1"]]$rtcorrSet)
    }
      })
  
  observeEvent(input$interactiveRTcorr,{
    if(set()$active){
      selectionsEIC$plots[["chrom1"]]$rtcorrSet <-  input$interactiveRTcorr
    }
    #  sEICs()
  })
  
  output$hotlink <- renderUI({
    if(set()$active){
      checkboxInput(ns('hotl'), "Hotlink mz and rt ranges",
                    value = selectionsEIC$plots[["chrom1"]]$hotlink)
    }
  })
  
  observeEvent(input$hotl,{
    if(set()$active){
      selectionsEIC$plots[["chrom1"]]$hotlink <-  input$hotl
    }
    #  sEICs()
  })
  #observeEvent(input$tic,{
  #selectionsEIC$plots$chrom1
  #})
  
  
  
  observeEvent(input$selMZ,{
    # print("mz changed")
    if(set()$active){
      selectionsEIC$plots[["chrom1"]]$mz <-  input$selMZ
      #sEICs()
    }
  })
  
  observeEvent(input$selEICs,{
    if(set()$active){
      selectionsEIC$plots$files$selected <- input$selEICs
    }
  })
  
  
  output$EICout <- renderUI({
    if(set()$active){
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
  
  output$chrominfo <- renderUI({ 
    if(set()$active){
      p(if(!is.null(selectionsEIC$plots[["chrom1"]]$hover$File)){
        paste0("Cursor on: ", basename(selectionsEIC$plots[["chrom1"]]$hover$File[1]),
               " @ ", round(as.numeric(selectionsEIC$plots[["chrom1"]]$hover$rt[1]), 3),
               " min / ", round(as.numeric(selectionsEIC$plots[["chrom1"]]$hover$rt[1]*60), 2), " sec")}
        else{""},
        if(!is.null(selectionsEIC$plots[["chrom1"]]$marker$File)){
          paste0("Marker on: ", basename(selectionsEIC$plots[["chrom1"]]$marker$File[1]),
                 " @ ", round(as.numeric(selectionsEIC$plots[["chrom1"]]$marker$rt[1]), 3),
                 " min / ", round(as.numeric(selectionsEIC$plots[["chrom1"]]$marker$rt[1]*60), 2), " sec")}
        else{""}
      )
    }
  })
  
  #observeEvent(selectionsEIC$plots$spec$marker$mz,{
  # selectionsEIC$plots[["chrom1"]]$mz <-  selectionsEIC$plots$spec$marker$mz
  #selectionsEIC$plots[["chrom1"]]$tic <-  F
  
  #})
  
  sEICs <- reactive ({
    if(set()$active && length(input$selMZ)>0 && length(input$selEICs)>0){
      #print(input$selMZ)
      if(set()$active && selectionsEIC$plots[["chrom1"]]$hotlink && !identical(selectionsEIC$set,set())){
        if(!is.null(set()$mz)){
          selectionsEIC$plots[["chrom1"]]$mz <- set()$mz}
        selectionsEIC$plots[["chrom1"]]$xrange <- set()$rtr
      }
        
        
      res <- multiEICplus(adducts = c(0),
                         mz = data.frame(mzmin = max(1,selectionsEIC$plots[["chrom1"]]$mz-selectionsEIC$plots[["chrom1"]]$mz*set()$layouts[[set()$activeGroup]]$settings$ppm*1e-6),
                                         mzmax=max(1,selectionsEIC$plots[["chrom1"]]$mz+selectionsEIC$plots[["chrom1"]]$mz*set()$layouts[[set()$activeGroup]]$settings$ppm*1e-6)),
                         rt = NULL,#data.frame(rtmin = FT$df$rt[1]-5, rtmax= FT$df$rt[1]+5),
                         rnames = 1:length(selectionsEIC$plots[["chrom1"]]$mz),
                         rawdata= set()$data,
                         RTcorr = if(is.null(input$interactiveRTcorr) || !input$interactiveRTcorr){NULL}else{set()$RTcorr}
      )
     # print(set()$data[which(basename(set()$filelist) %in% input$selEICs)])
      
      res <- subsetEICs(res, set()$filelist[which(basename(set()$filelist) %in% input$selEICs)])
      
      selectionsEIC$plots[['chrom1']]$maxxrange <- c(min(unlist(res$EIClist[[1]][,'rt'])),
                                                     max(unlist(res$EIClist[[1]][,'rt'])))/60
      
      selectionsEIC$plots[['chrom1']]$maxyrange <- if(input$EicTic){c(0,res$maxTIC)}else{c(0,res$maxEIC)}
      #print("sEICs")
      
      if(!is.null(selectionsEIC$plots[["chrom1"]]$xrange) && selectionsEIC$plots[["chrom1"]]$hotlink && !identical(selectionsEIC$set,set())
      ){
        mm <- 1
        
        if(selectionsEIC$plots[["chrom1"]]$tic){
          for(k in 1:length(res$EIClist)){
            mm <- max(mm,
                      max(unlist(res$EIClist[[k]][,"tic"])[which(unlist(res$EIClist[[k]][,"rt"]) >= min(selectionsEIC$plots[["chrom1"]]$xrange)*60 
                                                                     & unlist(res$EIClist[[k]][,"rt"]) <= max(selectionsEIC$plots[["chrom1"]]$xrange)*60)]))
          }
        }
        else{
          for(k in 1:length(res$EIClist)){
            mm <- max(mm,
                      max(unlist(res$EIClist[[k]][,"intensity"])[which(unlist(res$EIClist[[k]][,"rt"]) >= min(selectionsEIC$plots[["chrom1"]]$xrange)*60 
                                                                           & unlist(res$EIClist[[k]][,"rt"]) <= max(selectionsEIC$plots[["chrom1"]]$xrange)*60)]))
          }
        } 
        #print(mm)
        #print(unlist(res$EIClist[[k]][,"rt"])[which(unlist(res$EIClist[[k]][,"rt"]) >= min(selectionsEIC$plots[["chrom1"]]$xrange)*60 
         #                                               & unlist(res$EIClist[[k]][,"rt"]) <= max(selectionsEIC$plots[["chrom1"]]$xrange)*60)]/60)
        selectionsEIC$plots[['chrom1']]$yrange <- c(0,mm)
        
      }
    
    isolate(selectionsEIC$set <- set())
      
      
      return (res)
    }
  })
  
  #neccessary to run shiny nearPoints to determine data point nearest to pointer
  sEICsDF <- reactive({
    if(set()$active && !is.null(sEICs())){
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
    }
  })
  
  output$plainplot <- renderPlot({
    if(set()$active && !is.null(sEICs())){
      #sEICs()
      # pt1<- proc.time()
      #print(selectionsEIC$plots[["chrom1"]]$marker)
      
      
      
      
      EICplot(EICs = sEICs()$EIClist, cx = 1,#input$plotCx, 
              ylim = if(!is.null(selectionsEIC$plots[['chrom1']]$yrange)){selectionsEIC$plots[['chrom1']]$yrange}else{selectionsEIC$plots[['chrom1']]$maxyrange}, 
              xlim = if(!is.null(selectionsEIC$plots[['chrom1']]$xrange)){selectionsEIC$plots[['chrom1']]$xrange}else{selectionsEIC$plots[['chrom1']]$maxxrange},
              legendtext = paste(sub("^([^.]*).*", "\\1",basename(row.names(sEICs()$EIClist[[1]])))),
              colr = do.call(set()$layouts[[set()$activeGroup]]$settings$colr, list(n = nrow(sEICs()$EIClist[[1]]), alpha=0.8)),
              heading = if(input$EicTic){"TICs"}else{paste0("EIC for m/z ", round(input$selMZ,5), " +/- ",round(set()$layouts[[set()$activeGroup]]$settings$ppm,1), " ppm")},
              relto = NULL,
              TIC = input$EicTic,
              single = T,
              midline = if(length(selectionsEIC$plots[["chrom1"]]$marker$rt)!=0){selectionsEIC$plots[["chrom1"]]$marker$rt*60}else{NULL},
              lw = 2#input$plotLw
      )
      
      if(!is.null(selectionsEIC$plots[["chrom1"]]$hover)){
        if(input$EicTic){
          points(selectionsEIC$plots[["chrom1"]]$hover$rt,
                 selectionsEIC$plots[["chrom1"]]$hover$tic, cex =2)
        }else{
          points(selectionsEIC$plots[["chrom1"]]$hover$rt,
                 selectionsEIC$plots[["chrom1"]]$hover$intensity, cex =2)
        }
      }
      
      if(!is.null(selectionsEIC$plots[["chrom1"]]$marker)){
        if(input$EicTic){
          points(selectionsEIC$plots[["chrom1"]]$marker$rt,
                 selectionsEIC$plots[["chrom1"]]$marker$tic,
                 col = "red", cex =2)
        }else{
          points(selectionsEIC$plots[["chrom1"]]$marker$rt,
                 selectionsEIC$plots[["chrom1"]]$marker$intensity,
                 col = "red", cex =2)
        }
      }
    }
    
    
    
  })
  
  observeEvent(input$plainplot_dblclick,{
    if(set()$active){
      selectionsEIC$plots[["chrom1"]]$dblclick <- input$plainplot_dblclick
      selectionsEIC$lastChangedEIC <- "chrom1"
    }
  })  
  
  observeEvent(input$plainplot_click,{
    #print("click")
    if(set()$active){
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
      
      
      ###TEMPORARY FIX FOR BROKEN DOUBLECLICK, Ctrl + click

      if (keys() == 17) {
      if (!is.null(input$plainplot_brush)) {
        
        
        selectionsEIC$plots[['chrom1']]$xrange <- c(input$plainplot_brush$xmin, input$plainplot_brush$xmax)
        
        if(input$EicTic){
          ymax <- max(sEICsDF()$tic[which(sEICsDF()$rt <= input$plainplot_brush$xmax
                                          & sEICsDF()$rt >= input$plainplot_brush$xmin)])
        }else{
          ymax <- max(sEICsDF()$intensity[which(sEICsDF()$rt <= input$plainplot_brush$xmax
                                                & sEICsDF()$rt >= input$plainplot_brush$xmin)])
        }
        
        selectionsEIC$plots[['chrom1']]$yrange <- c(input$plainplot_brush$ymin, min(ymax,input$plainplot_brush$ymax))
        
      } else {
        # print("dblcklick")
        
        selectionsEIC$plots[['chrom1']]$xrange <- selectionsEIC$plots[['chrom1']]$maxxrange
        selectionsEIC$plots[['chrom1']]$yrange <- selectionsEIC$plots[['chrom1']]$maxyrange
      }
      }
      
      
      
      
      
      
      
    }
  })
  
  
  observeEvent(input$plainplot_hover,{
    if(set()$active && is.null(input$plainplot_brush) && !is.null(sEICsDF())){
      selectionsEIC$plots[["chrom1"]]$hover <- nearPoints(sEICsDF(),
                                                          input$plainplot_hover,
                                                          xvar = "rt",
                                                          yvar = if(input$EicTic){"tic"}else{"intensity"},
                                                          threshold = 20,
                                                          maxpoints = 1)
    }
    
  })
  
  observeEvent(input$plainplot_dblclick, {
    if(set()$active){
      
      if (!is.null(input$plainplot_brush)) {
        
        
        selectionsEIC$plots[['chrom1']]$xrange <- c(input$plainplot_brush$xmin, input$plainplot_brush$xmax)
        
        if(input$EicTic){
          ymax <- max(sEICsDF()$tic[which(sEICsDF()$rt <= input$plainplot_brush$xmax
                                          & sEICsDF()$rt >= input$plainplot_brush$xmin)])
        }else{
          ymax <- max(sEICsDF()$intensity[which(sEICsDF()$rt <= input$plainplot_brush$xmax
                                                & sEICsDF()$rt >= input$plainplot_brush$xmin)])
        }
        
        selectionsEIC$plots[['chrom1']]$yrange <- c(input$plainplot_brush$ymin, min(ymax,input$plainplot_brush$ymax))
        
      } else {
        # print("dblcklick")
        
        selectionsEIC$plots[['chrom1']]$xrange <- selectionsEIC$plots[['chrom1']]$maxxrange
        selectionsEIC$plots[['chrom1']]$yrange <- selectionsEIC$plots[['chrom1']]$maxyrange
      }}
  }
  )
  
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
  htmlOutput(ns('EICout'))
  
}