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
#' @param keys registered keystrokes
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
                                                              msdata = NULL,
                                                              moreArgs = NULL),
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
                                            
                                                        plotArgs = NULL,
                                            set = NULL #copy of set() to check if set() has changed
  )
  )
  
  # observeEvent(set(),{sc()})
  sc <- reactive({
    # observe({
    if(!is.null(set()) && set()$layout$active && !is.null(set()$spec$sel$File) && !identical(selections$plots$set, set()$spec )){
      
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
                             MSfile = set()$msdata[match(basename(set()$spec$sel$File[datasel]),basename(names(set()$msdata)))],
                             scan = set()$spec$sel$scan[datasel],
                             MoreArgs = list(MS2 = (!is.null(set()$spec$MS2) && set()$spec$MS2)))
          
          #remove NULL results (from errors)
          speclist <- speclist[which(!sapply(speclist, is.null))]
          
          #make sure signal to other functions that the spectrum IS a merge product
          if(length(speclist) > 1){
            selections$plots$spec$MSmerge <- quickMergeMS(speclist)
            
            res <- selections$plots$spec$MSmerge}
          else{
            selections$plots$spec$MSmerge <- NULL
          }
          
        }          
        #print("sc")
        #set the maximum x axis range to cover the spectrum data
        if(!is.null(set()$spec$maxxrange) 
           && !identical(selections$plots$set$maxxrange,set()$spec$maxxrange)){
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
    if(!is.null(set())){
    if(length(set()$layout$highlights) > 0){
      selections$plots$spec$highlights <- set()$layout$highlights
    }else{
      selections$plots$spec$highlights <- NULL
      
    }
    }
  })
  
  output$specinfo <- renderUI({ 
    if(!is.null(set()) && set()$layout$active && !is.null(set()$spec$sel$File)){
      
      
      
      
      
      
      fluidRow(
        p(paste0("Marker on: ", round(as.numeric(selections$plots$spec$marker[1]), 5),
                 ", Cursor on: ", round(as.numeric(selections$plots$spec$hover$mz[1]),5),
                 if(length(selections$plots$spec$hover$mz) !=0 && length(selections$plots$spec$marker) != 0){
                   paste0(" (",
                          if(as.numeric(selections$plots$spec$hover$mz) > as.numeric(selections$plots$spec$marker$mz)){"+"}else{""},
                          round(as.numeric(selections$plots$spec$hover$mz[1]) - as.numeric(selections$plots$spec$marker[1]),5), ")")}else{""}))
        
        # if(length(set()$spec$sel$File) >1){
        #            p("Scans:", paste(coll, collapse = ", "))
        # }else{
        #            a()
        #          }
      )     
      
    }
    
    
  })
  
  
  output$specAll <- renderUI({
    if(!is.null(set())
       && length(set()$layout$active) > 0 
       && set()$layout$active
       && length(set()$spec$sel) >1
    ){
      
     
      #select file based on basename rather than full path
      filesel <- match(basename(set()$spec$sel$File), basename(names(set()$msdata)))
      
      #get the precursors
      if(!is.null(set()$spec$MS2) && set()$spec$MS2){
        acn <- round(mapply("[",lapply(set()$msdata[filesel],slot,"msnAcquisitionNum"),set()$spec$sel$scan),5)
      }else{
        acn <- round(mapply("[",lapply(set()$msdata[filesel],slot,"acquisitionNum"),set()$spec$sel$scan),5)
      }
      
      coll <- character(0)
      for(i in unique(basename(names(set()$msdata))[filesel])){
        coll <- c(coll, paste0(i,"#",paste(acn[which(basename(names(set()$msdata))[filesel] == i)], collapse = "/")))
      }
      coll <- sort(coll)
      # }
      
      
      
      fluidPage(
        fluidRow(downloadButton(ns('pdfButton'), "Save as pdf"),
                 downloadButton(ns('peaklistButton'), "Save as table")
                          
        ),
        fluidRow(
          tags$div(title = paste("Scans:", paste(coll, collapse = ", ")),
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
                   )
          )),
        htmlOutput(ns("specinfo"))
        
        
      )
    }
  })
  
  
  
  
  output$Mspec <- renderPlot({
    if(!is.null(set()) && set()$layout$active && !is.null(set()$spec$sel$File)){
      sc()
      
      xr <- if(!is.null(selections$plots$spec$xrange)){selections$plots$spec$xrange}else{selections$plots$spec$maxxrange}
      
      #intensity values in the current x-axis range view 
      yinview <- selections$plots$spec$data[,2][which(selections$plots$spec$data[,1]>= min(xr) 
                                                      & selections$plots$spec$data[,1]<= max(xr))]
      
      #select file based on basename rather than full path
      filesel <- match(basename(set()$spec$sel$File), basename(names(set()$msdata)))
      
      #get the precursors
      prec <- round(mapply("[",lapply(set()$msdata[filesel],slot,"msnPrecursorMz"),set()$spec$sel$scan),5)
      
      
      label <- if(length(set()$spec$sel$File) >0){
        if(length(set()$spec$sel$File) == 1){
          paste0(basename(set()$spec$sel$File), "#", 
                 if(!is.null(set()$spec$MS2) && set()$spec$MS2){
                   set()$msdata[[filesel]]@msnAcquisitionNum[set()$spec$sel$scan]
                 }
                 else{set()$msdata[[filesel]]@acquisitionNum[set()$spec$sel$scan]},
                 " (", round(as.numeric(set()$spec$sel$rt)/60,3), " min / ", round(as.numeric(set()$spec$sel$rt),1), " sec)",
                 if(!is.null(set()$spec$MS2) && set()$spec$MS2){
                   paste0("\nParent m/z: ", prec)}else{""},
                 collapse = " ")}
        
        #more than one scan:
        else{
          if(!is.null(set()$spec$MS2) && set()$spec$MS2){
            paste0(
              "Average of ",
              length(set()$spec$sel$rt), " MS2 spectra (RT: ",
              min(round(as.numeric(set()$spec$sel$rt)/60,2)),
              " - ",
              max(round(as.numeric(set()$spec$sel$rt)/60,2)),
              " min)",
              "\nParent m/z: ", min(prec), " - ", max(prec) )
            
          }else{
            
            paste0(
              "Average of ",
              length(set()$spec$sel$rt), " MS spectra (RT: ",
              min(round(as.numeric(set()$spec$sel$rt)/60,2)),
              " - ",
              max(round(as.numeric(set()$spec$sel$rt)/60,2)),
              " min)"
            )
          }
        }
      }else{""}
      
       tempArgs <- list(x=selections$plots$spec$data[,1],
                                           y=selections$plots$spec$data[,2],
                                           norm=selections$plots$spec$ymax/100,
                                           cx=set()$layout$cex/1.5,
                                           k = 20,
                                           fileName = label,
                                           yrange = if(!is.null(selections$plots$spec$yrange)){selections$plots$spec$yrange}else{selections$plots$spec$maxyrange},
                                           xrange = if(!is.null(selections$plots$spec$xrange)){selections$plots$spec$xrange}else{selections$plots$spec$maxxrange},
                                           maxi = selections$plots$spec$ymax
      )
      
      if(!is.null(set()$moreArgs)){
       
        for(i in names(set()$moreArgs)){
          tempArgs[[i]] <- set()$moreArgs[[i]]
        }
        
        if(!is.null(tempArgs$labels)){
          
          mzmatched <- match(round(tempArgs$labels$x,5), round(tempArgs$x,5))
          
          tempArgs$labels <- tempArgs$labels[!is.na(mzmatched),]
          tempArgs$labels$y <- tempArgs$y[na.omit(mzmatched)]
          
        }
         
      }
      selections$plots$plotArgs <- tempArgs
      do.call(specplot,tempArgs)
        
      #   specplot(x=selections$plots$spec$data[,1],
      #          y=selections$plots$spec$data[,2],
      #          norm=selections$plots$spec$ymax/100,
      #          cx=set()$layout$cex/1.5,
      #          k = 20,
      #          fileName = label,
      #          yrange = if(!is.null(selections$plots$spec$yrange)){selections$plots$spec$yrange}else{selections$plots$spec$maxyrange},
      #          xrange = if(!is.null(selections$plots$spec$xrange)){selections$plots$spec$xrange}else{selections$plots$spec$maxxrange},
      #          maxi = selections$plots$spec$ymax
      # ) 
      
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
      
    }else{
      selections$plots$plotArgs <- NULL 
    }
  })#, height = if(is.null(set()$layout$height)){550}else{set()$layout$height})
  
  
  
  # observeEvent(input$Mspec_dblclick,{
  #   #selections$lastChangedEIC <- "spec1"
  # })  
  
  observeEvent(input$Mspec_click,{
    #print("click!")
    if (length(keys()) >0 && keys() == 16) {
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
    if (length(keys()) >0 && keys() == 17) {
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
      
      
    }})
  
  output$pdfButton <- downloadHandler(filename= function(){
    titleout <- if(length(set()$spec$sel$File) == 1){
      filesel <- match(basename(set()$spec$sel$File), basename(names(set()$msdata)))
      
      paste0(basename(set()$spec$sel$File), "_", 
             if(!is.null(set()$spec$MS2) && set()$spec$MS2){
               set()$msdata[[filesel]]@msnAcquisitionNum[set()$spec$sel$scan]
             }
             else{set()$msdata[[filesel]]@acquisitionNum[set()$spec$sel$scan]})}
    else{"averagedSpectra"}
    
    return(paste0(titleout,".pdf"))}, 
    content = function(file){
      
      pdf(file,
          14,6
      )
      
      if(!is.null(selections$plots$spec$fullplot)){
        replayPlot(selections$plots$spec$fullplot)
      }
      
      #replayPlot(selections$plots$spec$fullplot)
      dev.off()
      
    },
    contentType = "application/pdf")
  
  output$peaklistButton <- downloadHandler(filename= function(){
    titleout <- if(length(set()$spec$sel$File) == 1){
      filesel <- match(basename(set()$spec$sel$File), basename(names(set()$msdata)))
      
      paste0(basename(set()$spec$sel$File), "_", 
             if(!is.null(set()$spec$MS2) && set()$spec$MS2){
               set()$msdata[[filesel]]@msnAcquisitionNum[set()$spec$sel$scan]
             }
             else{set()$msdata[[filesel]]@acquisitionNum[set()$spec$sel$scan]})}
    else if(!is.null(selections$plots$spec$data)){"averagedSpectra"}
    else{"ERROR"}
    
    return(paste0(titleout,".tsv"))}, 
    content = function(file){
      if(!is.null(selections$plots$spec$data)){
      data.table::fwrite(data.frame(mz = selections$plots$spec$data[,1],
                                          intensity = selections$plots$spec$data[,2]), file, sep = "\t")
      }else{
        data.table::fwrite(data.frame(mz = 0,
                                      intensity = 0), file, sep = "\t")
      }
    },
    contentType = "text/tab-separated-values")
  
  return(
    reactive({
      selections$plots
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