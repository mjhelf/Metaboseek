selections <- reactiveValues(plots = list(chrom1 = list(xrange = NULL,
                                                        yrange = NULL,
                                                        maxxrange = NULL,
                                                        maxyrange = NULL,
                                                        marker = NULL,
                                                        mz = NULL,
                                                        tic = T),
                                          spec1 = list(xrange = NULL,
                                                       yrange = NULL,
                                                       maxxrange = NULL,
                                                       maxyrange = NULL,
                                                       marker = NULL,
                                                       data = NULL)
),
index = c("chrom1", "spec1"),
lastChangedEIC = "chrom1"
)

output$selEICs <- renderUI({ selectizeInput('selEICs', "select files", choices = lapply(MSData$layouts[[MSData$active]]$grouping,basename), selected = lapply(MSData$layouts[[MSData$active]]$grouping,basename)[[1]], multiple = TRUE)})
output$selMZ <- renderUI({ numericInput('selMZ', "m/z", value = selections$plots[["chrom1"]]$mz, min = 0, step = 0.01)})
output$EicTic <- renderUI({ checkboxInput('EicTic', "TIC",
                                          value = T
                                           # selections$plots[["chrom1"]]$tic
                                          )
  })
output$interactiveRTcorr <- renderUI({ checkboxInput('interactiveRTcorr', "RT correction", value = F)})


observeEvent(input$selEICs,{
  #initialize sEICs
  sEICs()
  
})

observeEvent(input$EicTic,{
  selections$plots[["chrom1"]]$tic <-  input$EicTic
  sEICs()
})

observeEvent(input$selMZ,{
  selections$plots[["chrom1"]]$mz <-  input$selMZ
  
})

observeEvent(selections$plots[["spec1"]]$marker$mz,{
  selections$plots[["chrom1"]]$mz <-  selections$plots[["spec1"]]$marker$mz
  selections$plots[["chrom1"]]$tic <-  F
  
})

sEICs <- reactive ({
  if(length(selections$plots[["chrom1"]]$mz)>0){
  r1 <- multiEICplus(adducts = c(0),
                     mz = data.frame(mzmin = max(1,selections$plots[["chrom1"]]$mz-selections$plots[["chrom1"]]$mz*MSData$layouts[[MSData$active]]$settings$ppm*1e-6),
                                     mzmax=max(1,selections$plots[["chrom1"]]$mz+selections$plots[["chrom1"]]$mz*MSData$layouts[[MSData$active]]$settings$ppm*1e-6)),
                     rt = NULL,#data.frame(rtmin = FT$df$rt[1]-5, rtmax= FT$df$rt[1]+5),
                     rnames = 1:length(selections$plots[["chrom1"]]$mz),
                     rawdata= MSData$data,
                     RTcorr = if(is.null(input$interactiveRTcorr) || !input$interactiveRTcorr){NULL}else{MSData$RTcorr}
  )
  
  
  res <- subsetEICs(r1, MSData$filelist[which(basename(MSData$filelist) %in% input$selEICs)])
  
  selections$plots[['chrom1']]$maxxrange <- c(min(unlist(res$EIClist[[1]][,'rt'])),
                                              max(unlist(res$EIClist[[1]][,'rt'])))/60
  
  selections$plots[['chrom1']]$maxyrange <- if(input$EicTic){c(0,res$maxTIC)}else{c(0,res$maxEIC)}
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
    
    EICplot(EICs = sEICs()$EIClist, cx = input$plotCx, 
            ylim = if(!is.null(selections$plots[['chrom1']]$yrange)){selections$plots[['chrom1']]$yrange}else{selections$plots[['chrom1']]$maxyrange}, 
            xlim = if(!is.null(selections$plots[['chrom1']]$xrange)){selections$plots[['chrom1']]$xrange}else{selections$plots[['chrom1']]$maxxrange},
            legendtext = paste(sub("^([^.]*).*", "\\1",basename(row.names(sEICs()$EIClist[[1]])))),
            colr = do.call(MSData$layouts[[MSData$active]]$settings$colr, list(n = nrow(sEICs()$EIClist[[1]]), alpha=0.8)),
            heading = if(input$EicTic){"TICs"}else{paste0("EIC for m/z ", round(selections$plots[["chrom1"]]$mz,5), " +/- ",round(MSData$layouts[[MSData$active]]$settings$ppm,1), " ppm")},
            relto = NULL,
            TIC = input$EicTic,
            single = T,
            midline = if(length(selections$plots[["chrom1"]]$marker$rt)!=0){selections$plots[["chrom1"]]$marker$rt*60}else{NULL},
            lw = input$plotLw
    )
    
    if(!is.null(selections$plots[["chrom1"]]$hover)){
      if(input$EicTic){
        points(selections$plots[["chrom1"]]$hover$rt,
               selections$plots[["chrom1"]]$hover$tic)
      }else{
        points(selections$plots[["chrom1"]]$hover$rt,
               selections$plots[["chrom1"]]$hover$intensity)
      }
    }
    
    if(!is.null(selections$plots[["chrom1"]]$marker)){
      if(input$EicTic){
        points(selections$plots[["chrom1"]]$marker$rt,
               selections$plots[["chrom1"]]$marker$tic,
               col = "red")
      }else{
        points(selections$plots[["chrom1"]]$marker$rt,
               selections$plots[["chrom1"]]$marker$intensity,
               col = "red")
      }
    }
  }
  
  
  
  #pt1 <- proc.time() - pt1
  #print(pt1)
  
  
})

observeEvent(input$plainplot_dblclick,{
  selections$plots[["chrom1"]]$dblclick <- input$plainplot_dblclick
  selections$lastChangedEIC <- "chrom1"
})  

observeEvent(input$plainplot_click,{
  if (keyin$keyd == 16) {
    
    selections$plots[["chrom1"]]$click <- input$plainplot_click
    selections$plots[["chrom1"]]$marker <- nearPoints(sEICsDF(),
                                                      input$plainplot_click,
                                                      xvar = "rt",
                                                      yvar = if(input$EicTic){"tic"}else{"intensity"},
                                                      threshold = 100,
                                                      maxpoints = 1)
    
    selections$lastChangedEIC <- "chrom1"
    
    #initiate sc()
    sc()
    
  }
})



#  observeEvent(input$plainplot_brush,{
#   selections[["chrom1"]]$click <- input$plainplot_brush
#  selections$lastChangedEIC <- "chrom1"
#})

#  observeEvent(input$plainplot_dblclick,{
#   selections[["chrom1"]]$hover <- input$plainplot_hover
#  })

observeEvent(input$plainplot_hover,{
  if(is.null(input$plainplot_brush)){
    selections$plots[["chrom1"]]$hover <- nearPoints(sEICsDF(),
                                                     input$plainplot_hover,
                                                     xvar = "rt",
                                                     yvar = if(input$EicTic){"tic"}else{"intensity"},
                                                     threshold = 100,
                                                     maxpoints = 1)
  }
  
})

observeEvent(input$plainplot_dblclick, {
  
  if (!is.null(input$plainplot_brush)) {
    selections$plots[['chrom1']]$xrange <- c(input$plainplot_brush$xmin, input$plainplot_brush$xmax)
    selections$plots[['chrom1']]$yrange <- c(input$plainplot_brush$ymin, input$plainplot_brush$ymax)
    
  } else {
    selections$plots[['chrom1']]$xrange <- selections$plots[['chrom1']]$maxxrange
    selections$plots[['chrom1']]$yrange <- selections$plots[['chrom1']]$maxyrange
  }})



###############Spectrum Plot

sc <- reactive({
  if(!is.null(selections$plots[["chrom1"]]$marker)){
    res <- getScan(MSData$data[[selections$plots[["chrom1"]]$marker$File]], selections$plots[["chrom1"]]$marker$scan)
    #print("sc")
    selections$plots[['spec1']]$maxxrange <- c(min(res[,1]),
                                               max(res[,1]))
    
    selections$plots[['spec1']]$maxyrange <- c(0, 100)
    
    selections$plots[['spec1']]$data <- res
    
    return(res)
    
  }
})

output$Mspec <- renderPlot({
  if(!is.null(selections$plots[["chrom1"]]$marker)){
    
    specplot(x=selections$plots[['spec1']]$data[,1],
             y=selections$plots[['spec1']]$data[,2],
             norm=max(selections$plots[['spec1']]$data[,2])/100,
             cx=1,
             k = 20,
             fileName = basename(selections$plots[["chrom1"]]$marker$File),
             yrange = if(!is.null(selections$plots[['spec1']]$yrange)){selections$plots[['spec1']]$yrange}else{selections$plots[['spec1']]$maxyrange},
             xrange = if(!is.null(selections$plots[['spec1']]$xrange)){selections$plots[['spec1']]$xrange}else{selections$plots[['spec1']]$maxxrange},
             maxi = max(selections$plots[['spec1']]$data[,2])
    ) 
    
    
    if(!is.null(selections$plots[["spec1"]]$hover)){
      
      points(selections$plots[["spec1"]]$hover$mz,
             selections$plots[["spec1"]]$hover$intensity)
      
    }
    
    if(!is.null(selections$plots[["spec1"]]$marker)){
      
      points(selections$plots[["spec1"]]$marker$mz,
             selections$plots[["spec1"]]$marker$intensity,
             col = "red")
      
    }
    
    
    
  }
}, height = 550)



observeEvent(input$Mspec_dblclick,{
  selections$plots[["spec1"]]$dblclick <- input$Mspec_dblclick
  selections$lastChangedEIC <- "spec1"
})  

observeEvent(input$Mspec_click,{
  if (keyin$keyd == 16) {
    #print("MSmarker")
    selections$plots[["spec1"]]$click <- input$Mspec_click
    
    temp <- as.data.frame(selections$plots[['spec1']]$data)
    temp$intensity <- temp$intensity/(max(temp$intensity)/100)
    
    selections$plots[["spec1"]]$marker <- nearPoints(temp,
                                                     input$Mspec_click,
                                                     xvar = "mz",
                                                     yvar = "intensity",
                                                     threshold = 100,
                                                     maxpoints = 1)
    
    
    # datadf
    
    selections$lastChangedEIC <- "spec1"
  }
})



#  observeEvent(input$plainplot_brush,{
#   selections[["chrom1"]]$click <- input$plainplot_brush
#  selections$lastChangedEIC <- "chrom1"
#})

#  observeEvent(input$plainplot_dblclick,{
#   selections[["chrom1"]]$hover <- input$plainplot_hover
#  })

observeEvent(input$Mspec_hover,{
  if(is.null(input$Mspec_brush)){
    temp <- as.data.frame(selections$plots[['spec1']]$data)
    temp$intensity <- temp$intensity/(max(temp$intensity)/100)
    
    selections$plots[["spec1"]]$hover <- nearPoints(temp,
                                                    input$Mspec_hover,
                                                    xvar = "mz",
                                                    yvar = "intensity",
                                                    threshold = 100,
                                                    maxpoints = 1)
  }
  
})

observeEvent(input$Mspec_dblclick, {
  
  if (!is.null(input$Mspec_brush)) {
    selections$plots[['spec1']]$xrange <- c(input$Mspec_brush$xmin, input$Mspec_brush$xmax)
    selections$plots[['spec1']]$yrange <- c(input$Mspec_brush$ymin, input$Mspec_brush$ymax)
    
  } else {
    selections$plots[['spec1']]$xrange <- selections$plots[['spec1']]$maxxrange
    selections$plots[['spec1']]$yrange <- selections$plots[['spec1']]$maxyrange
  }})
