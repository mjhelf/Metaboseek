source(file.path("modules_nonformal", "mainPlots_options_server.R"), local = TRUE)$value 
source(file.path("modules_nonformal", "interactiveView_server.R"), local = TRUE)$value 
source(file.path("modules_nonformal", "quickPlots_server.R"), local = TRUE)$value 


output$groupingActiveSelect <- renderUI({
    selectizeInput("groupingActiveSelect", "Select Grouping scheme:", choices = MSData$index, selected = MSData$active)
})

observeEvent(input$groupingActiveSelect,{
    
    MSData$active <- input$groupingActiveSelect
})

output$pdfButton <- downloadHandler(filename= function(){
  titleout <- filenamemaker(projectData$projectName,featureTables)
  
  return(paste0(titleout,".pdf"))}, 
                                    content = function(file){
                                      
                                      if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
                                        if(featureTables$tables[[featureTables$active]]$editable){
                                          featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
                                        }else{
                                          featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"] 
                                        }
                                      }
                                        EICgeneral(rtmid = if(nrow(combino())<=1000){combino()[,"rt"]}else{combino()[1:1000,"rt"]},
                                                   mzmid = if(nrow(combino())<=1000){combino()[,"mz"]}else{combino()[1:1000,"mz"]},
                                                               glist = MSData$layouts[[MSData$active]]$grouping,
                                                               cols = MSData$layouts[[MSData$active]]$settings$cols,
                                                               colrange = MSData$layouts[[MSData$active]]$settings$colr,
                                                               transparency = MSData$layouts[[MSData$active]]$settings$alpha,
                                                               RTall = input$RTtoggle,
                                                               rtw = MSData$layouts[[MSData$active]]$settings$rtw,
                                                               ppm = MSData$layouts[[MSData$active]]$settings$ppm,
                                                               rdata = MSData$data,
                                                   pdfFile = file,
                                                   leadingTIC = T,
                                                   TICall = input$TICtoggle,
                                                   lw = input$plotLw,
                                                   adducts = massShiftsOut()$shifts,
                                                   cx = input$plotCx,
                                                   midline = input$MLtoggle,
                                                   yzoom = input$plotYzoom,
                                                   RTcorrect = if(is.null(input$RtCorrActive) || !input$RtCorrActive){NULL}else{MSData$RTcorr}
                                        )
                                      },
                                    
                                    
                                    
                                    contentType = "application/pdf")


output$mainPlotPlaceholder <- renderImage({
  
  if(is.null(MSData$data)){
    list(src = "img/PlotPlaceholder.png",
         contentType = 'image/png',
         #width = 250,
         height = 500,
         alt = "MOSAiC")
  }
  
}, deleteFile = FALSE)

output$mainPlotEICsPre <- renderPlot({
  if(!is.null(MSData$data)){
      EICgeneral(rtmid = if(is.null(maintabsel())){NULL}else{hot_to_r(input$maintable)[maintabsel()$rrng[1],"rt"]},
                 mzmid = if(is.null(maintabsel())){NULL}else{hot_to_r(input$maintable)[maintabsel()$rrng[1],"mz"]},
                 glist = MSData$layouts[[MSData$active]]$grouping,
                 cols = MSData$layouts[[MSData$active]]$settings$cols,
                 colrange = MSData$layouts[[MSData$active]]$settings$colr,
                 transparency = MSData$layouts[[MSData$active]]$settings$alpha,
                 RTall = input$RTtoggle,
                 TICall = input$TICtoggle,
                 rtw = MSData$layouts[[MSData$active]]$settings$rtw,
                 ppm = MSData$layouts[[MSData$active]]$settings$ppm,
                 rdata = MSData$data,
                 pdfFile = NULL,
                 leadingTIC = F,
                 lw = input$plotLw,
                 adducts = massShiftsOut()$shifts,
                 cx = input$plotCx,
                 midline = input$MLtoggle,
                 yzoom = input$plotYzoom,
                 RTcorrect = if(is.null(input$RtCorrActive) || !input$RtCorrActive){NULL}else{MSData$RTcorr}
      )
  }
    
}, bg = "white", execOnResize = T)


output$mainPlotPlaceholder2 <- renderPlot({
  if(is.null(MSData$data)){
  #Placeholder:
    
    PH <- list()
  intens <- c(rep(0,31),(33:15)*3,(15:33)*3,rep(0,31))
  PH[[1]]<- matrix(list(c(1:100),intens,c(1:100),intens,1,100,1,100,max(intens),500,50),
                   nrow = 1,
                   ncol = 11,
                   dimnames = list(rows = c("sample1"),
                                   columns = c("scan", "intensity", "rt", "tic", "mzmin", "mzmax", "rtmin", "rtmax", "intmax", "intsum", "intmean")))
  
  for(n in 1:8){
    PH[[1]] <- rbind(PH[[1]], matrix(list(c(1:100)+n,intens,c(1:100)+n,intens,1+n,100+n,1+n,100+n,max(intens),500,50),
                                     nrow = 1,
                                     ncol = 11,
                                     dimnames = list(rows = c(paste0("sample",n+1)),
                                                     columns = c("scan", "intensity", "rt", "tic", "mzmin", "mzmax", "rtmin", "rtmax", "intmax", "intsum", "intmean"))))
  }
  groupPlot(EIClist = PH,
            grouping = list("No Data Loaded" = row.names(PH[[1]])),
            plotProps = list(TIC = T, #settings for single plots
                             cx = 1.2,
                             colr = do.call(input$colorscheme,
                                            list(n=nrow(PH[[1]]), alpha = 0.8)),
                             lw = 2,
                             midline = NULL,
                             ylim = NULL, #these should be data.frames or matrices of nrow = number of plotted features
                             xlim = NULL,
                             yzoom = 1),
            compProps = list(mfrow=c(1,1), #par options for the composite plot
                             oma=c(0,2,4,0),
                             xpd=NA, bg="white",
                             header =  "Welcome to MOSAiC",
                             header2 = NULL,
                             pdfFile = NULL,
                             pdfHi = 6*1,
                             pdfWi = 6*1,
                             cx = 1.2,
                             adductLabs = 0)
  )
  }
})




mainPlotHeight <- reactive({if(!is.null(MSData$active) && MSData$active != ""){
  paste0(ceiling(length(MSData$layouts[[MSData$active]]$grouping)/MSData$layouts[[MSData$active]]$settings$cols)*400+100,"px")
}
  else{"auto"}
  })

output$mainPlotEICs <- renderUI({
  if(!is.null(MSData$data)){
    plotOutput("mainPlotEICsPre",
               height = mainPlotHeight()#,
               #click = "spec2_click",
               #hover = "mainPlot_hover",
               #dblclick = "mainPlot_dblclick",
               #brush = brushOpts(
                #   id = "mainPlot_brush",
                 #  direction = "x",
                  # resetOnNew = TRUE)
               )
  }else{
    
    
    
    
    
    plotOutput("mainPlotPlaceholder2",
                height ="600px"
                )}
    
    
    
})
massShiftsOut <- reactive({
  if(is.null(input$massShiftTab)){
    tab <- massShifts$table
  }
  else{
    tab <- hot_to_r(input$massShiftTab)
  }
  
  if(!any(tab$use)){
    labs <- ""
      shifts <- 0
  }else{
    labs <- tab$Name[which(tab$use)]
    shifts <- tab$mz_shift[which(tab$use)]
  }
  
  return(list(labs =labs, shifts = shifts))
  
  
})


output$adductLegend <- renderPlot({
    if(length(massShiftsOut()$shifts) > 1 || massShiftsOut()$shifts != 0){
      
      legendplot("center",
                 legend = massShiftsOut()$labs,
                 lty = 1:length(massShiftsOut()$labs),
                 lwd = input$plotLw*1.2,
                 col = "black", bty = "n", 
                 cex = input$plotCx, horiz = T)
  }
  
}, height = 30)



observe({
  toggleState(id = "pdfButton", condition = !is.null(MSData$active))
})


