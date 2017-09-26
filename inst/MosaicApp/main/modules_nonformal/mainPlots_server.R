source(file.path("modules_nonformal", "mainPlots_options_server.R"), local = TRUE)$value 


output$groupingActiveSelect <- renderUI({
    selectizeInput("groupingActiveSelect", "Select Grouping scheme:", choices = MSData$index, selected = MSData$active)
})

observeEvent(input$groupingActiveSelect,{
    
    MSData$active <- input$groupingActiveSelect
})

output$pdfButton <- downloadHandler(filename= function(){paste0(input$projectName,"_mainPlot.pdf")}, 
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
                                                   adducts = if(input$plotAdducts == ""){c(0)}else{as.numeric(strsplit(input$plotAdducts, " ")[[1]])},
                                                   cx = input$plotCx,
                                                   midline = input$MLtoggle,
                                                   yzoom = input$plotYzoom
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
                 adducts = if(input$plotAdducts == ""){c(0)}else{as.numeric(strsplit(input$plotAdducts, " ")[[1]])},
                 cx = input$plotCx,
                 midline = input$MLtoggle,
                 yzoom = input$plotYzoom
      )
  }
    
}, bg = "white")


output$mainPlotPlaceholder2 <- renderPlot({
  if(is.null(MSData$data)){
  EICsTIC <- multiEIC(rawdata= rdata,
                      mz = mzx[1,],
                      rt = NULL,
                      rnames = "1", #major item names
                      byFile = F #if true, table will be sorted by rawfile, otherwise by feature
  )
  groupPlot(EIClist = EICsTIC,
            grouping = glist,
            plotProps = list(TIC = T, #settings for single plots
                             cx = cx,
                             colr = colrs,
                             lw = lw,
                             midline = NULL,
                             ylim = NULL, #these should be data.frames or matrices of nrow = number of plotted features
                             xlim = NULL,
                             yzoom = 1),
            compProps = list(mfrow=c(rows,cols), #par options for the composite plot
                             oma=c(0,2,4,0),
                             xpd=NA, bg="white",
                             header =  "TICs",
                             header2 = NULL,
                             pdfFile = NULL,
                             pdfHi = 6*rows,
                             pdfWi = 6*cols,
                             cx = cx,
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
    
    
    
    
    
    imageOutput("mainPlotPlaceholder",
                #height ="500px"
                width = "auto")}
    
    
    
})

output$adductLegend <- renderPlot({
    if(input$plotAdducts != ""){
      
      legendplot("top",
                 legend = strsplit(input$plotAdducts, " ")[[1]],
                 lty=1:length(strsplit(input$plotAdducts, " ")[[1]]),
                 lwd=input$plotLw*1.2,
                 col="black", bty="n", 
                 cex=input$plotCx, horiz = T)
  }
  
}, bg = "white")


observe({
  toggleState(id = "pdfButton", condition = !is.null(MSData$active))
})