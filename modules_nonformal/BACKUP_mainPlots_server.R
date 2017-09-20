output$TICtoggle <- renderUI({
  checkboxInput("TICtoggle","TIC", value = F)
})
output$RTtoggle <- renderUI({
  checkboxInput("RTtoggle","Full RT range", value = F)
})

#output$RTwindow <- renderUI({
 # numericInput("RTwindow","RT window (sec): ", value = 30, min = 0, width =60)
#})
observeEvent(input$RTwindow,{  if(!is.null(MSData$active)){
  MSData$layouts[[MSData$active]]$settings$rtw <- input$RTwindow
}
  })

output$PPMwindow <- renderUI({
  numericInput("PPMwindow","Mass tol (ppm): ", value = 5, min = 0)
})
observeEvent(input$PPMwindow,{
  if(!is.null(MSData$active)){
  MSData$layouts[[MSData$active]]$settings$ppm <- input$PPMwindow
}
  })

output$plotCols <- renderUI({
    numericInput("plotCols","Plots per row: ", value = 1, min = 1)
})
observeEvent(input$plotCols,{
    if(!is.null(MSData$active)){
        MSData$layouts[[MSData$active]]$settings$cols <- input$plotCols
    }
})


output$groupingActiveSelect <- renderUI({
    selectizeInput("groupingActiveSelect", "Select Grouping scheme:", choices = MSData$index, selected = MSData$active)
})

observeEvent(input$groupingActiveSelect,{
    
    MSData$active <- input$groupingActiveSelect
})

output$pdfButton <- downloadHandler(filename= function(){paste0(input$projectName,"_mainPlot.pdf")}, 
                                    content = function(file){
                                        EICgeneral(rtmid = combino()[,"rt"],
                                                               mzmid = combino()[,"mz"],
                                                               glist = MSData$layouts[[MSData$active]]$grouping,
                                                               cols = MSData$layouts[[MSData$active]]$settings$cols,
                                                               colrange = MSData$layouts[[MSData$active]]$settings$colr,
                                                               transparency = MSData$layouts[[MSData$active]]$settings$alpha,
                                                               RTall = input$RTtoggle,
                                                               rtw = MSData$layouts[[MSData$active]]$settings$rtw,
                                                               ppm = MSData$layouts[[MSData$active]]$settings$ppm,
                                                               rdata = MSData$data,
                                                   pdfFile = file
                                        )
                                      },
                                    
                                    
                                    
                                    contentType = "application/pdf")




output$mainPlotEICsPre <- renderPlot({
  if(!is.null(MSData$data)){
  
  #mzmid <- combino()[maintabsel()$rrng,"mz"]
  #rtmid <- combino()[maintabsel()$rrng,"rt"]
  mzmid <- hot_to_r(input$maintable)[maintabsel()$rrng,"mz"]
  rtmid <- hot_to_r(input$maintable)[maintabsel()$rrng,"rt"]
  
  
      glist <- MSData$layouts[[MSData$active]]$grouping
    colvec <- do.call(MSData$layouts[[MSData$active]]$settings$colr,
                list(n=max(sapply(glist,length)), alpha = MSData$layouts[[MSData$active]]$settings$alpha))
    colrs <- list()
    for(i in 1:length(glist)){
        
        colrs[[i]] <- colvec[1:length(glist[[i]])]
        
    }

    cols <- MSData$layouts[[MSData$active]]$settings$cols  
    rows <- ceiling(length(glist)/cols)
  if(input$RTtoggle | is.null(maintabsel())
     ){
    rtmid <- NULL
    rtx <- NULL
  }else{
    rtx <- data.frame(rtmin = rtmid - MSData$layouts[[MSData$active]]$settings$rtw,
               rtmax = rtmid + MSData$layouts[[MSData$active]]$settings$rtw)
  }  
  if(input$TICtoggle | is.null(maintabsel()) ){
    mzmid <- 100
    mzx <- data.frame(mzmin = 0,
                      mzmax = 1)
    titx <- "TICs"
    tictog <- TRUE
  }else{
    tictog <- FALSE
    mzx <- data.frame(mzmin = mzmid - mzmid*MSData$layouts[[MSData$active]]$settings$ppm*1e-6,
                      mzmax = mzmid + mzmid*MSData$layouts[[MSData$active]]$settings$ppm*1e-6)
    titx <- EICtitles(mzmid, rtmid, MSData$layouts[[MSData$active]]$settings$ppm)
  }  
    
    
    
    EICs <- multiEIC(rawdata= MSData$data,
                          mz = mzx,
                          rt = rtx,
                          rnames = row.names(combino()[maintabsel()$rrng,"mz"]), #major item names
                          byFile = F #if true, table will be sorted by rawfile, otherwise by feature
    )
    


    
    groupPlot(EIClist = EICs,
                                     grouping = glist,
                                     plotProps = list(TIC = tictog, #settings for single plots
                                                      cx = 1,
                                                      colr = colrs,
                                                      xlim = rtx),
                                     compProps = list(mfrow=c(rows,cols), #par options for the composite plot
                                                      oma=c(0,2,4,0),
                                                      xpd=NA, bg="white",
                                                      header =  titx,
                                                      header2 = NULL,
                                                      pdfFile = NULL,
                                                      pdfHi = 6*rows,
                                                      pdfWi = 6*cols,
                                                      cx = 1)
    )
  }
    
}, bg = "white")

mainPlotHeight <- reactive({if(!is.null(MSData$active)){
  paste0(ceiling(length(MSData$layouts[[MSData$active]]$grouping)/MSData$layouts[[MSData$active]]$settings$cols)*400+100,"px")
}
  else{"auto"}
  })

output$mainPlotEICs <- renderUI({
    plotOutput("mainPlotEICsPre", height = mainPlotHeight()#,
               #click = "spec2_click",
               #hover = "mainPlot_hover",
               #dblclick = "mainPlot_dblclick",
               #brush = brushOpts(
                #   id = "mainPlot_brush",
                 #  direction = "x",
                  # resetOnNew = TRUE)
               )
})

observe({
  toggleState(id = "pdfButton", condition = !is.null(MSData$active))
})