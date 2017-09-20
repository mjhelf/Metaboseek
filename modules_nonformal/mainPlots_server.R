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
                                                   TICall = input$TICtoggle
                                        )
                                      },
                                    
                                    
                                    
                                    contentType = "application/pdf")




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
                 leadingTIC = F
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