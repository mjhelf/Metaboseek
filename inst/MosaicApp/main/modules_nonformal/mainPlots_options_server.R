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

output$plotYzoom <- renderUI({
  numericInput("plotYzoom","Y-axis zoom: ", value = 1, min = 0.1)
})

output$plotLw <- renderUI({
  numericInput("plotLw","Line width: ", value = 1, min = 1, step = 1)
})

output$MLtoggle <- renderUI({
  checkboxInput("MLtoggle","Mark feature RT", value = T)
})

output$plotCx <- renderUI({
  numericInput("plotCx","Font size: ", value = 1.5, min = 0.1, step = 0.1)
})

output$colorscheme <- renderUI({
  selectizeInput("colorscheme","Color palette: ", 
                 choices= c("mosaic.colors", "topo.colors", "rainbow", "heat.colors", "terrain.colors", "cm.colors"),
                 selected = NULL
                 )
})
observeEvent(input$colorscheme,{
  if(!is.null(MSData$active)){
    MSData$layouts[[MSData$active]]$settings$colr <- input$colorscheme
  }
})


#####Mass shift table

massShifts <- reactiveValues(table = data.frame(use = c(T,F,F,F,F,F),
                                                Name = c("unmodified", "-H2O", "+13C iso", character(3)), 
                                                Mol_formula = character(6),
                                                charge = rep(1,6), 
                                                mz_shift = c(0, -18.0105646837, 1.003355, numeric(3)),
                                                stringsAsFactors = F) )

output$massShiftTab <- renderRHandsontable({if(!is.null(massShifts$table)){
  rhandsontable(massShifts$table,
                digits = 9)%>%
  hot_col("mz_shift", format="0.000000")
}
})

######## Download current grouping table as shown
output$savemassShiftTab <- downloadHandler(filename= function(){paste("massShifts.tsv")}, 
                                     content = function(file){write.table(hot_to_r(input$massShiftTab)
                                                                          #colstuff$anagroupraw
                                                                          , file, sep = "\t", quote = F,
                                                                          row.names = F
                                     )},
                                     contentType = "text/tab-separated-values")

# onRestored(function(state){
#### Load grouping table from file
observeEvent(input$loadmassShift$datapath,{massShifts$table <- read.table(input$loadmassShift$datapath, header=T, sep='\t', stringsAsFactors = F)})

###########################
##RT correction

observeEvent(input$RtCorrLoad$datapath,{
  print(input$RtCorrLoad$datapath)
  MSData$RTcorr <- attach(input$RtCorrLoad$datapath)$rtx
  
  for(i in 1:length(MSData$RTcorr$noncorr)){
    
    MSData$RTcorr[["rtdiff"]][[i]] <- MSData$RTcorr$noncorr[[i]]-MSData$RTcorr$corr[[i]]
    
  }
  
})

output$rtcorr <- renderPlot({
  if(!is.null(MSData$RTcorr)){
  RTplot(MSData$RTcorr,
         colscheme = input$colorscheme,
         liwi =  2* input$plotLw,
         cx = input$plotCx)
  }
  
})


#########
## MF prediction
callModule(MzqueryModule,"mz1", tag = "mz1", 
           set = reactive({list(search = list(elements = "C0-100H0-202N0-15O0-20",
                                              mz = list("feature table" = if(is.null(maintabsel())){NULL}else{hot_to_r(input$maintable)[maintabsel()$rrng[1],"mz"]},
                                                        "spectrum" = iSpec1()$spec$marker$mz#,
#                                                        "interactive view" = iSpec2()$spec$marker$mz
                                                        ), 
                                              data = iSpec1()$spec$data
                                              ) # the entire spectrum data for isotope matching
           )})
)

GlobalOpts <- callModule(GlobalOptionsModule, "globalopts")


