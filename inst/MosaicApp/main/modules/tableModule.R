library(rhandsontable)

tableModule <- function(input, output, session, df, editable = T){
  
 # ns <- NS(tag)
#  reactive({
  print(internalValues)
 # observeEvent(input$loadTable$datapath,{internalValues$dframe <- read.table(input$loadgroups$datapath, header=T, sep='\t', stringsAsFactors = F)})
  #observeEvent(df(),{internalValues$dframe <- read.table(input$loadgroups$datapath, header=T, sep='\t', stringsAsFactors = F)})
  
  ##### Render the current grouping table
  output$tableDisplay <- renderRHandsontable({
    if(!is.null(internalValues$dframe)){
    rhandsontable(internalValues$dframe,
                  readOnly = !editable,
                  contextMenu = F,
                  selectCallback = TRUE,
                  #height = rheight,
                  # width = 1000,
                  digits=8,
                  highlightCol = TRUE, highlightRow = TRUE)
  }
  })
  
  ######## Download current grouping table as shown
  output$saveTable <- downloadHandler(filename= function(){paste("Table.tsv")}, 
                                       content = function(file){write.table(hot_to_r(input$tableDisplay)
                                                                            #colstuff$anagroupraw
                                                                            , file, sep = "\t", quote = F,
                                                                            row.names = F
                                       )},
                                       contentType = "text/tab-separated-values")
  
  
  
  return(hot_to_r(input$tableDisplay))
#})
  
  }

tableModuleUI <- function(id){
  
  ns <- NS(id)
  fluidRow(
    
  fileInput(ns('loadTable'), "Upload a table", accept= NULL),
  rHandsontableOutput(ns('tableDisplay')),
  downloadButton(ns("savegroups"),"Save Table")  
  )
}