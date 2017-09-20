# current tester: http://127.0.0.1:6107/?_state_id_=2b008b0e0a760e61
# http://127.0.0.1:6107/?_state_id_=fdccb3808d9ceb09

#cele ppac pre raw: http://127.0.0.1:6107/?_state_id_=b9356011daa3a746
#cele ppac with raw: http://127.0.0.1:6107/?_state_id_=424649266de0d535

#v5 small dataset no featuretable : http://127.0.0.1:7758/?_state_id_=b45877173da8055a

# http://mosaic.bti.cornell.edu/Mosaic/?_state_id_=04c9e3036b6885b0
# pooja data pre analysis http://127.0.0.1:5040/?_state_id_=eba221307374af4e
#with p values http://127.0.0.1:5040/?_state_id_=79131b2ce9ee9aca
#with small datatable http://127.0.0.1:5040/?_state_id_=8d75824e335841a7
#newer, large datatable only pre analysis: http://127.0.0.1:5341/?_state_id_=010eadaf0a40d9aa

#PYclusterstuff pre analysis: http://127.0.0.1:5341/?_state_id_=b5f5287d7066f359


allIds <- c("._bookmark_", "activeTable", "shiny-tab-start", "projectName", "shiny-tab-loaddata",
            "shiny-tab-loadtables", "file1", "file1_progress", "header", "sep", "quote", "ldtbl",
            "confgroups", "anagroupswitch", "loadgroups", "loadgroups_progress", "anagrouping", "savegroups",
            "intcols", "preview", "shiny-tab-rawfiles", "rfileload", "rfileload_progress", "groupingName",
            "groupingEditSelect", "rnamelvl", "rawgrouping", "confrgroups", "savergroups", "loadrgroups",
            "loadrgroups_progress", "shiny-tab-processTableData", "normbutton", "selctrl", "usenormdata",
            "selAna", "analyzebutton", "nonNormalizedPlot-log", "nonNormalizedPlot-dplot", "nonNormalizedPlot-info",
            "NormalizedPlot-log", "NormalizedPlot-dplot", "NormalizedPlot-info", "shiny-tab-exploredata",
            "PlotOpts", "2408", "2408", "tab-2408-1", "tab-2408-2", "PPMwindow", "plotCols", "TICtoggle",
            "RTtoggle", "EICplots", "2039", "2039", "tab-2039-1", "tab-2039-2", "pdfButton", "groupingActiveSelect",
            "mainPlotEICs", "collapse", "maintable", "tbButton", "newTable", "tablePage", "tableInfo", "tableSaver",
            "collapse", "selnormdata", "mainSelGroup", "mainSelgProps", "mainSelsProps", "mainSelIntensities",
            "mainSelOthers", "mainSortDecreasing", "mainSortToggle", "mainSort", "addFilter", "updateFilter",
            "Filter1-toggler", "Filter1-colSel", "Filter1-minSel", "Filter1-maxSel", "Filter1-modeSel",
            "Filter1-txtSel", "Filter1-insider", "Filter2-toggler", "Filter2-colSel", "Filter2-minSel", 
            "Filter2-maxSel", "Filter2-modeSel", "Filter2-txtSel", "Filter2-insider", "Filter3-toggler", 
            "Filter3-colSel", "Filter3-minSel", "Filter3-maxSel", "Filter3-modeSel", "Filter3-txtSel",
            "Filter3-insider", "Filter4-toggler", "Filter4-colSel", "Filter4-minSel", "Filter4-maxSel", 
            "Filter4-modeSel", "Filter4-txtSel", "Filter4-insider", "Filter5-toggler", "Filter5-colSel",
            "Filter5-minSel", "Filter5-maxSel", "Filter5-modeSel", "Filter5-txtSel", "Filter5-insider", 
            "runcode_expr", "runcode_run", "runcode_error", "runcode_errorMsg", "diag"
)

keepIds<- c("projectName", "rfileload", "rfileload_progress")

excludeIds <- allIds[which(!allIds %in% keepIds)]

setBookmarkExclude(excludeIds)
# Save extra values in state$values when we bookmark
onBookmark(function(state) {

  
    state$values$tablestuff$tables <- featureTables$tables
    state$values$tablestuff$index <- featureTables$index
    state$values$tablestuff$active <- featureTables$active
    
    state$values$MSData$layouts <- MSData$layouts
    state$values$MSData$index <- MSData$index
    state$values$MSData$active <- MSData$active
    #state$values$MSData$rawgrouptable <- MSData$rawgrouptable
    #state$values$MSData$filelist <- MSData$filelist
    #state$values$MSData$data <- MSData$data
    
})

# Read values from state$values when we restore


onRestore(function(state) {
  withProgress(message = 'Please wait!', detail = "restoring feature table", value = 0.1, {
    
      ##Restoring Tables
      featureTables$tables <- state$values$tablestuff$tables
    featureTables$index <- state$values$tablestuff$index
    featureTables$active <- state$values$tablestuff$active
    
    #MSData$groupings <- state$values$MSData$groupings
    #MSData$filelist <- state$values$MSData$filelist 
    #MSData$data <- state$values$MSData$data 
    MSData$layouts <- state$values$MSData$layouts
    MSData$index <- state$values$MSData$index
    MSData$active <- state$values$MSData$active
  })  
    })

 # incProgress(0.5, detail = "restoring MS data")
  
onRestored(function(state) {

  withProgress(message = 'Please wait!', detail = "restoring MS data", value = 0.5, {
    
      if(!is.null(input$rfileload$datapath)){
      for(i in names(MSData$layouts)){
        
    MSData$layouts[[i]] <- updateRawLayout(MSData$layouts[[i]], new.stem = dirname(input$rfileload$datapath))
    }
      }else{
          for(i in names(MSData$layouts)){
              
          MSData$filelist <-    unique(c(MSData$filelist, MSData$layouts[[i]]$filelist))
          }
      }
    
    #get groups in alphabetic order

    #make xcmsRaws (time consuming)
    #MSData$data <- loadRaw(MSData$filelist, workers = enabledCores)

    MSData$rawgrouptable <- state$values$MSData$rawgrouptable
    # MSData$filelist is automatically updated on restore, because change of input$rfileload$datapath triggers unzip 
    #& construction of the filelist
})
})