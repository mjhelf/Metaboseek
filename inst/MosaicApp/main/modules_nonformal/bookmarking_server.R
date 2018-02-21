

allIds <- c("._bookmark_", "activeTable", "shiny-tab-help", "shiny-tab-XCMSrunpanel", "xcms_loadfolderOffline", "xcms_loadfolder", "xcms_name", "xcms_start", "xcms_settingsLoad", "xcms_settingsLoad_progress", "xcms_settingsDL", "xcms_settingsBox", "2899", "2899", "tab-2899-1", "tab-2899-2", "xcms_selectTab", "xcms_settingstab", "xcms_legend_master", "xcms_statustab", "shiny-tab-exploredata", "PlotOpts", "9396", "9396", "tab-9396-1", "tab-9396-2", "PPMwindow", "plotCols", "TICtoggle", "RTtoggle", "plotYzoom", "plotLw", "MLtoggle", "plotCx", "colorscheme", "tab-9396-3", "massShiftTab", "updateshifts", "savemassShiftTab", "loadmassShift", "loadmassShift_progress", "tab-9396-4", "RtCorrLoad", "RtCorrLoad_progress", "rtcorr", "tab-9396-5", "mz1-mzUI", "tab-9396-6", "projectName", "rootfol", "changeRoot", "EICplots", "8126", "8126", "tab-8126-1", "tab-8126-2", "groupingName", "groupingEditSelect", "rnamelvl", "rawgrouping", "confrgroups", "savergroups", "loadrgroups", "loadrgroups_progress", "tab-8126-3", "RtCorrActive", "ShowSpec", "pdfButton", "groupingActiveSelect", "rfileload", "rfileload_progress", "loadRawFolder", "loadRawFolderOffline", "mainPlotEICs", "adductPlot", "Spec1-specAll", "tab-8126-4", "EIC2-EICout", "Spec2-specAll", "tab-8126-5", "quickplots-gtype", "quickplots-ptype", "quickplots-errorbar", "quickplots-mark", "quickplots-usenorm", "quickplots-pdots", "quickplots-rot", "quickplots-multidata", "quickplots-log10", "quickplots-fplot", "quickplots-info", "TableBox", "7182", "7182", "tab-7182-1", "tab-7182-2", "toggleTabOpts", "header", "sep", "quote", "file1", "file1_progress", "anagroupswitch", "loadgroups", "loadgroups_progress", "anagrouping", "savegroups", "confgroups", "loadLine", "previewH3", "intcols", "preview", "tab-7182-3", "normbutton", "selctrl", "usenormdata", "selAna", "clarainfo", "kclusternum", "analyzebutton", "tab-7182-4", "maintable", "tbButton", "newTable", "tablePage", "tableInfo", "tableSaver", "collapse", "selnormdata", "mainSelGroup", "mainSelgProps", "mainSelsProps", "mainSelIntensities", "mainSelOthers", "mainSortDecreasing", "mainSortToggle", "mainSort", "addFilter", "updateFilter", "Filter1-toggler", "Filter1-colSel", "Filter1-minSel", "Filter1-maxSel", "Filter1-modeSel", "Filter1-txtSel", "Filter1-insider", "Filter2-toggler", "Filter2-colSel", "Filter2-minSel", "Filter2-maxSel", "Filter2-modeSel", "Filter2-txtSel", "Filter2-insider", "Filter3-toggler", "Filter3-colSel", "Filter3-minSel", "Filter3-maxSel", "Filter3-modeSel", "Filter3-txtSel", "Filter3-insider", "Filter4-toggler", "Filter4-colSel", "Filter4-minSel", "Filter4-maxSel", "Filter4-modeSel", "Filter4-txtSel", "Filter4-insider", "Filter5-toggler", "Filter5-colSel", "Filter5-minSel", "Filter5-maxSel", "Filter5-modeSel", "Filter5-txtSel", "Filter5-insider", "shiny-tab-workflow1"
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
    
    state$values$MSData$RTcorr <- MSData$RTcorr
    
    state$values$projectData <- projectData$filegroupfiles
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
    MSData$RTcorr <- state$values$MSData$RTcorr
    
    
    # MSData$filelist is automatically updated on restore, because change of input$rfileload$datapath triggers unzip 
    #& construction of the filelist
})
})