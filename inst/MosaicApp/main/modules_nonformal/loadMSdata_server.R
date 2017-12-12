##############LOAD RAW FILES MODULE#############
###Load the mzXML files

#### Load raw files from zip file
observeEvent(input$rfileload$datapath,{
    withProgress(message = 'Please wait!', detail = "unzipping files", value = 0.1, {
    exfolder = file.path(dirname(input$rfileload$datapath), gsub("\\.[^.]*$","",input$rfileload$name))

unzip(input$rfileload$datapath, exdir = exfolder )

incProgress(0.5, detail = "loading MS data")
newfiles <- list.files(exfolder, pattern=".mzXML", recursive = TRUE, full.names=T)
newfiles <- newfiles[which(!newfiles %in% MSData$filelist)]
MSData$filelist <- unique(c(MSData$filelist, newfiles))
MSData$data <- loadRawM(newfiles, workers = enabledCores)
temp_rawgrouptable <- data.frame(File = gsub(dirname(input$rfileload$datapath),"",MSData$filelist, ignore.case = T), Group = rep("All_Files", length(MSData$filelist)))
if(is.null(input$rfileload$datapath)){
    MSData$filelist <- unique(c(MSData$filelist, list.files(exfolder, pattern=".mzXML", recursive = TRUE, full.names=T))) ## for local execution, doesnt work yet need Folder selection
    MSData$layouts[["default"]] <- constructRawLayout(temp_rawgrouptable, stem = "")
}else{
    MSData$layouts[["default"]] <- constructRawLayout(temp_rawgrouptable, stem = dirname(input$rfileload$datapath))
}
MSData$index = unique(c("default",MSData$index))
MSData$active = "default"

})
})

##load rawfiles from folder directly on local windows machines
observe({
  toggleState(id = "loadRawFolder", condition = (servermode && activateLocalFiles))
})

toggle(id = "loadRawFolderOffline", condition = (!servermode && Sys.info()['sysname'] == "Windows"))
toggle(id = "loadRawFolder", condition = (servermode))


shinyDirChoose(input, 'loadRawFolder', roots=rootpath)


observeEvent(input$loadRawFolder,{
  check <-  parseDirPath(roots=rootpath, input$loadRawFolder)
  if(length(check)>0){
    
  MSData$localfolders <- c(gsub("\\\\","/",check), MSData$localfolders)
  }
})

observeEvent(input$loadRawFolderOffline,{
  check <-  gsub("\\\\","/",choose.dir())
  if(length(check)>0){
    
    MSData$localfolders <- c(gsub("\\\\","/",check), MSData$localfolders)
  }
})


###
projectData <- reactiveValues(filegroupfiles =NULL,
                              csvfiles = NULL,
                              filegroups = NULL,
                              projectName = paste0("MOSAiC_session_",timeStamp))

###
observeEvent(input$projectLoadOk,{
  
  withProgress(message = 'Please wait!', detail = "loading .csv file", value = 0.3, {

    if(!is.null(input$projectTables)){
  ColumnNames <- gsub("-",".",paste0(basename(projectData$filegroups$File),"__XIC"))
  ColumnNames[which(substring(ColumnNames,1,1) %in% as.character(0:9))] <- paste0("X",ColumnNames[which(substring(ColumnNames,1,1) %in% as.character(0:9))])
  
inputTable$df <- read.csv(projectData$csvfiles[which(basename(projectData$csvfiles) == input$projectTables)],
                          header=T, sep=",", 
                          quote='"', stringsAsFactors = F)
inputTable$tablename <- input$projectTables
if(length(which(colnames(inputTable$df) %in% ColumnNames)==0)){
  inputTable$colrange <- which(colnames(inputTable$df) %in% gsub("__XIC","",ColumnNames))
}else{
  inputTable$colrange <- grep("__XIC",colnames(inputTable$df))}

inputTable$anagroupraw <- data.frame(Column=sort(colnames(inputTable$df)[inputTable$colrange]),
                                     Group = projectData$filegroups$Group[order(ColumnNames)],
                                     stringsAsFactors = F)
}
incProgress(0.5, detail = "loading MS data")
newfiles <- projectData$filegroups$File
newfiles <- newfiles[which(!newfiles %in% MSData$filelist)]
MSData$filelist <- unique(c(MSData$filelist, newfiles))
MSData$data <- c(MSData$data,loadRawM(newfiles, workers = enabledCores))
temp_rawgrouptable <- projectData$filegroups
MSData$layouts[[projectData$projectName]] <- constructRawLayout(temp_rawgrouptable, stem = "")
MSData$index = unique(c(projectData$projectName,MSData$index))
MSData$active = projectData$projectName
})
  removeModal()

})

#### Load raw files from local folder
observeEvent(MSData$localfolders,{
  if(length(MSData$localfolders) > 0){
 

    projectData$filegroupfiles <- list.files(MSData$localfolders[1], pattern="filegroups.csv", recursive = FALSE, full.names=T)
    
    if(length(projectData$filegroupfiles) == 1){
      
      rtfile <- list.files(MSData$localfolders[1], pattern="RTcorr_data.Rdata", recursive = FALSE, full.names=T)
      if(length(rtfile) == 1){
      MSData$RTcorr <- attach(rtfile)$rtx
      
      for(i in 1:length(MSData$RTcorr$noncorr)){
        
        MSData$RTcorr[["rtdiff"]][[i]] <- MSData$RTcorr$noncorr[[i]]-MSData$RTcorr$corr[[i]]
        
      }
      }
      projectData$projectName <- basename(dirname(projectData$filegroupfiles))
      projectData$filegroups <- read.csv(projectData$filegroupfiles, stringsAsFactors = F, header = T)
      projectData$csvfiles <- list.files(MSData$localfolders[1], pattern=".csv$", recursive = TRUE, full.names=T)
      showModal(modalDialog(
        p("You have selected a folder that contaions one specific Mosaic xcms job. Would you like to load its settings?"),
        p("If you do not select a .csv file, only the MS data files will be loaded."),
        selectizeInput("projectTables", "select feature table to load",
                       choices = basename(projectData$csvfiles[which(!basename(projectData$csvfiles) %in% c("camera.csv",
                                                                                                   "centWave.csv",
                                                                                                   "filegroups.csv",
                                                                                                   "group.csv",
                                                                                                   "outputs.csv",
                                                                                                   "peakfilling.csv",
                                                                                                   "retcor.csv",
                                                                                                   "status.csv",
                                                                                                   "peaktable_all_unfilled.csv"
                                                                                                   ))]),
                       multiple = F),
        actionButton("projectLoadOk", "OK"),
        
        title = "Import xcms results",
        "",
        size = "l"
      ))
      
    }else{
      withProgress(message = 'Please wait!', detail = "loading MS data", value = 0.3, {
    incProgress(0.5, detail = "loading MS data")
      
      filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]",
                       "[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
      filepattern <- paste(paste("\\.", filepattern, "$", sep = ""), collapse = "|")
    newfiles <- list.files(MSData$localfolders[1], pattern=filepattern, recursive = TRUE, full.names=T)
    newfiles <- newfiles[which(!newfiles %in% MSData$filelist)]
    MSData$filelist <- unique(c(MSData$filelist, newfiles))
    MSData$data <- c(MSData$data,loadRawM(newfiles, workers = enabledCores))
    temp_rawgrouptable <- data.frame(File = MSData$filelist, Group = rep("All_Files", length(MSData$filelist)))
    MSData$layouts[["default"]] <- constructRawLayout(temp_rawgrouptable, stem = "")
    MSData$index = unique(c("default",MSData$index))
    MSData$active = "default"
    })
  }
  
}
})




output$groupingName <- renderUI({
    textInput("groupingName", "Grouping Name:", value = if(input$groupingEditSelect == "__NEW"){ paste0("Grouping",length(MSData$index)) }
                                                                                            else{  input$groupingEditSelect    })
})

output$groupingEditSelect <- renderUI({
    selectizeInput("groupingEditSelect", "Edit Grouping:", choices = c(MSData$index, "__NEW"), selected = "__NEW")
})

##### Make the default rgrouping table based on groupnames table and currently selected folder
observeEvent(c(MSData$filelist,input$rnamelvl),{
    
    stem <- if(!is.null(input$rfileload$datapath)) {dirname(input$rfileload$datapath)}else{""}
    if (!is.null(MSData$filelist)) {
        File = gsub(stem,"",MSData$filelist, ignore.case = T)
        Group = if(input$rnamelvl ==1){as.character(unname(sapply(sapply(File,strsplit,split = "/"),tail,input$rnamelvl)))}else{
    as.character(unname(apply(sapply(sapply(File,strsplit,split = "/"),tail,input$rnamelvl),2,"[",1)))}
MSData$rawgrouptable <- data.frame(File, Group, stringsAsFactors = F)
}})
##### Render the current rgrouping table
output$rawgrouping <- renderRHandsontable({if(!is.null(MSData$rawgrouptable)){
    rhandsontable(MSData$rawgrouptable)
}
})

######## Download current grouping table as shown
output$savergroups <- downloadHandler(filename= function(){paste("RawGrouping.tsv")}, 
                                      content = function(file){write.table(hot_to_r(input$rawgrouping)
                                                                           #colstuff$anagroupraw
                                                                           , file, sep = "\t",quote = F,
                                                                           row.names = F)},
                                      contentType = "text/tab-separated-values")
#### Load grouping table from file
observeEvent(input$loadrgroups$datapath,{MSData$rawgrouptable <- read.table(input$loadrgroups$datapath, header=T, sep='\t')})

#  When the Groups are confirmed, save the current view in rawstuff and make xcmsRaw objects
#  And also generate corresponding list objects

observeEvent(input$confrgroups,{
    withProgress(message = 'Please wait!', detail = "updating groups", value = 0.3, {
    MSData$rawgrouptable <- data.frame(File = as.character(hot_to_r(input$rawgrouping)$File),
                                                                   Group = as.character(hot_to_r(input$rawgrouping)$Group),
                                                                   stringsAsFactors = F)
if(is.null(input$rfileload$datapath)){
    newfiles <- unique(c(MSData$rawgrouptable$File,MSData$filelist))
  MSData$layouts[[input$groupingName]] <- constructRawLayout(MSData$rawgrouptable, stem = "")
}else{
    newfiles <- unique(c(paste0(dirname(input$rfileload$datapath),MSData$rawgrouptable$File),MSData$filelist))
MSData$layouts[[input$groupingName]] <- constructRawLayout(MSData$rawgrouptable, stem = dirname(input$rfileload$datapath))
}
newfiles <- newfiles[which(!newfiles %in% MSData$filelist)]
MSData$data <- c(MSData$data, loadRawM(newfiles, workers = enabledCores))
MSData$index <- c(MSData$index,input$groupingName)
MSData$active <- input$groupingName
})
})    