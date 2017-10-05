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
  toggleState(id = "loadRawFolder", condition = (!servermode && Sys.info()['sysname'] == "Windows"))
})

observeEvent(input$loadRawFolder,{
  MSData$localfolders <- c(gsub("\\\\","/",choose.dir()), MSData$localfolders)
})

#### Load raw files from local folder
observeEvent(MSData$localfolders,{
  if(length(MSData$localfolders) > 0){
  withProgress(message = 'Please wait!', detail = "loading MS data", value = 0.3, {

    incProgress(0.5, detail = "loading MS data")
    newfiles <- list.files(MSData$localfolders[1], pattern=".mzXML", recursive = TRUE, full.names=T)
    newfiles <- newfiles[which(!newfiles %in% MSData$filelist)]
    MSData$filelist <- unique(c(MSData$filelist, newfiles))
    MSData$data <- c(MSData$data,loadRawM(newfiles, workers = enabledCores))
    temp_rawgrouptable <- data.frame(File = MSData$filelist, Group = rep("All_Files", length(MSData$filelist)))
    MSData$layouts[["default"]] <- constructRawLayout(temp_rawgrouptable, stem = "")
    MSData$index = unique(c("default",MSData$index))
    MSData$active = "default"
    
  })
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