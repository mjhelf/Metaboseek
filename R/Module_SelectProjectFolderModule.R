#' SelectProjectFolderModule
#' 
#' 
#' server module for loading a Project Folder
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
SelectProjectFolderModule <- function(input,output, session,
                            values = reactiveValues(projectData = projectData,
                                                    featureTables = featureTables,
                                                    MSData = MSData,
                                                    GlobalOpts = GlobalOpts)
){
  
  ns <- NS(session$ns(NULL))
  
  
  internalValues <- reactiveValues(msLoadTrigger = F)
  
  
  MSFolder <- callModule(FilePathModule, "pfolder",
                         filepaths = reactive({values$GlobalOpts$filePaths}),
                         label = "Load Project Folder", description= "Select an existing Mosaic Project Folder (e.g. with xcms results)",
                         displayFolder = F)
 
  
  AltFileFolder <- callModule(FilePathModule, "altfilefolder",
                         filepaths = reactive({values$GlobalOpts$filePaths}),
                         label = "MS File Folder", description= "Select a Folder containing the appropriate MS data files.",
                         displayFolder = F)
  
  observeEvent(MSFolder$dir,{
    if(length(MSFolder$dir)>0 && (is.null(values$projectData$projectFolder) || values$projectData$projectFolder != MSFolder$dir)){
      values$projectData$projectFolder <- MSFolder$dir
      
      internalValues$filegroupsfile <- list.files(values$projectData$projectFolder, pattern="filegroups.csv", recursive = TRUE, full.names=T)
      
      
      if(length(internalValues$filegroupsfile) >0 ){
        
        rtfile <- list.files(values$projectData$projectFolder, pattern="RTcorr_data.Rdata", recursive = TRUE, full.names=T)
        if(length(rtfile) == 1){
          values$MSData$RTcorr <- attach(rtfile[1])$rtx
          
          for(i in 1:length(values$MSData$RTcorr$noncorr)){
            
            values$MSData$RTcorr[["rtdiff"]][[i]] <- values$MSData$RTcorr$noncorr[[i]]-values$MSData$RTcorr$corr[[i]]
            
          }
        }
        #projectData$projectName <- basename(dirname(projectData$filegroupfiles))
        internalValues$filegroups <- read.csv(internalValues$filegroupsfile, stringsAsFactors = F, header = T)

        temp <-  list.files(values$projectData$projectFolder, pattern="\\.csv$", recursive =  T, full.names = T)
        
        temp <- temp[!basename(temp) %in% c("camera.csv",
                                                              "centWave.csv",
                                                              "filegroups.csv",
                                                              "group.csv",
                                                              "outputs.csv",
                                                              "peakfilling.csv",
                                                              "retcor.csv",
                                                              "status.csv",
                                                              "peaktable_all_unfilled.csv")]
        
        tl <- list()
        
        for(i in unique(dirname(temp))){
          tl[[i]] <- as.list(temp[dirname(temp) == i])
          names(tl[[i]]) <- basename(temp[dirname(temp) == i])
        }
        
        names(tl) <- file.path(basename(values$projectData$projectFolder), gsub(values$projectData$projectFolder, "", names(tl)))
        
        internalValues$fileSelection <- tl
        
        
        showModal(modalDialog(
          p("You have selected a folder that contaions one specific Mosaic xcms job. Would you like to load its settings?"),
          p("If you do not select a .csv file, only the MS data files will be loaded."),
          checkboxInput(ns("checkModal"), "Load a Feature Table", value = T),
          selectizeInput(ns("modalSelect"), "select feature table to load",
                         choices = internalValues$fileSelection,
                         multiple = F),
          actionButton(ns("projectLoadOk"), "OK"),
          
          title = "Import xcms results",
          size = "l"
        ))
        
      }
      else{
        showNotification(paste("ERROR: The selected folder does not contain a filegroups.csv file"), type = "error", duration = 10)
        
      }
    }
  })
  
  observeEvent(input$projectLoadOk,{
    tryCatch({
    withProgress(message = 'Please wait!', detail = "Importing Feature Table", value = 0.3, {
      
      if(input$checkModal){
      
      tabid <- paste0("table",length(values$featureTables$tables))
      
      feats <- as.data.frame(data.table::fread(input$modalSelect,
                                               header = T,
                                               stringsAsFactors = F,
                                               quote = '"',
                                               sep = ","),
                             stringsAsFactors = F)
     
      ColumnNames <- gsub("-",".",paste0(basename(internalValues$filegroups$File),"__XIC"))
      
      ColumnNames2 <- ColumnNames
        ColumnNames2[which(substring(ColumnNames2,1,1) %in% as.character(0:9))] <- paste0("X",ColumnNames2[which(substring(ColumnNames2,1,1) %in% as.character(0:9))])

      intColRange <- grep("__XIC$",colnames(feats))
      #look for a .tGrouping file in same folder as table and load it
      if(file.exists(gsub("\\.csv$",".tGrouping",input$modalSelect))){
        anagroup <-read.delim(gsub("\\.csv$",".tGrouping",input$modalSelect), stringsAsFactors = F, header = T, sep = "\t")
      }
      #look for a .tGrouping file for this table in the entire project folder and load the first match
      else if(length(list.files(values$projectData$projectFolder, pattern = gsub("\\.csv$",".tGrouping",basename(input$modalSelect)), recursive = T, full.names = T)) > 0){
        anagroup <- read.delim(list.files(values$projectData$projectFolder, pattern = gsub("\\.csv$",".tGrouping",basename(input$modalSelect)), recursive = T, full.names = T)[1], stringsAsFactors = F, header = T, sep = "\t")
      }
      #backwards compatibility
      else if(file.exists(file.path(values$projectData$projectFolder, "analysis_groups.csv"))){
        anagroup <- read.csv(file.path(values$projectData$projectFolder, "analysis_groups.csv"), stringsAsFactors = F, header = T)
        
      }else if(all(ColumnNames %in% colnames(feats))){
      
          anagroup <- data.frame(Column=ColumnNames,
                                 Group = internalValues$filegroups$Group,
                                 stringsAsFactors = F)
      }else if(all(ColumnNames2 %in% colnames(feats))){
        
        anagroup <- data.frame(Column=ColumnNames2,
                               Group = internalValues$filegroups$Group,
                               stringsAsFactors = F)
      }
      #no __XIC columns? No automatic grouping suggestion
      else if(length(intColRange)==0){
        
        anagroup <- NULL
        
      }
      
      else{
        
        anagroup <- data.frame(Column=colnames(feats)[intColRange],
                               Group = rep("G1",(length(intColRange))),
                               stringsAsFactors = F)
      }
      incProgress(0.3, detail = "Formatting Feature Table")
      
      values$featureTables$tables[[tabid]] <- constructFeatureTable(feats,
                                                                    mzcol= "mz", #column in df with mz values (columnname)
                                                                    rtcol= "rt", #column in df with mz values (columnname)
                                                                    commentcol = "comments",
                                                                    fragmentcol = "fragments",
                                                                    rtFormat = "sec", # "sec" or "min" 
                                                                    anagrouptable = anagroup,
                                                                    tablename = basename(input$modalSelect),
                                                                    editable = F)
      values$featureTables$index <- updateFTIndex(values$featureTables$tables)
      values$featureTables$active <- tabid
   
    }
      removeModal()
      
   
      incProgress(0.1, detail = "loading MS data")
      
      if(!all(file.exists(internalValues$filegroups$File))){
        internalValues$missingFiles <-  internalValues$filegroups$File[!file.exists(internalValues$filegroups$File)]
        showModal(modalDialog(
          p("These MSData files were not found:"),
          p(paste(internalValues$missingFiles, collapse = ", ")),
          p("Please select a folder that contains these files. Mosaic will search all subfolders for matching filenames and update the information in this project folder for the next time you load it."),
          FilePathModuleUI(ns("altfilefolder")),
          
          title = "File locations have changed",
          easyClose = F,
          fade = F,
          size = "m",
          footer = actionButton(ns("modalIgnore"), "Ignore") 
        ))
         
      }else{
      
        internalValues$msLoadTrigger <- !internalValues$msLoadTrigger
      }
    })
    
    },
    error = function(e) {
      showNotification(paste("ERROR: Something went wrong, see console for details!"), type = "error", duration = 15)
      print(e)
    })
    
    
  })
   
  
  observeEvent(c(AltFileFolder$dir),{
    #print(AltFileFolder$dir)
   # print("dirTrigger")
    if(length(AltFileFolder$dir) > 0){
      
      allFiles <- list.files(AltFileFolder$dir, pattern=.MosaicOptions$filePattern, recursive = TRUE, full.names=T)
      
      allHits <- sapply(basename(internalValues$missingFiles), grep, x = allFiles, value = T)
      if(is.list(allHits)){
        allHits <- unlist(allHits[which(sapply(allHits, length) != 0)])
      }
      
      if(length(allHits > 0)){
        
        singleHits <- allHits[!duplicated(basename(allHits))]
        
        replaceThese <- sapply(basename(singleHits), grep, x = basename(internalValues$filegroups$File), value = F)
        
        internalValues$filegroups$File[replaceThese] <-  singleHits}
      
      try({
        #print("writing files")
        #override old filegroups with updated file paths and make a backup of the old file
        file.copy(internalValues$filegroupsfile, gsub("\\.csv$", "_original.csv", internalValues$filegroupsfile))
        write.csv(internalValues$filegroups, file = internalValues$filegroupsfile, row.names = F)
      })
      
      internalValues$msLoadTrigger <- !internalValues$msLoadTrigger
      
    }
    
      
       
   
    
  })
  
  observeEvent(c(input$modalIgnore),{
 # print(input$modalIgnore)
    print("ignoremodal triggered")
    }, ignoreInit = T)
  
  observeEvent(c(internalValues$msLoadTrigger),{
    
   # print("msLoadTrigger triggered")
  }, ignoreInit = T)
  
  observeEvent(c(internalValues$msLoadTrigger,  input$modalIgnore),{
    if(is.null(input$modalIgnore) || (input$modalIgnore > 0 || length(AltFileFolder$dir) > 0)){
    if(!is.null(is.null(input$modalIgnore))){
    removeModal()
    }
    
    
    if(!all(file.exists(internalValues$filegroups$File))){
      
      showNotification(paste0("The following files were not found and could not be loaded: ",
                              paste0(basename(internalValues$filegroups$File[!file.exists(internalValues$filegroups$File)]),
                                     collapse = ", ")), type = "error", duration = 0)
     # print("updating filegroups")
      
      internalValues$filegroups <- internalValues$filegroups[file.exists(internalValues$filegroups$File), ]
    }
    if(length(internalValues$filegroups$File) > 0 ){
    #  print("loading MS data")

      withProgress(message = 'Please wait!', detail = "Loading MS data", value = 0.8, {
        newfiles <- internalValues$filegroups$File
        newfiles <- newfiles[which(!newfiles %in% values$MSData$filelist)]
        values$MSData$filelist <- unique(c(values$MSData$filelist, newfiles))
        values$MSData$data <- c(values$MSData$data,loadRawM(newfiles, workers = values$GlobalOpts$enabledCores))
        temp_rawgrouptable <- internalValues$filegroups #probably to avoid feeding a pointer into constructRawLayout
        values$MSData$layouts[[basename(values$projectData$projectFolder)]] <- constructRawLayout(temp_rawgrouptable, stem = "")
        values$MSData$index = unique(c(basename(values$projectData$projectFolder),values$MSData$index))
        values$MSData$active =basename(values$projectData$projectFolder)
      })
    }
}
  }, ignoreInit = T)
  
}

#' SelectProjectFolderModuleUI
#' 
#' 
#' server module for loading Tables
#' 
#' @param id
#' 
#' @export 
SelectProjectFolderModuleUI <- function(id){
  
  ns <- NS(id)
 
  FilePathModuleUI(ns("pfolder"))
   
}