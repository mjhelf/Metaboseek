#' SelectProjectFolderModule
#' 
#' Module for loading a Project Folder
#' 
#' @inherit MseekModules
#' 
#' @return Returns nothing
#' 
#' @examples 
#' \dontrun{
#' library(shiny)
#' 
#' ui <- SelectProjectFolderModuleUI("examplemodule")
#' 
#' server <- function(input, output) {
#'   MseekMinimalServer(diagnostics = F, data = F, tables = F)
#'   
#'   ExampleModule <- callModule(SelectProjectFolderModule, "examplemodule", values)
#' }
#' 
#' Run Shiny app ----
#' shinyApp(ui, server)
#' }
#' 
#' @describeIn SelectProjectFolderModule Server logic
#' 
#' @export 
SelectProjectFolderModule <- function(input,output, session,
                                      values = reactiveValues(projectData = NULL,
                                                              featureTables = NULL,
                                                              MSData = NULL,
                                                              GlobalOpts = NULL)
){
  
  ns <- NS(session$ns(NULL))
  
  
  internalValues <- reactiveValues(msLoadTrigger = F,
                                   getRecent = NULL,
                                   selRecent = .MseekOptions$recentProjects[1]
  )
  
  
  MSFolder <- callModule(FilePathModule, "pfolder",
                         filepaths = reactive({values$GlobalOpts$filePaths}),
                         label = "Load other Project", description= "Select an existing Mseek Project Folder (e.g. with xcms results)",
                         displayFolder = F)
  
  
  AltFileFolder <- callModule(FilePathModule, "altfilefolder",
                              filepaths = reactive({values$GlobalOpts$filePaths}),
                              label = "MS File Folder", description= "Select a Folder containing the appropriate MS data files.",
                              displayFolder = F)
  
  observeEvent(MSFolder$dir,{
    if(length(MSFolder$dir)>0 
       && (is.null(values$projectData$projectFolder) 
           || values$projectData$projectFolder != MSFolder$dir)){
      values$projectData$projectFolder <- MSFolder$dir
      
      internalValues$filegroupsfile <- list.files(values$projectData$projectFolder, pattern="filegroups.csv", recursive = TRUE, full.names=T)
      
      
      if(length(internalValues$filegroupsfile) >0 ){
        
          if(file.exists(file.path(values$projectData$projectFolder, 
                                   pattern="RTcorr_data.Rds"))){
              values$MSData$RTcorr <- readRDS(file.path(values$projectData$projectFolder, 
                                                        "RTcorr_data.Rds"))
              
              for(i in 1:length(values$MSData$RTcorr$noncorr)){
                  
                  values$MSData$RTcorr[["rtdiff"]][[i]] <- values$MSData$RTcorr$noncorr[[i]]-values$MSData$RTcorr$corr[[i]]
                  
              }
              
          }else{
          #for backwards compatibility with old project folders
        rtfile <- list.files(values$projectData$projectFolder, 
                             pattern="RTcorr_data.Rdata",
                             recursive = TRUE, full.names=T)
        if(length(rtfile) == 1){
          values$MSData$RTcorr <- attach(rtfile[1])$rtx
          
          for(i in 1:length(values$MSData$RTcorr$noncorr)){
            
            values$MSData$RTcorr[["rtdiff"]][[i]] <- values$MSData$RTcorr$noncorr[[i]]-values$MSData$RTcorr$corr[[i]]
            
          }
        }
          }
        
        internalValues$filegroups <- read.csv(internalValues$filegroupsfile,
                                              stringsAsFactors = F, header = T)
        
        if(is.null(internalValues$filegroups$Group2)){
          internalValues$filegroups$Group2 <- internalValues$filegroups$Group
        }
        
        
        temp <-  list.files(values$projectData$projectFolder, pattern="\\.csv$|\\.[Mm][Ss][Kk][Ff][Tt]$",
                            recursive =  T, full.names = T)
        
        temp <- temp[!basename(temp) %in% c("camera.csv",
                                            "centWave.csv",
                                            "filegroups.csv",
                                            "group.csv",
                                            "outputs.csv",
                                            "peakfilling.csv",
                                            "retcor.csv",
                                            "status.csv",
                                            "peaktable_all_unfilled.csv",
                                            "analysis_groups.csv",
                                            "filegroups_base.csv")]
        
        tl <- list()
        
        for(i in unique(dirname(temp))){
          tl[[i]] <- as.list(temp[dirname(temp) == i])
          names(tl[[i]]) <- basename(temp[dirname(temp) == i])
        }
        
        names(tl) <- file.path(basename(values$projectData$projectFolder),
                               gsub(values$projectData$projectFolder, "",
                                    names(tl)))
        
        internalValues$fileSelection <- tl
        
        
        showModal(modalDialog(
          p("You have selected a folder that contains one specific Metaboseek xcms job. Would you like to load its results?"),
          p("If you do not select a .csv file, only the MS data files will be loaded."),
          checkboxInput(ns("checkModal"), "Load a Feature Table", value = T),
          selectizeInput(ns("modalSelect"), "select feature table to load",
                         choices = internalValues$fileSelection,
                         multiple = F),
          # div(title = "Try to load Metaboseek intensities (columns with '__XIC' 
          #     suffix rather than xcms provided intensities. 
          #     Recommended if no xcms peak filling was performed. NOTE: If the 
          #     project folder contains a .tGrouping file (Feature Table grouping 
          #     saved by Metaboseek), the pre-saved grouping is loaded.",
          #     checkboxInput(ns("checkMseekIntensities"), 
          #               "Load Metaboseek intensities if available (WARNING: if the selected table has already been analyzed (e.g. calculation of foldChanges during 'Basic Analysis'), make sure this selection is in line with the previous analysis, or reanalyze the table!)",
          #               value = values$GlobalOpts$preferMseekIntensities)),
          # div(title = "Load the .mskFT file instead of the .csv file, if available.
          #              Using .mskFT will provide all previous grouping information and processing history and 
          #              is the preferred option.",
          #     checkboxInput(ns("checkMseekFT"), 
          #                   "Load .mskFT files if available",
          #                   value = TRUE)),
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
      withProgress(message = 'Please wait!',
                   detail = "Importing Feature Table",
                   value = 0.3, {
        
        if(input$checkModal){
          
          tabid <- paste0("table",length(values$featureTables$tables))
          
          if(grepl('\\.[Mm][Ss][Kk][Ff][Tt]$',input$modalSelect)[1]){
              
              values$featureTables$tables[[tabid]] <- loadMseekFT(input$modalSelect)
              values$featureTables$index <- updateFTIndex(values$featureTables$tables)
              values$featureTables$active <- tabid
              
              
              }else{
          
          
          feats <- as.data.frame(data.table::fread(input$modalSelect,
                                                   header = T,
                                                   stringsAsFactors = F,
                                                   quote = '"',
                                                   sep = ","),
                                 stringsAsFactors = F)
          
          # for all columns that are of type logical and only contain NAs, assume they are mutilated empty character strings
          # and a victim of type.convert - make them character vectors again
          charCols <- sapply(feats,typeof) == "logical" & sapply(lapply(feats,is.na),all)
          if(any(charCols)){
            feats[,charCols] <- character(nrow(feats))
          }
          
          if(length(grep("__XIC$",colnames(feats)))){
              ColumnNames <- paste0(basename(internalValues$filegroups$File),"__XIC")
              ColumnNames2 <- ColumnNames
              
              
          }else{
              ColumnNames <- gsub("-",
                                  ".",
                                  basename(internalValues$filegroups$File)) 
            ColumnNames2 <- ColumnNames
          ColumnNames2[which(substring(ColumnNames2,1,1) %in% as.character(0:9))] <- paste0("X",ColumnNames2[which(substring(ColumnNames2,1,1) %in% as.character(0:9))])
            
              }
         
          
          
          if(length(grep("__XIC$",colnames(feats)))){
          intColRange <- grep("__XIC$",colnames(feats))
          }else{
            intColRange <- grep(values$GlobalOpts$filePattern,colnames(feats))
          }
          
          #look for a .tGrouping file in same folder as table and load it
          if(file.exists(gsub("\\.csv$",".tGrouping",input$modalSelect))){
            anagroup <-read.delim(gsub("\\.csv$",
                                       ".tGrouping",
                                       input$modalSelect),
                                  stringsAsFactors = F,
                                  header = T, sep = "\t")
          }
          #look for a .tGrouping file for this table in the entire project folder and load the first match
          else if(length(list.files(values$projectData$projectFolder,
                                    pattern = gsub("\\.csv$",
                                                   ".tGrouping",
                                                   basename(input$modalSelect)),
                                    recursive = T, full.names = T)) > 0){
            anagroup <- read.delim(list.files(values$projectData$projectFolder,
                                              pattern = gsub("\\.csv$",
                                                             ".tGrouping",
                                                             basename(input$modalSelect)),
                                              recursive = T, full.names = T)[1], stringsAsFactors = F, header = T, sep = "\t")
          }
          #backwards compatibility
          else if(file.exists(file.path(values$projectData$projectFolder,
                                        "analysis_groups.csv"))){
            anagroup <- read.csv(file.path(values$projectData$projectFolder,
                                           "analysis_groups.csv"), 
                                 stringsAsFactors = F, header = T)
            
          }else if(all(ColumnNames %in% colnames(feats))){
            anagroup <- data.frame(Column=ColumnNames,
                                   Group = internalValues$filegroups$Group,
                                   stringsAsFactors = F)
          }else if(all(ColumnNames2 %in% colnames(feats))){
            anagroup <- data.frame(Column=ColumnNames2,
                                   Group = internalValues$filegroups$Group,
                                   stringsAsFactors = F)
          }
          #no __XIC intColrange found? No automatic grouping suggestion
          else if(length(intColRange)==0){
            anagroup <- NULL
          }else{
            
            anagroup <- data.frame(Column=colnames(feats)[intColRange],
                                   Group = rep("G1",(length(intColRange))),
                                   stringsAsFactors = F)
          }
          incProgress(0.3, detail = "Formatting Feature Table")
          
          values$featureTables$tables[[tabid]] <- buildMseekFT(feats,
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
        }
        removeModal()
        
        
        incProgress(0.1, detail = "loading MS data")
        
        if(!all(file.exists(internalValues$filegroups$File))){
          internalValues$missingFiles <-  internalValues$filegroups$File[!file.exists(internalValues$filegroups$File)]
          showModal(modalDialog(
            p("These MSData files were not found:"),
            p(paste(internalValues$missingFiles, collapse = ", ")),
            p("Please select a folder that contains these files. Mseek will search all subfolders for matching filenames and update the information in this project folder for the next time you load it."),
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
  
  # observeEvent(input$checkMseekIntensities,{
  #   values$GlobalOpts$preferMseekIntensities <- input$checkMseekIntensities
  # })
  
  observeEvent(c(AltFileFolder$dir),{
    #print(AltFileFolder$dir)
    # print("dirTrigger")
    if(length(AltFileFolder$dir) > 0){
      
      allFiles <- list.files(AltFileFolder$dir, 
                             pattern=.MseekOptions$filePattern, 
                             recursive = TRUE, full.names=T)
      
      allHits <- sapply(basename(internalValues$missingFiles),
                        grep, x = allFiles, value = T)
      if(is.list(allHits)){
        allHits <- unlist(allHits[which(sapply(allHits, length) != 0)])
      }
      
      if(length(allHits > 0)){
        
        singleHits <- allHits[!duplicated(basename(allHits))]
        
        replaceThese <- sapply(basename(singleHits), grep,
                               x = basename(internalValues$filegroups$File), 
                               value = F)
        
        internalValues$filegroups$File[replaceThese] <-  singleHits}
      
      try({
        #print("writing files")
        #override old filegroups with updated file paths and make a backup of the old file
        file.copy(internalValues$filegroupsfile, 
                  gsub("\\.csv$", "_original.csv", 
                       internalValues$filegroupsfile))
        write.csv(internalValues$filegroups, 
                  file = internalValues$filegroupsfile,
                  row.names = F)
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
    if(is.null(input$modalIgnore) 
       || (input$modalIgnore > 0 
           || length(AltFileFolder$dir) > 0)){
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
        
        withProgress(message = 'Please wait!', detail = "Loading MS data", value = 0, {
          newfiles <- internalValues$filegroups$File
          newfiles <- newfiles[which(!newfiles %in% values$MSData$filelist)]
          values$MSData$filelist <- unique(c(values$MSData$filelist, newfiles))
          values$MSData$data <- c(values$MSData$data,loadRawM(newfiles, workers = values$GlobalOpts$enabledCores))
          
          if(length(values$MSData$filelist)>0){
            
            if(is.null(values$MSData$MSnExp) || (!is.null(values$MSData$MSnExp) 
                                                 && any(!values$MSData$filelist %in% as.character(values$MSData$MSnExp@phenoData@data$sampleNames)))){
              
              setProgress(value = 0.3, message = "Generating OnDiskMSnExp object...")
              #will be overwritten every time there is a change in the filelist
              values$MSData$MSnExp <- MSnbase::readMSData(values$MSData$filelist, pdata = NULL, verbose = F,
                                                   centroided. = T,
                                                   smoothed. = NA, mode = "onDisk")
              
              
            }
          }
          
          temp_rawgrouptable <- internalValues$filegroups #probably to avoid feeding a pointer into constructRawLayout
          values$MSData$layouts[[basename(values$projectData$projectFolder)]] <- constructRawLayout(temp_rawgrouptable, stem = "", msnExp = values$MSData$MSnExp)
          values$MSData$index = unique(c(basename(values$projectData$projectFolder),values$MSData$index))
          values$MSData$active =basename(values$projectData$projectFolder)
          values$GlobalOpts$recentProjects <- as.character(na.omit(unique(c(values$projectData$projectFolder, values$GlobalOpts$recentProjects))[1:10]))
          MseekOptions(recentProjects = values$GlobalOpts$recentProjects)
        })
      }
    }
  }, ignoreInit = T)
  
  output$selrecent <- renderUI({
    
    
  })
  output$selrecent <- renderUI({
    if(!is.null(values$GlobalOpts$recentProjects)){
      folist <-  as.list(values$GlobalOpts$recentProjects)
      names(folist) <- basename(values$GlobalOpts$recentProjects)
      
      div(title = internalValues$selRecent,
          # fluidRow(
          #   column(8,
          selectizeInput(ns("selRecent"),"Recent projects: ", 
                         choices= folist,
                         selected = internalValues$selRecent,
                         width = "100%"
          )
          #),
          #)
          
      )
      
    }
  })
  observeEvent(input$selRecent,{
    internalValues$selRecent <- input$selRecent
    
  })
  
  observeEvent(input$loadRecent,{
    MSFolder$dir <- input$selRecent
    
  })
  
}

#' @describeIn SelectProjectFolderModule UI elements
#' @export 
SelectProjectFolderModuleUI <- function(id){
  
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns('selrecent')),
    fluidRow(
      
      column(6,
             div(title = "Load the selected recent Project",
             actionButton(ns("loadRecent"),"Load recent"))
      ),
      column(6,
             FilePathModuleUI(ns("pfolder")))
    ))
}