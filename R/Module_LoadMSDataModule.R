#' LoadMSDataModule
#' 
#' 
#' Module for loading MS data into Metaboseek
#' 
#' @inherit MseekModules
#' 
#' @return Nothing, but modifies \code{\link{values}}
#' 
#' @describeIn LoadMSDataModule server logic
#' 
#' @export 
LoadMSDataModule <- function(input,output, session,
                                      values = reactiveValues(projectData = projectData,
                                                              featureTables = featureTables,
                                                              MSData = MSData,
                                                              GlobalOpts = GlobalOpts)
){
  
  ns <- NS(session$ns(NULL))
  
  
  internalValues <- reactiveValues(msLoadTrigger = F)
  
  FileLoader <- callModule(FilePathModule, "loadfiles",
                           filepaths = reactive({values$GlobalOpts$filePaths}),
                           label = "Load files", description= "Select MS data files (only compatible files are visible)",
                           displayFolder = T,
                           selectFile = T,
                           pattern = values$GlobalOpts$filePattern)
  
  FolderLoader <- callModule(FilePathModule, "loadfolder",
                           filepaths = reactive({values$GlobalOpts$filePaths}),
                           label = "Load an entire folder", description= "Select a folder with MS data files",
                           displayFolder = T,
                           selectFile = F,
                           pattern = "")
  
  observeEvent(input$loadMSDButton,{
    
    showModal(modalDialog(
      fluidPage(
        fluidRow(
          column(6,
                 FilePathModuleUI(ns("loadfiles"))),
          column(6,
                 FilePathModuleUI(ns("loadfolder")))
      )
      ),
      title = "Load MS data",
      easyClose = T,
      fade = F,
      size = "m",
      footer = fluidPage(
        
        fluidRow(
          column(6,
                 actionButton(ns("modalConf"), "Confirm selection")),
          column(6,
                 modalButton("Cancel"))
        )
        
      )
    ))
    
    
  })
  
  observeEvent(input$modalConf,{
    
    withProgress(message = 'Please wait!', detail = "Loading MS data", value = 0.8, {
      folderfiles <- if(length(FolderLoader$dir) > 0){list.files(FolderLoader$dir, pattern=.MseekOptions$filePattern, recursive = TRUE, full.names=T)}else{NULL}
      #print(folderfiles)
      newfiles <- unique(c(FileLoader$files$datapath, folderfiles))
     # print(newfiles)
      newfiles <- newfiles[which(!newfiles %in% values$MSData$filelist)]
      values$MSData$data <- c(values$MSData$data,loadRawM(newfiles, workers = values$GlobalOpts$enabledCores))
      values$MSData$filelist <- unique(c(values$MSData$filelist, newfiles))
      
      if(length(values$MSData$filelist)>0){
          
          if(is.null(values$MSData$MSnExp) || (!is.null(values$MSData$MSnExp) 
                                        && any(!values$MSData$filelist %in% as.character(values$MSData$MSnExp@phenoData@data$sampleNames)))){
            
            #will be overwritten every time there is a change in the filelist
            values$MSData$MSnExp <- MSnbase::readMSData(values$MSData$filelist, pdata = NULL, verbose = F,
                                                 centroided. = T,
                                                 smoothed. = NA, mode = "onDisk")
            
           
          }
        }
        

      if(is.null(values$MSData$layouts)){
        temp_rawgrouptable <- data.frame(File = values$MSData$filelist, 
                                         Group = rep("Group 1", length(values$MSData$filelist)),
                                         Group2 = rep("Group 1", length(values$MSData$filelist)),
                                         stringsAsFactors = F)
        values$MSData$layouts[["MS Files"]] <- constructRawLayout(temp_rawgrouptable, stem = "", msnExp = values$MSData$MSnExp)
        values$MSData$index = unique(c("MS Files",values$MSData$index))
        values$MSData$active = "MS Files"
      }else{
        msfNum <- length(grep("MS Files",names(values$MSData$layouts)))
        temp_rawgrouptable <- data.frame(File = values$MSData$filelist,
                                         Group = rep("Group 1", length(values$MSData$filelist)),
                                         Group2 = rep("Group 1", length(values$MSData$filelist)),
                                         stringsAsFactors = F)
        values$MSData$layouts[[paste0("MS Files ",msfNum+1)]] <- constructRawLayout(temp_rawgrouptable, stem = "", msnExp = values$MSData$MSnExp)
        values$MSData$index = unique(c(paste0("MS Files ",msfNum+1),values$MSData$index))
        values$MSData$active = paste0("MS Files ",msfNum+1)
        
      }
      
     removeModal()
    })
    
  })
  
  
}


#' @describeIn LoadMSDataModule UI elements
#' @export 
LoadMSDataModuleUI <- function(id){
  
  ns <- NS(id)
  
  actionButton(ns("loadMSDButton"), "Load MS data", icon = icon("folder-open", lib = "font-awesome"))
  
}