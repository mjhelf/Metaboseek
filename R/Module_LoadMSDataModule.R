#' LoadMSDataModule
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
      folderfiles <- if(length(FolderLoader$dir) > 0){list.files(FolderLoader$dir, pattern=.MosaicOptions$filePattern, recursive = TRUE, full.names=T)}else{NULL}
      print(folderfiles)
      newfiles <- unique(c(FileLoader$files$datapath, folderfiles))
      print(newfiles)
      newfiles <- newfiles[which(!newfiles %in% values$MSData$filelist)]
      values$MSData$filelist <- unique(c(values$MSData$filelist, newfiles))
      values$MSData$data <- c(values$MSData$data,loadRawM(newfiles, workers = values$GlobalOpts$enabledCores))
      
      if(is.null(values$MSData$layouts)){
        temp_rawgrouptable <- data.frame(File = values$MSData$filelist, Group = rep("Group 1", length(values$MSData$filelist)))
        values$MSData$layouts[["MS Files"]] <- constructRawLayout(temp_rawgrouptable, stem = "")
        values$MSData$index = unique(c("MS Files",values$MSData$index))
        values$MSData$active = "MS Files"
        
      }
      
     
    })
    
  })
  
  
}


#' LoadMSDataModuleUI
#' 
#' 
#' server module for loading Tables
#' 
#' @param id
#' 
#' @export 
LoadMSDataModuleUI <- function(id){
  
  ns <- NS(id)
  
  actionButton(ns("loadMSDButton"), "Load MS data", icon = icon("folder-open", lib = "font-awesome"))
  
}