#' xcmsModule
#' 
#' server module for accessing the xcms data analysis workflow
#' 
#' @inherit MseekModules
#' @describeIn xcmsModule Server logic
#' 
#' @return Returns nothing
#' 
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs show toggle toggleState
#' @import shinyBS
#' @import shinyFiles
#' @import rhandsontable
#' 
#' @export 
xcmsModule <- function(input,output, session,
                   values = reactiveValues(),
                   static = list(servermode = F,
                                 rootpath = .MseekOptions$filePaths,
                                 activateXCMS = T,
                                 filePattern = .MseekOptions$filePattern)
){
  
 ns <- NS(session$ns(NULL))
 
 internalStatic <- c(list(Mversion =  1),
                      static)
  
 
internalValues <- reactiveValues(params = list(filegroups = data.frame(File = character(1), Groups = character(1), stringsAsFactors = F),
                                             centWave = read.csv(system.file("config", "xcms_defaults", "centWave.csv",package = "METABOseek"),
                                                                 row.names = 1,
                                                                 stringsAsFactors = F),
                                             group = read.csv(system.file("config", "xcms_defaults", "group.csv",package = "METABOseek"),
                                                              row.names = 1,
                                                              stringsAsFactors = F),
                                             retcor = read.csv(system.file("config", "xcms_defaults", "retcor.csv",package = "METABOseek"),
                                                               row.names = 1,
                                                               stringsAsFactors = F),
                                             outputs = read.csv(system.file("config", "xcms_defaults", "outputs.csv",package = "METABOseek"),
                                                                row.names = 1,
                                                                stringsAsFactors = F),
                                             peakfilling = read.csv(system.file("config", "xcms_defaults", "peakfilling.csv",package = "METABOseek"),
                                                                    row.names = 1,
                                                                    stringsAsFactors = F),
                                             camera = read.csv(system.file("config", "xcms_defaults", "camera.csv",package = "METABOseek"),
                                                               row.names = 1,
                                                               stringsAsFactors = F)
),
wd = character(),
active = "centWave",
jobs = NULL,
viewjob = NULL,
xcmsModule_loaded = F,
noRtCorrAnaCheck = T,
rtCorrAnaCheck = T)

observeEvent(values$MSData$layouts[[values$MSData$active]]$rawgrouptable,{
  
  #if raw files are loaded into the MS viewer, load them in here as well
  if(length(values$MSData$layouts[[values$MSData$active]]$rawgrouptable) >0 
     &&  is(values$MSData$layouts[[values$MSData$active]]$rawgrouptable,"data.frame") 
     && !internalValues$xcmsModule_loaded #only do this if loadFolder button in xcms module hasnt been used yet
     ){
    internalValues$params$filegroups <- values$MSData$layouts[[values$MSData$active]]$rawgrouptable[,c("File", "Group")]
    internalValues$params$filegroups$File <- as.character(internalValues$params$filegroups$File)
    internalValues$params$filegroups$Group <- as.character(internalValues$params$filegroups$Group)
    internalValues$wd <- get_common_dir(internalValues$params$filegroups$File)
    internalValues$active <- "filegroups"
    
  }
})

observeEvent(input$xcms_settingsLoad$datapath,{
  
  exfolder = file.path(dirname(input$xcms_settingsLoad$datapath), gsub("\\.[^.]*$","",input$xcms_settingsLoad$name))
  
  unzip(input$xcms_settingsLoad$datapath, exdir = exfolder )
  
  newfiles <- list.files(exfolder, pattern=".csv", recursive = TRUE, full.names=T)
  
  for( i in newfiles){
    internalValues$params[[gsub("\\.[^.]*$","",basename(i))]] <- read.csv(i,
                                                                        row.names = 1,
                                                                        stringsAsFactors = F)
  }
  
  if(file.exists(file.path(exfolder, "postProcessingSettings.json"))){
  ppOptions <- jsonlite::unserializeJSON(readChar(file.path(exfolder, "postProcessingSettings.json"),
                                     file.info(file.path(exfolder, "postProcessingSettings.json"))$size))
  
  for(i in names(ppOptions)){
    if(i %in% c("rtCorrAnaCheck", "noRtCorrAnaCheck")){
      internalValues[[i]] <- ppOptions[[i]]
    }else{
      tAnalysisX[[i]] <- ppOptions[[i]]
    }
    
  }
  }
  
  internalValues$wd <- get_common_dir(internalValues$params$filegroups$File)
  
  internalValues$xcmsModule_loaded <- T
  
  #if an old outputs.csv file is loaded, replace it with the new default.
  if(ncol(internalValues$params$outputs) < 5) {
    internalValues$params$outputs <- read.csv(system.file("config", "xcms_defaults", "outputs.csv",package = "METABOseek"),
                                            row.names = 1,
                                            stringsAsFactors = F)
  }
})


output$xcms_settingsDL <- downloadHandler(filename= function(){paste("settings.zip")}, 
                                          content = function(file){
                                            
                                            
                                            flist = paste0(names(internalValues$params),".csv")
                                            for(i in 1:length(internalValues$params)){
                                              write.csv(internalValues$params[[i]], file = flist[i], row.names = T)
                                            }
                                            
                                            posSettings <- reactiveValuesToList(tAnalysisX)
                                            posSettings$rtCorrAnaCheck <- internalValues$rtCorrAnaCheck
                                            posSettings$noRtCorrAnaCheck <- internalValues$noRtCorrAnaCheck
                                            
                                            write(jsonlite::serializeJSON(posSettings, pretty = T), "postProcessingSettings.json")
                                            flist <- c(flist,"postProcessingSettings.json")
                                            

                                            zip(file, flist, flags = "-j")
                                            if(file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
                                          },
                                          contentType = "application/zip")


toggle(id ="xcms_loadfolderOffline", condition = (!internalStatic$servermode && Sys.info()['sysname'] == "Windows"))
toggle(id = "xcms_loadfolder", condition = ((internalStatic$servermode) || (!internalStatic$servermode && Sys.info()['sysname'] != "Windows")))

observe({
  toggleState(id = "xcms_loadfolder", condition = ((internalStatic$servermode && internalStatic$activateXCMS) || (!internalStatic$servermode && Sys.info()['sysname'] != "Windows")))
  toggleState(id = "xcms_start", condition = (length(internalValues$wd)>0 && (!internalStatic$servermode || (internalStatic$servermode && internalStatic$activateXCMS))))
})

shinyDirChoose(input, 'xcms_loadfolder', session = session, roots=internalStatic$rootpath)


observeEvent(input$xcms_loadfolder,{
  fol <-  parseDirPath(roots=internalStatic$rootpath, input$xcms_loadfolder)
  if(length(fol)>0 &&!is.na(fol)){
    #taken from xcms package
    
    flist = list.files(fol, pattern=internalStatic$filePattern, recursive = TRUE, full.names=T)
    internalValues$params$filegroups <- data.frame(File = flist, Group = rep("G1", length(flist)), stringsAsFactors = F)
    internalValues$wd <- fol
    internalValues$active <- "filegroups"
    internalValues$xcmsModule_loaded <- T
  }
})

observeEvent(input$xcms_loadfolderOffline,{
  fol <- gsub("\\\\","/",choose.dir())
  if(length(fol)>0 &&!is.na(fol)){
    #taken from xcms package
    flist = list.files(fol, pattern=internalStatic$filePattern, recursive = TRUE, full.names=T)
    internalValues$params$filegroups <- data.frame(File = flist, Group = rep("G1", length(flist)), stringsAsFactors = F)
    internalValues$wd <- fol
    internalValues$active <- "filegroups"
    internalValues$xcmsModule_loaded <- T
  }
  
})

output$xcms_selectTab <- renderUI({selectizeInput(ns('xcms_selectTab'),"Change settings for...", 
                                                  choices = list("File Grouping" = "filegroups",
                                                                 "Peak Detection" = "centWave",
                                                                 "Peak filling" = "peakfilling",
                                                                 "Feature grouping" = "group",
                                                                 "CAMERA settings" = "camera",
                                                                 "RT correction" = "retcor",
                                                                 "Output Files" = "outputs"),
                                                  selected = internalValues$active
)})

observeEvent(input$xcms_selectTab,{
  if(!is.null(input$xcms_settingstab) && nrow(hot_to_r(input$xcms_settingstab)) != 0){
    internalValues$params[[internalValues$active]][,which(colnames(internalValues$params[[internalValues$active]]) != "Description")] <- hot_to_r(input$xcms_settingstab)
  }  
  internalValues$active <- input$xcms_selectTab
  
})




observeEvent(input$xcms_start,{
  if(!is.null(input$xcms_settingstab) && nrow(hot_to_r(input$xcms_settingstab)) != 0){
    internalValues$params[[internalValues$active]][,which(colnames(internalValues$params[[internalValues$active]]) != "Description")] <- hot_to_r(input$xcms_settingstab)
  }
  
  fo <- file.path(internalValues$wd, paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),"_", input$xcms_name))
  dir.create(fo)
  
  write.csv(data.frame(X=1,Time=0,Status="",Details="",elapsed_time=0), file = file.path(fo,"status.csv"))
  internalValues$jobs <- c(internalValues$jobs, fo)
  file.copy(system.file("scripts", "xcms_runner_i.R",package = "METABOseek"),fo)
  
  for(i in 1:length(internalValues$params)){
    write.csv(internalValues$params[[i]], file = file.path(fo,paste0(names(internalValues$params)[i],".csv")), row.names = T)
  }
  
  posSettings <- reactiveValuesToList(tAnalysisX)
  posSettings$rtCorrAnaCheck <- internalValues$rtCorrAnaCheck
  posSettings$noRtCorrAnaCheck <- internalValues$noRtCorrAnaCheck
  
  write(jsonlite::serializeJSON(posSettings, pretty = T), file.path(fo, "postProcessingSettings.json"))
  
  
  zip(file.path(fo,"settings.zip"), grep(list.files(fo, full.names = T), pattern = "status.csv", invert = T, value = T), flags = "-j")
  
  runner <- system.file("scripts", "xcms_runner_i.R",package = "METABOseek")
  rpath <- file.path(R.home(component = "bin"), "Rscript")
  
  
  system(paste0( '"',
                 rpath,
                 '"  --verbose ',
                 '"',
                 runner,
                 '" "',
                 fo,
                 '"'),
         wait = F)
  
  showModal(modalDialog(p("The xcms analysis is running in a separate process now.
                          You can continue using METABOseek or close METABOseek now without interrupting the analysis.
                          The results of this analysis can be found in ", strong(fo)),
                        title = "xcms analysis is running!",
                        easyClose = T
  ))
  
})

output$xcms_settingstab <- renderRHandsontable({
  MAT_comments <- matrix(ncol = length(which(colnames(internalValues$params[[internalValues$active]]) != "Description")),
                         nrow = nrow(internalValues$params[[internalValues$active]]))
  if(!is.null(internalValues$params[[internalValues$active]]) & internalValues$active != "filegroups"){
    MAT_comments[, 1] <- internalValues$params[[internalValues$active]]$Description
  }
  
  showme <- as.data.frame(internalValues$params[[internalValues$active]][,which(colnames(internalValues$params[[internalValues$active]]) != "Description")],
                          stringsAsFactors = F,
                          row.names = row.names(internalValues$params[[internalValues$active]]))
  colnames(showme) <- colnames(internalValues$params[[internalValues$active]])[which(colnames(internalValues$params[[internalValues$active]]) != "Description")]
  
  rhandsontable(showme,
                readOnly = F,
                contextMenu = T,
                selectCallback = TRUE,
                comments = MAT_comments,
                digits = 8,
                highlightCol = TRUE,
                highlightRow = TRUE,
                rowHeaderWidth = 200) %>%
    hot_cell(1,"MOSAIC_intensities", readOnly = T)%>%
    hot_cell(1,"xcms_peakfilling", readOnly = T)%>%
    hot_cell(1,"CAMERA_analysis", readOnly = T)
  
})



observeEvent(input$xcms_statustab,{
  if(!is.null(input$xcms_statustab) && !is.na(hot_to_r(input$xcms_statustab)$Status[1]) && hot_to_r(input$xcms_statustab)$Status[1] == "Finished"){
    showNotification(paste("XCMS analysis finished"), duration = 0)
  }
  
  if(!is.null(input$xcms_statustab) && !is.na(hot_to_r(input$xcms_statustab)$Status[1]) && hot_to_r(input$xcms_statustab)$Status[1] == "Starting analysis"){
    showNotification(paste("XCMS analysis started"), duration = 0)
  }
})



output$xcms_statustab <- renderRHandsontable({if(!is.null(internalValues$jobs)){
  
  rhandsontable(reactiveFileReader(1500,
                                   NULL,
                                   file.path(internalValues$jobs[1],"status.csv"),
                                   read.csv,
                                   stringsAsFactors = F, 
                                   row.names = 1)(),
                readOnly = T,
                contextMenu = F,
                selectCallback = TRUE,
                digits=8,
                highlightCol = TRUE,
                highlightRow = TRUE,
                rowHeaderWidth = 200)
  
}
})

# Generate a text output ----
output$summary <- renderPrint({
  print(gsub("\\\\","/", input$xcms_folder))
})

output$noRtCorrCheck <- renderUI({
  div(title= "Activate post-processing for non-retention time corrected data.",
      checkboxInput(ns('nortcorrcheck'), 'Before retention time correction', value = internalValues$noRtCorrAnaCheck))
})

observeEvent(input$nortcorrcheck,{
  internalValues$noRtCorrAnaCheck <- input$nortcorrcheck
})

output$rtCorrCheck <- renderUI({
  div(title= "Activate post-processing for retention time corrected data.",
      checkboxInput(ns('rtcorrcheck'), 'After retention time correction', value = internalValues$rtCorrAnaCheck))
})

observeEvent(input$rtcorrcheck,{
  internalValues$rtCorrAnaCheck <- input$rtcorrcheck
})

tAnalysisX <- callModule(TableAnalysisModule, "TabAnalysisXcms",
                        reactives = reactive({list(fileGrouping = if(internalValues$active == "filegroups" 
                                                                     && !is.null(input$xcms_settingstab) 
                                                                     && length(hot_to_r(input$xcms_settingstab)$File) > 0 ){
                          hot_to_r(input$xcms_settingstab)}
                          else{internalValues$params$filegroups})
                          }),
                        values = reactiveValues(featureTables = NULL,
                                                MSData= NULL))
observeEvent(tAnalysisX,{
  tAnalysisX$analysesSelected <- NULL#tAnalysisX$analysesAvailable
}, once = T)

return(internalValues)

}


#' @describeIn xcmsModule UI elements
#' @export 
xcmsModuleUI <-  function(id){
  ns <- NS(id)
  useShinyjs()
fluidPage(
  fluidRow(
    box(title = "Run XCMS analysis", width = 12, status= "primary",
        
        p("This module runs and observes an XCMS analysis with customizable settings and generates a new folder inside the mzXML file folder with results from the xcms analysis."),
        
        p(strong("Not on by default in Server mode!")," Currently only one xcms job per METABOseek session (concurrent job monitoring coming later)."),
        fluidRow(
          column(6,
                 actionButton(ns('xcms_loadfolderOffline'), "load MS file folder"),
                 shinyDirButton(ns('xcms_loadfolder'), "load MS file folder", title = "select a folder with MS data files"),
                 
                 
                 textInput(ns('xcms_name'), "Title of this analysis", "xcms_run"),
                 actionButton(ns('xcms_start'),"Start analysis!", style="color: #fff; background-color: #C41230; border-color: #595959")),
          
          column(6,
                 fileInput(ns('xcms_settingsLoad'),"Load settings", accept = "application/zip"),
                 
                 downloadButton(ns("xcms_settingsDL"), "Download settings")
          ))
        
    )),
  
  fluidRow(
    tabBox(title = "XCMS Settings",
           id = "xcms_settingsBox",
           width = 12, side = "right", selected = "XCMS Settings",
           
           tabPanel("_"),
           
           
           
           tabPanel("XCMS Settings",
                    
                  fluidPage(
                    fluidRow(
                    htmlOutput(ns('xcms_selectTab'))
                    ),
                    fluidRow(
                    rHandsontableOutput(ns('xcms_settingstab'))
                    ),
                    fluidRow(
                      h3("Automatic post-processing of MS data"),
                      p("Basic analysis and p-value calculation require more than one group set in File Grouping.")),
                    fluidRow(
                      column(3,htmlOutput(ns("noRtCorrCheck"))),
                      column(3, htmlOutput(ns("rtCorrCheck")))
                    ),
                    fluidRow(
                      TableAnalysisModuleUI(ns("TabAnalysisXcms"))
                    )
                    
           )))
    ),
  fluidRow(
    box(title = "Job status", width = 12, status= "primary",
        p("View status of a running XCMS job here"),
        rHandsontableOutput(ns('xcms_statustab'))
    ))
  

)
}