xcmsSettings <- reactiveValues(params = list(filegroups = data.frame(File = character(1), Groups = character(1)),
                                             centWave = read.csv(system.file("config", "xcms_defaults", "centWave.csv",package = "Mosaic"),
                                                                 row.names = 1,
                                                                 stringsAsFactors = F),
                                             group = read.csv(system.file("config", "xcms_defaults", "group.csv",package = "Mosaic"),
                                                              row.names = 1,
                                                              stringsAsFactors = F),
                                             retcor = read.csv(system.file("config", "xcms_defaults", "retcor.csv",package = "Mosaic"),
                                                               row.names = 1,
                                                               stringsAsFactors = F),
                                             outputs = read.csv(system.file("config", "xcms_defaults", "outputs.csv",package = "Mosaic"),
                                                                row.names = 1,
                                                                stringsAsFactors = F),
                                             peakfilling = read.csv(system.file("config", "xcms_defaults", "peakfilling.csv",package = "Mosaic"),
                                                                    row.names = 1,
                                                                    stringsAsFactors = F),
                                             camera = read.csv(system.file("config", "xcms_defaults", "camera.csv",package = "Mosaic"),
                                                               row.names = 1,
                                                               stringsAsFactors = F)
),
wd = character(),
active = "centWave",
jobs = NULL,
viewjob = NULL)


observeEvent(input$xcms_settingsLoad$datapath,{
  
  exfolder = file.path(dirname(input$xcms_settingsLoad$datapath), gsub("\\.[^.]*$","",input$xcms_settingsLoad$name))
  
  unzip(input$xcms_settingsLoad$datapath, exdir = exfolder )
  
  newfiles <- list.files(exfolder, pattern=".csv", recursive = TRUE, full.names=T)
  
  for( i in newfiles){
    xcmsSettings$params[[gsub("\\.[^.]*$","",basename(i))]] <- read.csv(i,
                                                                        row.names = 1,
                                                                        stringsAsFactors = F)
  }
  xcmsSettings$wd <- get_common_dir(xcmsSettings$params$filegroups$File)
  
  #if an old outputs.csv file is loaded, replace it with the new default.
  if(ncol(xcmsSettings$params$outputs) < 5) {
    xcmsSettings$params$outputs <- read.csv(system.file("config", "xcms_defaults", "outputs.csv",package = "Mosaic"),
                                            row.names = 1,
                                            stringsAsFactors = F)
  }
})


output$xcms_settingsDL <- downloadHandler(filename= function(){paste("settings.zip")}, 
                                          content = function(file){
                                            
                                            
                                            flist = paste0(names(xcmsSettings$params),".csv")
                                            for(i in 1:length(xcmsSettings$params)){
                                              write.csv(xcmsSettings$params[[i]], file = flist[i], row.names = T)
                                            }
                                            
                                            zip(file, flist, flags = "-j")
                                            if(file.exists(paste0(file, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
                                          },
                                          contentType = "application/zip")


toggle(id = "xcms_loadfolderOffline", condition = (!servermode && Sys.info()['sysname'] == "Windows"))
toggle(id = "xcms_loadfolder", condition = ((servermode) || (!servermode && Sys.info()['sysname'] != "Windows")))

observe({
  toggleState(id = "xcms_loadfolder", condition = ((servermode && activateXCMS) || (!servermode && Sys.info()['sysname'] != "Windows")))
  toggleState(id = "xcms_start", condition = (length(xcmsSettings$wd)>0 && (!servermode || (servermode && activateXCMS))))
})

shinyDirChoose(input, 'xcms_loadfolder', session = session, roots=rootpath)


observeEvent(input$xcms_loadfolder,{
  fol <-  parseDirPath(roots=rootpath, input$xcms_loadfolder)
  if(length(fol)>0){
    #taken from xcms package
    filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]",
                     "[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
    filepattern <- paste(paste("\\.", filepattern, "$", sep = ""), collapse = "|")
    flist = list.files(fol, pattern=filepattern, recursive = TRUE, full.names=T)
    xcmsSettings$params$filegroups <- data.frame(File = flist, Group = rep("G1", length(flist)), stringsAsFactors = F)
    xcmsSettings$wd <- fol
    xcmsSettings$active <- "filegroups"
  }
})

observeEvent(input$xcms_loadfolderOffline,{
  fol <- gsub("\\\\","/",choose.dir())
  if(length(fol)>0){
    #taken from xcms package
    filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]",
                     "[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
    filepattern <- paste(paste("\\.", filepattern, "$", sep = ""), collapse = "|")
    flist = list.files(fol, pattern=filepattern, recursive = TRUE, full.names=T)
    xcmsSettings$params$filegroups <- data.frame(File = flist, Group = rep("G1", length(flist)), stringsAsFactors = F)
    xcmsSettings$wd <- fol
    xcmsSettings$active <- "filegroups"
  }
  
})

output$xcms_selectTab <- renderUI({selectizeInput('xcms_selectTab',"Change settings for...", 
                                                  choices = list("File Grouping" = "filegroups",
                                                                 "Peak Detection" = "centWave",
                                                                 "Peak filling" = "peakfilling",
                                                                 "Feature grouping" = "group",
                                                                 "CAMERA settings" = "camera",
                                                                 "RT correction" = "retcor",
                                                                 "Output Files" = "outputs"),
                                                  selected = xcmsSettings$active
)})

observeEvent(input$xcms_selectTab,{
  if(!is.null(input$xcms_settingstab) && nrow(hot_to_r(input$xcms_settingstab)) != 0){
    xcmsSettings$params[[xcmsSettings$active]][,which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")] <- hot_to_r(input$xcms_settingstab)
  }  
  xcmsSettings$active <- input$xcms_selectTab
  
})




observeEvent(input$xcms_start,{
  if(!is.null(input$xcms_settingstab) && nrow(hot_to_r(input$xcms_settingstab)) != 0){
    xcmsSettings$params[[xcmsSettings$active]][,which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")] <- hot_to_r(input$xcms_settingstab)
  }
  
  fo <- file.path(xcmsSettings$wd, paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),"_", input$xcms_name))
  dir.create(fo)
  
  write.csv(data.frame(X=1,Time=0,Status="",Details="",elapsed_time=0), file = file.path(fo,"status.csv"))
  xcmsSettings$jobs <- c(xcmsSettings$jobs, fo)
  file.copy(system.file("scripts", "xcms_runner_i.R",package = "Mosaic"),fo)
  
  for(i in 1:length(xcmsSettings$params)){
    write.csv(xcmsSettings$params[[i]], file = file.path(fo,paste0(names(xcmsSettings$params)[i],".csv")), row.names = T)
  }
  
  
  zip(file.path(fo,"settings.zip"), file.path(fo, c(paste0(names(xcmsSettings$params),".csv"))), flags = "-j")

  runner <- system.file("scripts", "xcms_runner_i.R",package = "Mosaic")
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
                          You can continue using MOSAiC or close MOSAiC now without interrupting the analysis.
                          The results of this analysis can be found in ", strong(fo)),
                        title = "xcms analysis is running!",
                        easyClose = T
  ))
  
})

output$xcms_settingstab <- renderRHandsontable({
  MAT_comments <- matrix(ncol = length(which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")),
                         nrow = nrow(xcmsSettings$params[[xcmsSettings$active]]))
  if(!is.null(xcmsSettings$params[[xcmsSettings$active]]) & xcmsSettings$active != "filegroups"){
    MAT_comments[, 1] <- xcmsSettings$params[[xcmsSettings$active]]$Description
  }
 
  showme <- as.data.frame(xcmsSettings$params[[xcmsSettings$active]][,which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")],
                          stringsAsFactors = F,
                          row.names = row.names(xcmsSettings$params[[xcmsSettings$active]]))
  colnames(showme) <- colnames(xcmsSettings$params[[xcmsSettings$active]])[which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")]
  
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



output$xcms_statustab <- renderRHandsontable({if(!is.null(xcmsSettings$jobs)){
  
  rhandsontable(reactiveFileReader(1500,
                                   NULL,
                                   file.path(xcmsSettings$jobs[1],"status.csv"),
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

