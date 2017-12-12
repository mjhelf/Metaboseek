xcmsSettings <- reactiveValues(params = list(filegroups = data.frame(File = character(1), Groups = character(1)),
                                             centWave = read.csv(system.file("MosaicApp", "xcmsRunner","defaults", "centWave.csv",package = "Mosaic"),
                                                                 row.names = 1,
                                                                 stringsAsFactors = F),
                                             group = read.csv(system.file("MosaicApp", "xcmsRunner","defaults", "group.csv",package = "Mosaic"),
                                                              row.names = 1,
                                                              stringsAsFactors = F),
                                             retcor = read.csv(system.file("MosaicApp", "xcmsRunner","defaults", "retcor.csv",package = "Mosaic"),
                                                               row.names = 1,
                                                               stringsAsFactors = F),
                                             outputs = read.csv(system.file("MosaicApp", "xcmsRunner","defaults", "outputs.csv",package = "Mosaic"),
                                                                row.names = 1,
                                                                stringsAsFactors = F),
                                             peakfilling = read.csv(system.file("MosaicApp", "xcmsRunner","defaults", "peakfilling.csv",package = "Mosaic"),
                                                                    row.names = 1,
                                                                    stringsAsFactors = F),
                                             camera = read.csv(system.file("MosaicApp", "xcmsRunner","defaults", "camera.csv",package = "Mosaic"),
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

#fromJSON(toJSON(mzxml_pos))
# write_json(mzxml_pos, paste0(wd,"files"))
# mzxml_pos2 <- read_json(paste0(wd,"files"), simplifyVector = T)


toggle(id = "xcms_loadfolderOffline", condition = (!servermode && Sys.info()['sysname'] == "Windows"))
toggle(id = "xcms_loadfolder", condition = (servermode))

observe({
  toggleState(id = "xcms_loadfolder", condition = (servermode && activateXCMS))
  toggleState(id = "xcms_start", condition = (!servermode || (servermode && activateXCMS)))
})

shinyDirChoose(input, 'xcms_loadfolder', roots=rootpath)


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
  #print(xcmsSettings$params[[xcmsSettings$active]][,which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")])
  #print(input$xcms_settingstab)
  if(!is.null(input$xcms_settingstab) && nrow(hot_to_r(input$xcms_settingstab)) != 0){
    xcmsSettings$params[[xcmsSettings$active]][,which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")] <- hot_to_r(input$xcms_settingstab)
  }  
  xcmsSettings$active <- input$xcms_selectTab
  
})




observeEvent(input$xcms_start,{
  if(!is.null(input$xcms_settingstab) && nrow(hot_to_r(input$xcms_settingstab)) != 0){
    xcmsSettings$params[[xcmsSettings$active]][,which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")] <- hot_to_r(input$xcms_settingstab)
    fo <- file.path(xcmsSettings$wd, paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),"_", input$xcms_name))
    dir.create(fo)
    
    write.csv(data.frame(X=1,Time=0,Status="",Details="",elapsed_time=0), file = file.path(fo,"status.csv"))
    xcmsSettings$jobs <- c(xcmsSettings$jobs, fo)
    file.copy(system.file("MosaicApp", "xcmsRunner","scripts", "xcms_runner_i.R",package = "Mosaic"),fo)
    
    for(i in 1:length(xcmsSettings$params)){
      write.csv(xcmsSettings$params[[i]], file = file.path(fo,paste0(names(xcmsSettings$params)[i],".csv")), row.names = T)
    }
    
    ver <- "version_1"
    file.create(file.path(fo,ver))
    zip(file.path(fo,"settings.zip"), file.path(fo, c(paste0(names(xcmsSettings$params),".csv"),ver)), flags = "-j")
    #file.remove(file.path(fo,names(xcmsSettings$params))) #note: better delete files from the runner if necessary
    
    #fo <- "C:/Users/mjh43/OneDrive - Cornell University/"
    runner <- system.file("MosaicApp", "xcmsRunner","scripts", "xcms_runner_i.R",package = "Mosaic")
                       #  file.path(getwd(),
                        #"scripts",
                        #"xcms_runner_i.R") 
    
    # runner <- file.path("C:/Users/mjh43/OneDrive - Cornell University/R scripts new/Mosaic/xcms standalone",
    #                    "scripts",
    #                   "tester.R") 
    
    system(paste0("Rscript ",
                  '"',
                  runner,
                  '" "',
                  fo,
                  #'" "',
                  #getwd(),
                  '"'),
           wait = F)
  }
  
})

output$xcms_settingstab <- renderRHandsontable({
 # if(!is.null(xcmsSettings$params[[xcmsSettings$active]])) & xcmsSettings$active != "filegroups"){
  MAT_comments <- matrix(ncol = length(which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")),
                         nrow = nrow(xcmsSettings$params[[xcmsSettings$active]]))
   if(!is.null(xcmsSettings$params[[xcmsSettings$active]]) & xcmsSettings$active != "filegroups"){
  MAT_comments[, 1] <- xcmsSettings$params[[xcmsSettings$active]]$Description
   }
  #MAT_comments[2, 2] = "Another test comment"
  
 # print(MAT_comments)
  
  #necessary to handle case when only one column != Description
  showme <- as.data.frame(xcmsSettings$params[[xcmsSettings$active]][,which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")],
                          stringsAsFactors = F,
                          row.names = row.names(xcmsSettings$params[[xcmsSettings$active]]))
  colnames(showme) <- colnames(xcmsSettings$params[[xcmsSettings$active]])[which(colnames(xcmsSettings$params[[xcmsSettings$active]]) != "Description")]
  
  rhandsontable(showme,
                readOnly = F,
                contextMenu = T,
                selectCallback = TRUE,
                #height = rheight,
                # width = 1000,
                #allowComments = (!is.null(xcmsSettings$params[[xcmsSettings$active]]) & xcmsSettings$active != "filegroups"),
                comments = MAT_comments,
                digits = 8,
                highlightCol = TRUE,
                highlightRow = TRUE,
                rowHeaderWidth = 200)# %>%
  #hot_col("Value", readOnly = FALSE)%>%
  #hot_col("Group", readOnly = FALSE)%>%
  #hot_col("MOSAIC_intensities", readOnly = FALSE)%>%
  # hot_cols(columnSorting = FALSE,format="0.000000")%>%
  #hot_cols(fixedColumnsLeft = 3)%>%
  #  hot_cols(columnSorting = TRUE)%>%
  #hot_col("em",format="0.000000")%>%
  #hot_cols(renderer = "
  #        function(instance, td, row, col, prop, value, cellProperties) {
  #       Handsontable.TextCell.renderer.apply(this, arguments);
  #      td.style.color = 'black';
  #     }")
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
                #height = rheight,
                # width = 1000,
                digits=8,
                highlightCol = TRUE,
                highlightRow = TRUE,
                rowHeaderWidth = 200) %>%
    #hot_cell(1, 1, "Test comment")
  #hot_col("Value", readOnly = FALSE)%>%
  #hot_col("Group", readOnly = FALSE)%>%
  #hot_col("MOSAIC_intensities", readOnly = FALSE)%>%
  # hot_cols(columnSorting = FALSE,format="0.000000")%>%
  #hot_cols(fixedColumnsLeft = 3)%>%
  #  hot_cols(columnSorting = TRUE)%>%
  #hot_col("em",format="0.000000")%>%
  hot_cols(renderer = "
           function(instance, td, row, col, prop, value, cellProperties) {
           Handsontable.TextCell.renderer.apply(this, arguments);
           td.style.color = 'black';
           }")
  }
})

# Generate a text output ----
output$summary <- renderPrint({
  print(gsub("\\\\","/", input$xcms_folder))
})

