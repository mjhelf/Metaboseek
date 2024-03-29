
tryCatch({


library(xcms)
library(Metaboseek)
library(CAMERA)
library(BiocParallel)


fols <- commandArgs(trailingOnly=TRUE)

setwd(fols[1])

writeLines(utils::capture.output(utils::sessionInfo()), "settings/AnalysisSessionInfo.txt")

history <- writeStatus (previous = NULL,
                        message = list(Status = paste0("Starting analysis with xcms_runner in Metaboseek v",packageVersion("Metaboseek")," and xcms version ", packageVersion("xcms")),
                                       Details = "initializing parameters"))

#Load settings from csv files in wd
##########################
filegroups = read.csv("settings/filegroups.csv",
                   row.names = 1,
                   stringsAsFactors = F)

mzxml_pos <- filegroups$File
fgroups <- as.integer(as.factor(filegroups$Group))
#########################
centWave = read.csv("settings/centWave.csv",
                    row.names = 1,
                    stringsAsFactors = F)

#peak picking parameters
cparam <- CentWaveParam(ppm = as.numeric(centWave["ppm",1]),
                        peakwidth = c(min(as.numeric(unlist(strsplit(centWave["peakwidth",1], split = " ")))), max(as.numeric(unlist(strsplit(centWave["peakwidth",1], split = " "))))),
                        snthresh = as.numeric(centWave["snthresh",1]),
                        mzCenterFun = as.character(centWave["mzCenterFun",1]),
                        firstBaselineCheck = as.logical(centWave["firstBaselineCheck",1]),
                        prefilter = c(min(as.numeric(unlist(strsplit(centWave["prefilter",1], split = " ")))), max(as.numeric(unlist(strsplit(centWave["prefilter",1], split = " "))))),
                        mzdiff = as.numeric(centWave["mzdiff",1]),
                        noise = as.numeric(centWave["noise",1]), 
                        fitgauss = as.logical(centWave["fitgauss",1]), 
                        verboseColumns = FALSE ) 

BiocParallel::register(
    BiocParallel::bpstart(
        if(Sys.info()['sysname'] == "Windows"){
            BiocParallel::SnowParam(as.integer(centWave["workers",1]))
        }else{
            BiocParallel::MulticoreParam(as.integer(centWave["workers",1]))
        }
    )
)


#bparam <- SnowParam(workers = as.integer(centWave["workers",1]))
##########################
groupparam = read.csv("settings/group.csv",
                 row.names = 1,
                 stringsAsFactors = F)

#grouping parameters
gparam <- PeakDensityParam(sampleGroups = if(as.logical(groupparam["usegroups",1])){fgroups}else{c(1:length(mzxml_pos))},
  minFraction = as.numeric(groupparam["minfrac",1]),
  bw = as.numeric(groupparam["bw",1]),
  binSize = as.numeric(groupparam["mzwid",1]),
  maxFeatures = as.integer(groupparam["max",1]),
  minSamples = as.integer(groupparam["minsamp",1])
)
#########################

peakfilling = read.csv("settings/peakfilling.csv",
                       row.names = 1,
                       stringsAsFactors = F)

fparam <- FillChromPeaksParam(expandMz = as.numeric(peakfilling["expandMz",1]),
                    expandRt = as.numeric(peakfilling["expandRt",1]),
                    ppm = as.numeric(peakfilling["ppm",1]))

mos_fparam <- list(ppm = as.numeric(peakfilling["ppm_m",1]),
                   rtw = as.numeric(peakfilling["rtw",1]),
                   rtrange = as.logical(peakfilling["rtrange",1]),
                   areaMode = if("areaMode" %in% row.names(peakfilling)){as.logical(peakfilling["areaMode",1])}else{FALSE}
                   )

##########################
retcorParam = read.csv("settings/retcor.csv",
                       row.names = 1,
                       stringsAsFactors = F)
#retcor parameters

  rparam <- ObiwarpParam(binSize = as.numeric(retcorParam["profStep",1]),
                         centerSample = 1,#integer(),
                         response = as.numeric(retcorParam["response",1]),
                         distFun = as.character(retcorParam["distFunc",1]),
                         gapInit = numeric(),
                         gapExtend = numeric(),
                         factorDiag = 2,
                         factorGap = 1,
                         localAlignment = T,
                         initPenalty = 0)

##force use of PeakGroupsParam if obiwarp does not work
pgparam <- PeakGroupsParam(minFraction = as.numeric(retcorParam["minFraction",1]),
                           extraPeaks = as.integer(retcorParam["extraPeaks",1]),
                           smooth = as.character(retcorParam["smooth",1]),
                           span = as.integer(retcorParam["span",1]),
                           family = as.character(retcorParam["family",1]))



camera = read.csv("settings/camera.csv",
                       row.names = 1,
                       stringsAsFactors = F)

cam_param <- list(polarity = as.character(camera["polarity",1]),
                  ppm = as.numeric(camera["ppm",1]),
                  mzabs = as.numeric(camera["mzabs",1]),
                  sigma = as.numeric(camera["sigma",1]),
                  perfwhm = as.numeric(camera["perfwhm",1]),
                  cor_eic_th = as.numeric(camera["cor_eic_th",1]),
                  pval = as.numeric(camera["pval",1]),
                  maxcharge = as.integer(camera["maxcharge",1]),
                  maxiso = as.integer(camera["maxiso",1]),
                  minfrac = as.numeric(camera["minfrac",1]),
                  filter = as.logical(camera["filter",1]))

##########################
outputs = read.csv("settings/outputs.csv",
                       row.names = 1,
                       stringsAsFactors = F)

#make all columns except the desxr
for (i in seq(ncol(outputs) - 1)){
  outputs[,i] <- as.logical(outputs[,i])
}

#############################

ppOptions <- NULL
try({
ppOptions <- jsonlite::unserializeJSON(readChar("settings/postProcessingSettings.json",
                                                file.info("settings/postProcessingSettings.json")$size))
})
##########

history <- writeStatus (previous = history,
                        message = list(Status = "Starting analysis",
                                       Details = "loading files"))

#xcmsRaw object list for Mseek intensity method
if(any(na.omit(as.logical(outputs$MOSAIC_intensities))) 
   || (length(ppOptions) 
       && any(grepl("shapes", ppOptions$analysesSelected2)))){ #needed for peak shapes analysis types
rfiles <- loadRawM(filelist= mzxml_pos, MSn = F, workers = as.integer(centWave["workers",1]), rnames = mzxml_pos)
}else{
 rfiles <- NULL   
}

  fileaccess <- readMSData(mzxml_pos, pdata = NULL, verbose = isMSnbaseVerbose(),
                           msLevel. = 1,
                           centroided. = T,
                           smoothed. = NA,
                           mode = "onDisk")


###########
history <- writeStatus (previous = history,
                        message = list(Status = "Finding peaks",
                                       Details = "XcmsSet peak detection"))

xset <- findChromPeaks(fileaccess, cparam,
                               BPPARAM = bpparam(), return.type = "XCMSnExp")

  history <- writeStatus (previous = history,
                          message = list(Status = "Exporting data",
                                         Details = "peaktable_all.csv"))
  
  # write.csv(chromPeaks(xset),file = "peaktable_all_unfilled.csv")
   data.table::fwrite(chromPeaks(xset),
                      "peaktable_all_unfilled.csv",
          sep = ",",
          quote = T,
          row.names = F
   )

#only run this if anything other than the peaktable_all is requested
if(any(outputs[-1,1])){

### GROUPING STEP  
  history <- writeStatus (previous = history,
                          message = list(Status = "Group peaks",
                                         Details = "group.density"))

## group corresponding peaks across samples (NOT needed for retcor with obiwarp)
xset <- groupChromPeaks(xset,
                        gparam)

if(outputs["peaktable_grouped", "Value"]){

##DATA EXPORT
    history <- writeStatus (previous = history,
                            message = list(Status = "Saving table",
                                           Details = "Saving peaktable_grouped and running post-processing"))
    
    
  peaktable_grouped  <- savetable(xset,
                        #status = history,
                        # fill = if(outputs["peaktable_grouped","xcms_peakfilling"]){
                        #           fparam}
                        #        else{NULL},
                        # nonfill = outputs["peaktable_grouped", "Value"],
                        filename = "peaktable_grouped",
                        bparams = bpparam(),
                        intensities = if((outputs["peaktable_grouped", "MOSAIC_intensities"])){mos_fparam}else{NULL},
                        rawdata = rfiles,
                        postProc = if(length(ppOptions) && ppOptions$noRtCorrAnaCheck){ppOptions}else{NULL})  
  
  if(outputs["peaktable_grouped","xcms_peakfilling"]){
      
    history <- writeStatus (previous = history,
                            message = list(Status = "Saving table",
                                           Details = "Performing xcms peak filling. Saving peaktable_grouped_filled and running post-processing"))
     
      
      xset <- xcms::fillChromPeaks(xset, fparam,
                                   BPPARAM = bpparam())
      
     
  
  peaktable_grouped  <- savetable(xset,
                                   importResultsFrom = peaktable_grouped, #will import post-processing and Mseek intensities from previous step if appropriate, otherwise will generate them de novo
                                  filename = "peaktable_grouped_filled",
                                  bparams = bpparam(),
                                  intensities = if((outputs["peaktable_grouped", "MOSAIC_intensities"])){mos_fparam}else{NULL},
                                  rawdata = rfiles,
                                  postProc = if(length(ppOptions) && ppOptions$noRtCorrAnaCheck){ppOptions}else{NULL})
  
  }

  if(outputs["peaktable_grouped", "CAMERA_analysis"]){
    
    history <- writeStatus (previous = history,
                            message = list(Status = "CAMERA annotation",
                                           Details = "Adduct and isotope annotation with the CAMERA package (before RT correction)"))
    
    an <- do.call(cameraWrapper, c(list(xset = xset, workers = as.integer(centWave["workers",1])),cam_param))
   
                          savetable(an,
                          importResultsFrom = peaktable_grouped, #will import post-processing and Mseek intensities from previous step if appropriate, otherwise will generate them de novo
                          filename = if(outputs["peaktable_grouped","xcms_peakfilling"]){"peaktable_filled_noRTcorr_CAMERA"}else{"peaktable_noRTcorr_CAMERA"},
                          bparams = bpparam(),
                          intensities = if(outputs["peaktable_grouped", "MOSAIC_intensities"]){mos_fparam}else{NULL},
                          rawdata = rfiles,
                          postProc = if(length(ppOptions) && ppOptions$noRtCorrAnaCheck){ppOptions}else{NULL}) 
    
    rm(an)
    
  }
  rm(peaktable_grouped)
}

if(outputs["peaktable_grouped_Rtcorr","Value"]){
###RETCOR STEP

history <- writeStatus (previous = history,
                        message = list(Status = "Retention time correction",
                                       Details = as.character(retcorParam["method",1])))

if(as.character(retcorParam["method",1]) == "obiwarp"){
xset <- tryCatch(adjustRtime(xset,
                              param = rparam),
         error = function(e){list(msg = e,
                                  obj = adjustRtime(xset,
                                                    param = pgparam))})
}else{
  xset <- adjustRtime(xset,
                      param = pgparam)
  }

if(is.list(xset)){
  xset <- xset[[2]]
  history$status[1,"Details"] <- paste("ERROR: Obiwarp failed, used PeakGroups method instead.")
  
}
  

history <- writeStatus (previous = history,
                        message = list(Status = "Exporting data",
                                       Details = "Retention time correction information"))

rtx <-  rtexport(xset)    
#save(rtx, file = "RTcorr_data.Rdata")
saveRDS(rtx, file = "RTcorr_data.Rds")


 history <- writeStatus (previous = history,
                          message = list(Status = "Regroup peaks",
                                         Details = "group.density"))
xset <- groupChromPeaks(xset,
                        gparam)


history <- writeStatus (previous = history,
                        message = list(Status = "Saving table",
                                       Details = "Saving peaktable_grouped_RTcorr and running post-processing"))


peaktable_grouped  <- savetable(xset,
                                filename = "peaktable_grouped_RTcorr",
                                bparams = bpparam(),
                                intensities = if((outputs["peaktable_grouped_Rtcorr", "MOSAIC_intensities"])){mos_fparam}else{NULL},
                                rawdata = rfiles,
                                postProc = if(length(ppOptions) && ppOptions$rtCorrAnaCheck){ppOptions}else{NULL})  

if(outputs["peaktable_grouped_Rtcorr","xcms_peakfilling"]){
    
    
  history <- writeStatus (previous = history,
                          message = list(Status = "Saving table",
                                         Details = "Performing xcms peak filling. Saving peaktable_grouped_RTcorr_filled and running post-processing"))
    
    xset <- xcms::fillChromPeaks(xset, fparam,
                                 BPPARAM = bpparam())
    
   
    
    peaktable_grouped  <- savetable(xset,
                                    importResultsFrom = peaktable_grouped, #will import post-processing and Mseek intensities from previous step if appropriate, otherwise will generate them de novo
                                    filename = "peaktable_grouped_RTcorr_filled",
                                    bparams = bpparam(),
                                    intensities = if((outputs["peaktable_grouped_Rtcorr", "MOSAIC_intensities"])){mos_fparam}else{NULL},
                                    rawdata = rfiles,
                                    postProc = if(length(ppOptions) && ppOptions$rtCorrAnaCheck){ppOptions}else{NULL})
    
}

if(outputs["peaktable_grouped_Rtcorr", "CAMERA_analysis"]){
    
    history <- writeStatus (previous = history,
                            message = list(Status = "CAMERA annotation",
                                           Details = "Adduct and isotope annotation with the CAMERA package (after RT correction)"))
    
    an <- do.call(cameraWrapper, c(list(xset = xset, workers = as.integer(centWave["workers",1])),cam_param))
    
    savetable(an,
                          importResultsFrom = peaktable_grouped, #will import post-processing and Mseek intensities from previous step if appropriate, otherwise will generate them de novo
                          filename = if(outputs["peaktable_grouped_Rtcorr","xcms_peakfilling"]){"peaktable_grouped_RTcorr_filled_CAMERA"}else{"peaktable_grouped_RTcorr_CAMERA"},
                          bparams = bpparam(),
                          intensities = if(outputs["peaktable_grouped_Rtcorr", "MOSAIC_intensities"]){mos_fparam}else{NULL},
                          rawdata = rfiles,
                          postProc = if(length(ppOptions) && ppOptions$rtCorrAnaCheck){ppOptions}else{NULL}) 
    
    rm(an)
}
}
}

#####################

history <- writeStatus (previous = history,
                        message = list(Status = "Finished",
                                       Details = "all analyses done, total time"))
},

error = function(e){writeStatus (previous = history,
                                         message = list(Status = "ERROR",
                                                        Details = paste("An error has occured:", e, collapse = " ")))  }
)
