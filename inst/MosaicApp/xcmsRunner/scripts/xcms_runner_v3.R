
library(xcms)
#library(jsonlite)
#library(CAMERA)
#library(BiocParallel)

source("C:/Users/mjh43/OneDrive - Cornell University/R scripts new/Mosaic/xcms standalone/functions/class_msdata.R")
source("C:/Users/mjh43/OneDrive - Cornell University/R scripts new/Mosaic/xcms standalone/functions/xcms_retcorAdjust.R")
source("C:/Users/mjh43/OneDrive - Cornell University/R scripts new/Mosaic/xcms standalone/functions/xcms_runner_stepping.R")



#setwd(commandArgs(trailingOnly=TRUE))
setwd("C:/Workspace/mzxml/MOSAIC Experimental/ppac vs cele new/full files/xcms_runner_v3/")
print(getwd())

history <- writeStatus (previous = NULL,
                        message = list(Status = "Starting analysis",
                                       Details = "initializing parameters"))

#Load settings from csv files in wd
##########################
filegroups = read.csv("filegroups",
                   row.names = 1,
                   stringsAsFactors = F)

mzxml_pos <- filegroups$File
#########################
centWave = read.csv("centWave",
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

bparam <- SnowParam(workers = as.integer(centWave["workers",1]))
##########################
groupparam = read.csv("group",
                 row.names = 1,
                 stringsAsFactors = F)

#grouping parameters
gparam <- PeakDensityParam(sampleGroups = c(1:length(mzxml_pos)),
  minFraction = as.numeric(groupparam["minfrac",1]),
  bw = as.numeric(groupparam["bw",1]),
  binSize = as.numeric(groupparam["mzwid",1]),
  maxFeatures = as.integer(groupparam["max",1]),
  minSamples = as.integer(groupparam["minsamp",1])
)

##########################
retcorParam = read.csv("retcor",
                       row.names = 1,
                       stringsAsFactors = F)
#retcor parameters
if(as.character(retcorParam["method",1]) == "obiwarp"){
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
}

##force use of PeakGroupsParam if obiwarp does not work
pgparam <- PeakGroupsParam(minFraction = 0.9, span = 0.4)

##########################
outputs = read.csv("outputs",
                       row.names = 1,
                       stringsAsFactors = F)

#logical() which outputs are requested
tbouts <- as.logical(outputs$Value[-1])
names(tbouts) <- row.names(outputs[-1,])

#logical() which outputs should get MOSAIC intensities
getints <- as.logical(outputs$MOSAIC_intensities[-1])
names(getints) <- row.names(outputs[-1,])

#############################


history <- writeStatus (previous = history,
                        message = list(Status = "Starting analysis",
                                       Details = "loading files"))

#xcmsRaw object list for Mosaic intensity method
if(any(na.omit(as.logical(outputs$MOSAIC_intensities)))){
rfiles <- loadRaw(filelist= mzxml_pos, MSn = F, workers = as.integer(centWave["workers",1]), rnames = mzxml_pos)
}

fileaccess <- readMSData2(mzxml_pos, pdata = NULL, verbose = isMSnbaseVerbose(),
                            centroided. = T,
                            smoothed. = NA)#,
                         #mode = "onDisk")

###########
history <- writeStatus (previous = history,
                        message = list(Status = "Finding peaks",
                                       Details = "XcmsSet peak detection"))

xset <- findChromPeaks(fileaccess, cparam,
                               BPPARAM = bparam, return.type = "XCMSnExp")

if(tbouts["peaktable_all"]){
  history <- writeStatus (previous = history,
                          message = list(Status = "Exporting data",
                                         Details = "peaktable_all.csv"))
  
   write.csv(chromPeaks(xset),file = "peaktable_all_unfilled.csv")
}

if(any(tbouts[!names(tbouts) == "peaktable_all"])){

### GROUPING STEP  
  history <- writeStatus (previous = history,
                          message = list(Status = "Group peaks",
                                         Details = "group.density"))

## group corresponding peaks across samples (NOT needed for retcor with obiwarp)
xset <- groupChromPeaks(xset,
                        gparam)

##DATA EXPORT
  history  <- savetable(xset,
                        status = history,
                        fill = if(tbouts["peaktable_grouped_filled"]){
                                FillChromPeaksParam(expandMz = 0.005,
                                                    expandRt = 5,
                                                    ppm = 3)}
                               else{NULL},
                        nonfill = tbouts["peaktable_grouped"],
                        filename = "peaktable_grouped.csv",
                        bparams = bparam,
                        intensities = (getints["peaktable_grouped"] | getints["peaktable_grouped_filled"]),
                        rawdata = rfiles)  



if(tbouts["peaktable_grouped_Rtcorr"] | tbouts["peaktable_grouped_Rtcorr_filled"]){
###RETCOR STEP

history <- writeStatus (previous = history,
                        message = list(Status = "Retention time correction",
                                       Details = as.character(retcorParam["method",1])))


xset <- tryCatch(xset <- adjustRtime(xset,
                              param = rparam),
         error = function(e){list(msg = e,
                                  obj = adjustRtime(xset,
                                                    param = pgparam))})

if(is.list(xset)){
  
  history[1,"Details"] <- paste("ERROR: Obiwarp failed, used PeakGroups method instead.")  #,paste(attr(xset[[1]], "result"), collapse = " ")
  xset <- xset[[2]]
}
  

 history <- writeStatus (previous = history,
                          message = list(Status = "Regroup peaks",
                                         Details = "group.density"))
xset <- groupChromPeaks(xset,
                        gparam)

##DATA EXPORT
history  <- savetable(xset,
                      status = history,
                      fill = if(tbouts["peaktable_grouped_Rtcorr_filled"]){
                        FillChromPeaksParam(expandMz = 0.005,
                                            expandRt = 5,
                                            ppm = 3)}
                      else{NULL},
                      nonfill = tbouts["peaktable_grouped_Rtcorr"],
                      filename = "peaktable_grouped_Rtcorr.csv",
                      bparams = bparam,
                      intensities = (getints["peaktable_grouped_Rtcorr"] | getints["peaktable_grouped_Rtcorr_filled"]),
                      rawdata = rfiles)  

}

}

if(tbouts["peaktable_CAMERA"]){
  
  history <- writeStatus (previous = history,
                          message = list(Status = "CAMERA annotation",
                                         Details = "Adduct and isotope annotation with the CAMERA package (after RT correction)"))
  
  library(CAMERA)
  an   <- xsAnnotate(as(xset, "xcmsSet"),
                     nSlaves = as.integer(centWave["workers",1]),
                     polarity = "positive")###CHANGE POLARITY
  
  an <- groupFWHM(an, sigma=3, perfwhm = 0.5 ) # peakwidth at FWHM is about 2.335*sigma, sigma factor should correspond to what max rt difference can be for features to be grouped.
  #verify grouping
  an <- groupCorr(an)
  an <- findIsotopes(an)
  an <- findAdducts(an, polarity= "positive")###CHANGE POLARITY
  peaklist <- getPeaklist(an)
  
  history  <- savetable(an,
                        status = history,
                        fill = NULL,
                        nonfill = T,
                        filename = "peaktable_CAMERA",
                        bparams = bparam,
                        intensities = getints["peaktable_CAMERA"],
                        rawdata = rfiles)  
  cleanParallel(an)
  
}

#####################

history <- writeStatus (previous = history,
                        message = list(Status = "Finished",
                                       Details = "all analyses done, total time"))

