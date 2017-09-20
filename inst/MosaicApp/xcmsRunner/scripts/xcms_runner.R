
library(xcms)
#library(jsonlite)
#library(CAMERA)
library(BiocParallel)

savetable <- function(xset,
                      fill = F,
                      CAMERA = F,
                      filename = "tableoutxx.csv",
                      bparams = bparam,
                      intensities = F,
                      rawdata = NULL){
  
                      if(CAMERA){
                        tb <- getPeaklist(xset)
                        if(intensities & !is.null(rawdata)){
                          #extract intensities "manually" -- warning, does not account for rtcorr yet!
                          rta <- rtadjust(xset@xcmsSet, # XCMSnExp object
                                          tb[,c("rt","rtmin","rtmax")])
                          
                          for(i in 1:length(rawdata)){
                            rtwin <- data.frame(rta[[i]]$rtmin-5,rta[[i]]$rtmax+5)
                          exIntensities(rawfile= rawdata[[i]] ,
                                        featuretable=tb,
                                        mz = tb$mz,
                                        ppm=5,
                                        rtw= rtwin,
                                        coltag=basename(names(rawdata)[i]))
                          }
                        }
                        }
                      else{ #handle non-CAMER data
                        if(fill){
                        
                          fparam = FillChromPeaksParam(expandMz = 0.005, expandRt = 5, ppm = 3)
                          xset <- fillChromPeaks(xset, fparam,
                                                  BPPARAM = bparams)
                        }
                        tb <- peakTable(as(xset,"xcmsSet"))
                        
                        #set NAs to 0 (mostly important if !fill)
                        tb[is.na(tb)]<-0
                        #extract intensities "manually" handles rtcorr!
                        if(intensities & !is.null(rawdata)){
                          if(hasAdjustedRtime(xset)){
                            rta <- rtadjust(xset, # XCMSnExp object
                                     tb[,c("rt","rtmin","rtmax")])
                            
                          }
                          for(i in 1:length(rawdata)){
                            if(hasAdjustedRtime(xset)){
                            rtwin <- data.frame(rta[[i]]$rtmin-5,rta[[i]]$rtmax+5)
                                                
                            }else{
                              rtwin <- data.frame(tb$rtmin-5,tb$rtmax+5)}
                            
                            tb <- exIntensities(rawfile= rawdata[[i]] ,
                                          featuretable=tb,
                                          mz = tb$mz,
                                          ppm=5,
                                          rtw= rtwin,
                                          coltag=basename(names(rawdata)[i]))
                          }
                        }
                        }
                      
  write.csv(tb, file = filename)
  return(NULL)
}

######multiEIC###################
#' multiEIC
#' 
#' Get intensities from a table of mz/rt features from an EICraw- created object. Requires rtmin and rtmax  
#'
#' New version with apply instead of loops 5x faster for EICs
#'
#'

exIntensities <- function (rawfile= rawdata[[1]] ,
                      featuretable=alli,
                      mz = alli$mz,
                      ppm=5,
                      rtw= data.frame(alli$rtmin-5,alli$rtmax+5),
                      coltag="test"){
  
  #cat(paste0("Reference list with ",length(featuretable[,1])," features, iterating through list, feature #" ))
  
  
  mx <- matrix(data= c(mz-ppm*(mz/1000000),
                       mz+ppm*(mz/1000000),
                       rowMin(as.matrix(rtw)),
                       rowMax(as.matrix(rtw))), nrow= length(mz), ncol=4)
  
  mxl <-unname(as.list(data.frame(t(mx[,1:2]))))
  rxl <-unname(as.list(data.frame(t(mx[,3:4]))))
  
  
  summe <- mapply(rawEICm, mzrange = mxl,
                  rtrange = rxl, MoreArgs=list(object=rawfile), SIMPLIFY = F)
  
  #substract "baseline" and get rid of scan#
  fx <- function(x) x$intensity-min(x$intensity)
  summe <- lapply(summe, fx )
  
  featuretable[[paste0(sub("^([^.]*).*", "\\1",basename(coltag)),"__XIC")]] <- sapply(summe, mean)
   
   return(featuretable)}

rawEICm <- function(object,
                    mzrange = numeric(),
                    rtrange = numeric(),
                    scanrange = numeric())  {
  
  if (length(rtrange) >= 2 & max(object@scantime) >= rtrange[1]) { #the fix is after &, and will only work if x axis in plots is defined independent of EIC ranges!
    rtrange <- range(rtrange)
    
    scanidx <- (object@scantime >= rtrange[1]) & (object@scantime <= rtrange[2])
    scanrange <- c(match(TRUE, scanidx), length(scanidx) - match(TRUE, rev(scanidx)))
  }  else if (length(scanrange) < 2)
    scanrange <- c(1, length(object@scantime))
  else
    scanrange <- range(scanrange)
  
  scanrange[1] <- max(1,scanrange[1])
  scanrange[2] <- min(length(object@scantime),scanrange[2])
  
  if (!is.double(object@env$mz))  object@env$mz <- as.double(object@env$mz)
  if (!is.double(object@env$intensity)) object@env$intensity <- as.double(object@env$intensity)
  if (!is.integer(object@scanindex)) object@scanindex <- as.integer(object@scanindex)
  
  .Call("getEIC",object@env$mz,object@env$intensity,object@scanindex,as.double(mzrange),as.integer(scanrange),as.integer(length(object@scantime)), PACKAGE ='xcms' )
}

loadRaw <- function (filelist= mzxml_pos, MSn = T, workers=10, rnames = filelist){
  
  if (length(filelist)<=10){workers<-1}
  param <- SnowParam(workers = workers)
  rawcoll <- bplapply(filelist,xcmsRaw,  profstep=0, includeMSn = MSn, BPPARAM= param)
  names(rawcoll)<- rnames
  
  return(rawcoll)}


rawGrouping <- function(rawgrouptable){
  ## Make list object of grouped column names                                        
  colme <- list()
  for (l in unique(rawgrouptable$Group)){
    colme[[l]] <- as.character(rawgrouptable$File[which(rawgrouptable$Group==l)])
  }
  
  return(colme)
}

## recalculate original rt values from rt values as reported in featuretables after rt correction
rtadjust <- function(xset, # XCMSnExp object
                     rts){ #df, each column is expected to contain rt values
  if(class(xset)=="XCMSnExp"){
  noncorr <- rtime(xset, bySample=T, adjusted = F)
  corr <- rtime(xset, bySample=T, adjusted = T)
  }
  if(class(xset)=="xcmsSet"){
    noncorr <- xset@rt$raw
    corr <- xset@rt$corrected
  }
 
  fx <- function(num, rtcorr, rtdiffs){
    sel <- which.min(abs(num - rtcorr))
    return(num + rtdiffs[sel])
  }
  
  
  rtdiff <- list()
  res <- list()
  for(i in c(1:length(noncorr))){
    rtdiff[[i]] <- noncorr[[i]]-corr[[i]]
  
    res[[i]] <- rts
  for(n in c(1:ncol(rts))){
    res[[i]][,n] <- sapply(rts[,n],
                           fx,
                           rtcorr = corr[[i]],
                           rtdiffs = rtdiff[[i]])
    
  }
  }
  
  if(class(xset)=="XCMSnExp"){
    names(res) <- basename(xset@processingData@files)
  }
  if(class(xset)=="xcmsSet"){
    names(res) <- basename(xset@filepaths)
    
  }
  

  return(res)
}


setwd(commandArgs(trailingOnly=TRUE))
#setwd("C:/Workspace/mzxml/MOSAIC Experimental/ppac vs cele new/full files/xcms_run5/")
print(getwd())
pt1 <- proc.time()
history <- data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                      Status = "Starting analysis",
                      Details = "initializing parameters",
                      elapsed_time = 0, stringsAsFactors = F
)
write.csv(history,file = "status.csv")

#Load settings from csv files in wd
filegroups = read.csv("filegroups",
                   row.names = 1,
                   stringsAsFactors = F)

centWave = read.csv("centWave",
                    row.names = 1,
                    stringsAsFactors = F)

groupparam = read.csv("group",
                 row.names = 1,
                 stringsAsFactors = F)

retcorParam = read.csv("retcor",
                       row.names = 1,
                       stringsAsFactors = F)

outputs = read.csv("outputs",
                       row.names = 1,
                       stringsAsFactors = F)

tbouts <- as.logical(outputs$Value[-1])
names(tbouts) <- row.names(outputs[-1,])

getints <- as.logical(outputs$MOSAIC_intensities[-1])
names(getints) <- row.names(outputs[-1,])

mzxml_pos <- filegroups$File

print(mzxml_pos)

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

print(cparam)

bparam <- SnowParam(workers = as.integer(centWave["workers",1]))

print(bparam)

#grouping parameters
gparam <- PeakDensityParam(sampleGroups = c(1:length(mzxml_pos)),
  minFraction = as.numeric(groupparam["minfrac",1]),
  bw = as.numeric(groupparam["bw",1]),
  binSize = as.numeric(groupparam["mzwid",1]),
  maxFeatures = as.integer(groupparam["max",1]),
  minSamples = as.integer(groupparam["minsamp",1])
)

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



#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Starting analysis",
                            Details = "loading files",
                            elapsed_time = 0, stringsAsFactors = F
                            ),
                            history)
write.csv(history,file = "status.csv")

if(any(na.omit(as.logical(outputs$MOSAIC_intensities)))){
rfiles <- loadRaw(filelist= mzxml_pos, MSn = F, workers = as.integer(centWave["workers",1]), rnames = mzxml_pos)
}

fileaccess <- readMSData2(mzxml_pos, pdata = NULL, verbose = isMSnbaseVerbose(),
                            centroided. = T,
                            smoothed. = NA)

#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Finding peaks",
                            Details = "XcmsSet peak detection",
                            elapsed_time = 0, stringsAsFactors = F
),
history)
write.csv(history,file = "status.csv")


xset <- findChromPeaks(fileaccess, cparam,
                               BPPARAM = bparam, return.type = "XCMSnExp")

if(tbouts["peaktable_all"]){
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Exporting data",
                              Details = "peaktable_all.csv",
                              elapsed_time = 0, stringsAsFactors = F
  ),
  history)
  write.csv(history,file = "status.csv")
  
  
  write.csv(chromPeaks(xset),file = "peaktable_all_unfilled.csv")
}

if(any(tbouts[!names(tbouts) == "peaktable_all"])){

### GROUPING STEP  
#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Group peaks",
                            Details = "group.density",
                            elapsed_time = 0, stringsAsFactors = F
),
history)
write.csv(history,file = "status.csv")


## group corresponding peaks across samples (NOT needed for retcor with obiwarp)
xset <- groupChromPeaks(xset,
                        gparam)

##DATA EXPORT
if(tbouts["peaktable_grouped"]){
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Exporting data",
                              Details = "peaktable_grouped.csv",
                              elapsed_time = 0, stringsAsFactors = F
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (xset,
           fill = F,
           CAMERA = F,
           filename = "peaktable_grouped.csv",
           bparams = bparam,
           intensities = getints["peaktable_grouped"],
           rawdata = rfiles)
 
  
}

##DATA EXPORT
if(tbouts["peaktable_grouped_filled"]){
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Exporting data",
                              Details = "peaktable_grouped_filled.csv",
                              elapsed_time = 0, stringsAsFactors = F
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (xset,
             fill = T,
             CAMERA = F,
             filename = "peaktable_grouped_filled.csv",
             bparams = bparam,
             intensities = getints["peaktable_grouped_filled"],
             rawdata = rfiles)
  
}

if(tbouts["peaktable_CAMERA"]){
  
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "CAMERA annotation",
                              Details = "RT correction, adduct and isotope annotation with the CAMERA package",
                              elapsed_time = 0, stringsAsFactors = F
  ),
  history)
  write.csv(history,file = "status.csv")
 
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
  
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Exporting data",
                              Details = "peaktable_CAMERA.csv",
                              elapsed_time = 0, stringsAsFactors = F
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (an,
             fill = T,
             CAMERA = T,
             filename = "peaktable_CAMERA.csv",
             bparams = bparam,
             intensities = getints["peaktable_CAMERA"],
             rawdata = rfiles)
  cleanParallel(an)
  
}



if(tbouts["peaktable_grouped_Rtcorr"] | tbouts["peaktable_grouped_Rtcorr_filled"]){
###RETCOR STEP
#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Retention time correction",
                            Details = "obiwarp",
                            elapsed_time = 0, stringsAsFactors = F
),
history)
write.csv(history,file = "status.csv")

##force use of PeakGroupsParam if obiwarp does not work
pgparam <- PeakGroupsParam(minFraction = 0.9, span = 0.4)
xset <- tryCatch(adjustRtime(xset,
                              param = rparam),
         error = function(e){list(msg = e,
                                  obj = adjustRtime(xset,
                                                    param = pgparam))})

if(is.list(xset)){
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  history[1,"Details"] <- paste("ERROR: Obiwarp failed, used PeakGroups method instead.")  #,paste(attr(xset[[1]], "result"), collapse = " ")
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Regroup peaks",
                              Details = "group.density",
                              elapsed_time = 0, stringsAsFactors = F
  ),
  history)
  write.csv(history,file = "status.csv")
  
  xset <- xset[[2]]
  
}else{


#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Regroup peaks",
                            Details = "group.density",
                            elapsed_time = 0, stringsAsFactors = F
),
history)
write.csv(history,file = "status.csv")
}

xset <- groupChromPeaks(xset,
                        gparam)


##DATA EXPORT
if(tbouts["peaktable_grouped_Rtcorr"]){
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Exporting data",
                              Details = "peaktable_grouped_Rtcorr.csv",
                              elapsed_time = 0, stringsAsFactors = F
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (xset,
             fill = F,
             CAMERA = F,
             filename = "peaktable_grouped_Rtcorr.csv",
             bparams = bparam,
             intensities = getints["peaktable_grouped_Rtcorr"],
             rawdata = rfiles)
  
  
}

##DATA EXPORT
if(tbouts["peaktable_grouped_Rtcorr_filled"]){
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Exporting data",
                              Details = "peaktable_grouped_Rtcorr_filled.csv",
                              elapsed_time = 0, stringsAsFactors = F
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (xset,
             fill = T,
             CAMERA = F,
             filename = "peaktable_grouped_Rtcorr_filled.csv",
             bparams = bparam,
             intensities = getints["peaktable_grouped_Rtcorr_filled"],
             rawdata = rfiles)
  
}

}

}


#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Exporting XCMSnExp object",
                            Details = "xset and xset_filled",
                            elapsed_time = 0, stringsAsFactors = F
),
history)
write.csv(history,file = "status.csv")



save(xset,file = "xset")
fparam = FillChromPeaksParam(expandMz = 0.005, expandRt = 5, ppm = 3)
xset <- fillChromPeaks(xset, fparam,
                       BPPARAM = bparam)
save(xset,file = "xset_filled")

#####################
#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Finished",
                            Details = "all analyses done",
                            elapsed_time = 0, stringsAsFactors = F
),
history)

write.csv(history,file = "status.csv")

