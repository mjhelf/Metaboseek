
library(xcms)
library(jsonlite)
library(BiocParallel)

savetable <- function(xset,
                      fill = F,
                      CAMERA = F,
                      filename = "tableout.csv",
                      bpparam = bparam,
                      intensities = F){
  
                      if(CAMERA){
                        tb <- getPeaklist(xset)}
                      else{
                       # if(fill){
                          #xset <- xset
                        #}
                        tb <- peakTable(xset)
                        
                        #set NAs to 0 (mostly important if !fill)
                        tb[is.na(tb)]<-0
                        
                        }
                      
  write.csv(tb, file = filename)
  return(tb)
}

#setwd(commandArgs(trailingOnly=TRUE))
setwd("C:/Users/mjh43/OneDrive - Cornell University/R scripts new/Mosaic/Beta r2/external/scripts/")
print(getwd())
pt1 <- proc.time()
history <- data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                      Status = "Starting analysis",
                      Details = "initializing parameters",
                      elapsed_time = 0
)
write.csv(history,file = "status.csv")

#Load settings from csv files in wd
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

mzxml_pos <- read_json("files",  simplifyVector = T)

print(mzxml_pos)

#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Starting analysis",
                            Details = "loading files",
                            elapsed_time = 0
                            ),
                            history)
write.csv(history,file = "status.csv")


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
                            elapsed_time = 0
),
history)
write.csv(history,file = "status.csv")


xset <- findChromPeaks(fileaccess, cparam,
                               BPPARAM = bparam, return.type = "xcmsSet")


if(tbouts["peaktable_all"]){
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Exporting data",
                              Details = "peaktable_all.csv",
                              elapsed_time = 0
  ),
  history)
  write.csv(history,file = "status.csv")
  
  
  write.csv(xset@peaks,file = "peaktable_all.csv")
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
                            elapsed_time = 0
),
history)
write.csv(history,file = "status.csv")



xset <- group(xset,
              minfrac = as.numeric(groupparam["minfrac",1]),
              bw = as.numeric(groupparam["bw",1]),
              mzwid = as.numeric(groupparam["mzwid",1]),
              max = as.integer(groupparam["max",1]),
              minsamp = as.integer(groupparam["minsamp",1])
              )

##DATA EXPORT
if(tbouts["peaktable_grouped"]){
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Exporting data",
                              Details = "peaktable_grouped.csv",
                              elapsed_time = 0
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (xset,
           fill = F,
           CAMERA = F,
           filename = "peaktable_grouped.csv",
           bpparam = bparam,
           intensities = getints["peaktable_grouped"])
 
  
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
                              elapsed_time = 0
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (xset,
             fill = T,
             CAMERA = F,
             filename = "peaktable_grouped_filled.csv",
             bpparam = bparam,
             intensities = getints["peaktable_grouped_filled"])
  
}

if(tbouts["peaktable_CAMERA"]){
  
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "CAMERA annotation",
                              Details = "RT correction, adduct and isotope annotation with the CAMERA package",
                              elapsed_time = 0
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (xset,
             fill = T,
             CAMERA = F,
             filename = "peaktable_grouped_filled.csv",
             bpparam = bparam,
             intensities = getints["peaktable_grouped_filled"])
  
  
  library(CAMERA)
  an   <- xsAnnotate(xset, nSlaves = as.integer(centWave["workers",1]), polarity = "positive")###CHANGE POLARITY
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
                              elapsed_time = 0
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (an,
             fill = T,
             CAMERA = T,
             filename = "peaktable_CAMERA.csv",
             bpparam = bparam,
             intensities = getints["peaktable_CAMERA"])
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
                            elapsed_time = 0
),
history)
write.csv(history,file = "status.csv")

xset <- retcor(xset,
               method = as.character(retcorParam["method",1]),
               profStep = as.numeric(retcorParam["profStep",1]),
               response = as.numeric(retcorParam["response",1]),
               distFunc = as.character(retcorParam["distFunc",1]),
               plottype = "none")

#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Regroup peaks",
                            Details = "group.density",
                            elapsed_time = 0
),
history)
write.csv(history,file = "status.csv")


xset <- group(xset,
              minfrac = as.numeric(groupparam["minfrac",1]),
              bw = as.numeric(groupparam["bw",1]),
              mzwid = as.numeric(groupparam["mzwid",1]),
              max = as.integer(groupparam["max",1]),
              minsamp = as.integer(groupparam["minsamp",1])
)

##DATA EXPORT
if(tbouts["peaktable_grouped_Rtcorr"]){
  
  #update elapsed time
  pt1 <- proc.time()-pt1
  history[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                              Status = "Exporting data",
                              Details = "peaktable_grouped_Rtcorr.csv",
                              elapsed_time = 0
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (xset,
             fill = F,
             CAMERA = F,
             filename = "peaktable_grouped_Rtcorr.csv",
             bpparam = bparam,
             intensities = getints["peaktable_grouped_Rtcorr"])
  
  
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
                              elapsed_time = 0
  ),
  history)
  write.csv(history,file = "status.csv")
  savetable (xset,
             fill = T,
             CAMERA = F,
             filename = "peaktable_grouped_Rtcorr_filled.csv",
             bpparam = bparam,
             intensities = getints["peaktable_grouped_Rtcorr_filled"])
  
}



}
}

#####################
#update elapsed time
pt1 <- proc.time()-pt1
history[1,"elapsed_time"] <- pt1[3]
pt1 <- proc.time()

history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                            Status = "Finished",
                            Details = "all analyses done",
                            elapsed_time = 0
),
history)

write.csv(history,file = "status.csv")

