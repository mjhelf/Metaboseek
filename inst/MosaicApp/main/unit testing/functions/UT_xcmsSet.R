

mzxml_pos <- list.files("C:/Workspace/mzxml/MOSAIC Experimental/ppac vs cele new/full files/", pattern="mzXML", recursive = TRUE, include.dirs = T, full.names = T)
rwdata <- loadRaw(mzxml_pos)

bparam <- SnowParam(workers = 4)
xset.oldMeth <- xcmsSet(mzxml_pos, method="centWave",
                        ppm=4, peakwidth=c(3,20), 
                        mzdiff=-0.005, snthresh=3,prefilter=c(3,100),
                        BPPARAM = bparam)



cparam <- CentWaveParam(ppm = 4, peakwidth = c(3, 20), snthresh = 3,mzCenterFun = "wMean",firstBaselineCheck = TRUE,
              prefilter = c(3, 100), mzdiff=-0.005, noise = 0, fitgauss = TRUE, verboseColumns = TRUE )#, mzCenterFun = "wMean", integrate = 1L,
             # mzdiff = -0.001, fitgauss = FALSE, , verboseColumns = FALSE,
             # roiList = list(), , roiScales = numeric())

MSnExpThing <- readMSData(mzxml_pos, pdata = NULL, verbose = isMSnbaseVerbose(),
                           centroided. = T,
                           smoothed. = NA)

#This generates an onDisk object (low memory usage, lower performance)
MSnExpThing2 <- readMSData2(mzxml_pos, pdata = NULL, verbose = isMSnbaseVerbose(),
            centroided. = T,
            smoothed. = NA)

xset.newmeth <- findChromPeaks(MSnExpThing2, cparam,
                               BPPARAM = bparam, return.type = "xcmsSet")

xset.pos <- group(xset.newmeth, minfrac=0.2, bw=5, mzwid=0.1, max=500, minsamp=1)
save(xset.pos, file=paste0(tag2,"xsetgrouped"))

xset.pos.pl <- peakTable(xset.pos)
xset.pos.pl[is.na(xset.pos.pl)]<-0
write.csv(xset.pos.pl,file = paste0(tag2,"peaklist_nonRTcorr.csv"))


#Annotate non-retention time corrected xcmsSet
##OPTIONAL ISOTOPE AND ADDUCT ANNOTATION
if(camera){
  an   <- xsAnnotate(xset.pos, nSlaves = 8, polarity = pol)###CHANGE POLARITY
  an <- groupFWHM(an, sigma=3, perfwhm = 0.5 ) # peakwidth at FWHM is about 2.335*sigma, sigma factor should correspond to what max rt difference can be for features to be grouped.
  #verify grouping
  an <- groupCorr(an)
  save(an,file=paste0(tag2,"an_after_groupCorr"))
  an <- findIsotopes(an)
  save(an,file=paste0(tag2,"an_after_findIsotopes"))  
  an <- findAdducts(an, polarity= pol)###CHANGE POLARITY
  save(an,file=paste0(tag2,"an_after_findAdducts"))
  
  peaklist <- getPeaklist(an)
  cleanParallel(an)
  
  write.csv(peaklist, file= paste0(tag2,"peaklist_CAMERA.csv"))
}

#Retention Time Correction and regroup (without CAMERA)

xset2.pos <- retcor(xset.pos, method="obiwarp", profStep=0.1, plottype = "deviation")
save(xset2.pos, file=paste0(tag2,"xset2_pos"))

xset3.pos <- group(xset2.pos, minfrac=0, bw=5, mzwid=0.005, max=500, minsamp=1)
save(xset3.pos, file=paste0(tag2,"xset3_pos"))

xset3.pos.pl <- peakTable(xset3.pos)
xset3.pos.pl[is.na(xset3.pos.pl)]<-0
write.csv(xset3.pos.pl,file = paste0(tag2,"peaklist_RTcorr.csv"))

#Fill missing peaks
xset3.pos.filled <- fillPeaks.chrom(xset3.pos, BPPARAM=param)
xset3.pos.filled.pl <- peakTable(xset3.pos.filled)
write.csv(xset3.pos.filled.pl,file = paste0(tag2,"peaklist_filled_RTcorr.csv"))

## make own XICs for each file in rawfiles list in addition to CAMERA table results
plist_own <- if(camera){peaklist}else{xset3.pos.filled.pl}
pt2<-proc.time()
for (n in c(1:length(rawdata[[1]]))){
  plist_own <-    multiEIC(rawfile= rawdata[[1]][[n]] ,
                           featuretable=plist_own,
                           mz = plist_own$mz,
                           ppm=5,
                           rtw= data.frame(plist_own$rtmin-5,plist_own$rtmax+5),
                           mini=5000,
                           coltag=paste0(rawdata[[1]][[n]]@filepath@.Data),
                           pval=0.05,
                           XIC=T,
                           abovex = F,
                           gauss= F)
  print(n)}
pt2<-proc.time()-pt2
print(pt2)
write.csv(plist_own,file = paste0(tag2,"_plist_own_RTcorr.csv"))
