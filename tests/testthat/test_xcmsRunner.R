context("xcms integration and object histories")

test_that("xcmsRunner functions work",{
    
    wtfiles <-  list.files(system.file("extdata", "examples", "ms1", "wt", package = "Metaboseek"), full.names = T, pattern = ".mzXML")
    
fileaccess <- MSnbase::readMSData(wtfiles[1:2],
                         pdata = NULL, verbose = isMSnbaseVerbose(),
                         msLevel. = 1,
                         centroided. = T,
                         smoothed. = NA,
                         mode = "onDisk")

xset <- xcms::findChromPeaks(fileaccess, xcms::CentWaveParam(),
                       BPPARAM = BiocParallel::bpparam(), return.type = "XCMSnExp")

xset2 <- xcms::groupChromPeaks(xset,xcms::PeakDensityParam(sampleGroups = 1:2))

an <- do.call(cameraWrapper, c(list(xset = xset2, workers = 2, polarity = "positive")))


ttt<- buildMseekFT(xset2)

ttt<- buildMseekFT(an)

if(file.exists("./AA_Local")){
    setwd("./AA_Local/")
}

progress <- savetable(xset2,
                      status = NULL,
                      fill = xcms::FillChromPeaksParam(expandMz = 0.005,
                                                       expandRt = 5, ppm = 3),
                      nonfill = T,
                      filename = "tableout_xset",
                      bparams = BiocParallel::bpparam(),
                      intensities = list(ppm = 5,
                                         rtw = 5,
                                         rtrange = T),
                      rawdata = NULL,
                      saveR = T,
                      postProc = NULL)

progressAN <- savetable(an,
                      status = NULL,
                      fill = xcms::FillChromPeaksParam(expandMz = 0.005,
                                                       expandRt = 5, ppm = 3),
                      nonfill = T,
                      filename = "tableoutxxAN",
                      bparams = BiocParallel::bpparam(),
                      intensities = list(ppm = 5,
                                         rtw = 5,
                                         rtrange = T),
                      rawdata = NULL,
                      saveR = T,
                      postProc = NULL)

getwd()

processHistory(an)
})
tt <- xcms::peakTable(as(xset2, "xcmsSet"))
