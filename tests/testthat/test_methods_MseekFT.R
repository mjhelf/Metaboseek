context("methods MseekFT")


MseekExamplePreload() #loads objects tab1 and tab2 into session!

wtfiles <-  list.files(system.file("extdata", "examples", "ms1", "wt", package = "Metaboseek"), full.names = T, pattern = ".mzXML")

fileaccess <- MSnbase::readMSData(wtfiles[1:2],
                                  pdata = NULL, verbose = isMSnbaseVerbose(),
                                  msLevel. = 1,
                                  centroided. = T,
                                  smoothed. = NA,
                                  mode = "onDisk")

xset_detection <- xcms::findChromPeaks(fileaccess, xcms::CentWaveParam(),
                             BPPARAM = BiocParallel::bpparam(), return.type = "XCMSnExp")

xset_grouped <- xcms::groupChromPeaks(xset_detection,xcms::PeakDensityParam(sampleGroups = 1:2))

CAMERA_res <- do.call(cameraWrapper, c(list(xset = xset_grouped, workers = 2, polarity = "positive")))

xset_rtcorr <- xcms::adjustRtime(xset_detection,param = ObiwarpParam())

xset_rtcorr_grouped <- xcms::groupChromPeaks(xset_rtcorr,xcms::PeakDensityParam(sampleGroups = 1:2))

tabX1 <- Metaboseek:::buildMseekFT(xset_rtcorr_grouped)

tabX2<- Metaboseek:::buildMseekFT(CAMERA_res)
test_that("utility functions", {

    expect_equal(rtexport(CAMERA_res),
                 rtexport(xset_grouped))

    
})
    
    
test_that("removeNAs works", {
    tabX1wNAs <- tabX1
    
    tabX1$df[is.na(tabX1$df)] <- 0
    
#    tabX1wNAs$df[tabX1wNAs$df == 0] <- NA
    
    expect_true(any(is.na(tabX1wNAs$df)))
    
    afterRemoval <- removeNAs(tabX1wNAs)
    
    expect_equal(tabX1$df,
                 afterRemoval$df)
    
    expect_equal(length(processHistory(tabX1))+1,
                 length(processHistory(afterRemoval)))
    
  }
)

test_that("we can get Mseek intensities",{
    
    expect_true(hasError(previousStep(getMseekIntensities(tabX1, MSD$data[1:2]))))
    expect_true(all(paste0(basename(names(MSD$data[1:2])), "__XIC") %in% colnames(getMseekIntensities(tabX1, MSD$data[1:2], adjustedRT = FALSE)$df)))
})

with_ints <- getMseekIntensities(tabX1, MSD$data, adjustedRT = FALSE)



test_that("transferring Mseek intensities works",{
    
    #using getMseekIntensities with signature that includes a template for Mseek intensities
    expect_silent({
    transferred <- getMseekIntensities(tabX1, MSD$data, with_ints, adjustedRT = FALSE)
    })
    expect_true(!hasError(previousStep(transferred)))

    expect_equal(transferred$df[,transferred$MseekIntensities],
                with_ints$df[,with_ints$MseekIntensities]
                )
    
    
    expect_equal(searchFunParam(transferred, "Metaboseek::getMseekIntensities", index = FALSE)[[1]]@param,
                 searchFunParam(with_ints, "Metaboseek::getMseekIntensities", index = FALSE)[[1]]@param
    )
    
    #intensities were transferred
    expect_equal(previousStep(transferred)@info,
                 "Added Mseek intensities, safely transferred from Custom Table")

    expect_message({
        transferred2 <- getMseekIntensities(tabX1, MSD$data, tabX2, adjustedRT = FALSE)
    })
    
    expect_true(!hasError(previousStep(transferred2)))
    
    #intensities were freshly calculated
    expect_equal(previousStep(transferred2)@info,
                 "Added Mseek intensities.")
    
    
    })

test_that("Mseek analyzeFT submethods work",{
    suppressWarnings({
    expect_true(hasError(previousStep(FTNormalize(with_ints))))
    })
    expect_null(intensityCols(with_ints))
    
    intensityCols(with_ints) <- paste0(basename(names(MSD$data)), "__XIC")
    
    expect_equal(intensityCols(with_ints),
                 paste0(basename(names(MSD$data)), "__XIC"))
    
    with_ints <- FTNormalize(with_ints)
    
    expect_true(!hasError(previousStep(FTNormalize(with_ints))))
    
    allmeans <- sapply(paste0(intensityCols(with_ints), "__norm"), function(n){
        mean(with_ints$df[[n]])
    })
    
    expect_equal(min(allmeans), max(allmeans))
    
    gt <- data.frame(Column = intensityCols(with_ints),
                     Group = c(rep("G1",3),rep("G2",4),"G1","G2"),
                     stringsAsFactors = FALSE)
    groupingTable(with_ints) <- gt
    expect_equal(groupingTable(with_ints),
                 gt)
    
    expect_false(hasError(previousStep(FTBasicAnalysis(with_ints))))
    
    
    
    tab1 <- FTOldPeakShapes(tab1, MSD$data[1:2], ppm = 5, workers = 1)
    
    expect_false(hasError(previousStep(tab1)))
    
    tab1 <- FTPeakShapes(tab1, MSD$data[1:2], ppm = 5, workers = 1)
    
    expect_false(hasError(previousStep(tab1)))
    
    tab1ana <- FTMzMatch(tab1, db = system.file("db", "smid-db_pos.csv",
                                                       package = "Metaboseek"),
                           ppm = 5, mzdiff = 0.001)
    
    expect_false(hasError(previousStep(tab1ana)))
    
    expect_equal(sum(grepl("osas#1",tab1ana$df$mzMatches)),
                 1)
    
    with_ints <- FTT.test(with_ints)
    
    #because this object has  groups defined
    expect_false(hasError(previousStep(with_ints)))
    
    tab1ana <- FTT.test(tab1ana)
    
    #because this object has  groups defined
    expect_false(hasError(previousStep(tab1ana)))
    
    with_ints <- FTAnova(with_ints)
    
    #because this object has  groups defined
    expect_false(hasError(previousStep(with_ints)))
    
    tab1ana <- FTAnova(tab1ana)
    
    #because this object has groups defined
    expect_false(hasError(previousStep(tab1ana)))
    
    tab1ana <- FTCluster(tab1ana)
    
    #because this object has  groups defined
    expect_false(hasError(previousStep(tab1ana)))
    
    tab1ana <- FTPCA(tab1ana, featureMode = TRUE)
    
    expect_false(hasError(previousStep(tab1ana)))
    
    tab1ana <- FTPCA(tab1ana, featureMode = FALSE)
    
    expect_false(hasError(previousStep(tab1ana)))
    expect_true(!is.null(tab1ana$PCA_samples))
    
    fil <- list(Filter1 = list(active = FALSE,
                                    colSelected = "mz",
                                    numeric = TRUE,
                                    minSelInit = 300,
                                    maxSelInit = 305),
                Filter2 = list(active = TRUE,
                                                        colSelected = "mzMatches",
                                                        numeric = FALSE,
                                                        txtSelInit = "",
                                                        modeSelInit = "is not"))
    
    tab1fil <- FTFilter(tab1ana, filters = fil, sortBy = character())
    expect_equal(nrow(tab1fil$df),2)
    
    fil2 <- list(Filter1 = list(active = TRUE,
                               colSelected = "mz",
                               numeric = TRUE,
                               minSelInit = 300,
                               maxSelInit = 500),
                Filter2 = list(active = FALSE,
                               colSelected = "mzMatches",
                               numeric = FALSE,
                               txtSelInit = "",
                               modeSelInit = "is not"))
    
    tab1fil <- FTFilter(tab1ana, filters = fil2, sortBy = character())
    expect_equal(nrow(tab1fil$df),3)
    
    fil2 <- list(Filter1 = list(active = TRUE,
                                colSelected = "mz",
                                numeric = TRUE,
                                minSelInit = 300,
                                maxSelInit = 500),
                 Filter2 = list(active = TRUE,
                                colSelected = "mzMatches",
                                numeric = FALSE,
                                txtSelInit = "",
                                modeSelInit = "is not"))
    
    tab1fil <- FTFilter(tab1ana, filters = fil2, sortBy = character())
    expect_equal(nrow(tab1fil$df),0)
    
})

test_that("analyzeFT S4 method works",{
    
    df1 <- buildMseekFT(data.frame(mz = c(101:105),
                      rt = c(201:205),
                      file1__XIC = c(1000,1000,1000,1000,1000),
                      file2__XIC = c(0,1000, 1000,2000,3000),
                      file3__XIC = c(1000,0,1500,500,0),
                      file4__XIC = c(1000,0,500,100,1000)
    ))
    expect_s3_class({
        Metaboseek:::analyzeFT(object = tab1,
                               MSData = NULL,
                               param = FTAnalysisParam(analyze = "Basic analysis",
                                                       controlGroup = "G1")
        )
         }, "MseekFT")
        
        FTA_param <- FTAnalysisParam(intensities = character(),
                                     groups = list(),
                                     .files = character(),
                                     analyze = c("Basic analysis"), 
                                     normalize = T,
                                     useNormalized = T,
                                     logNormalized = F,
                                     ppm = 5,
                                     controlGroup = NULL,
                                     numClusters = 2,
                                     mzMatchParam = list(db ="smid-db_pos.csv",
                                                         ppm = 5,
                                                         mzdiff = 0.001),
                                     workers = 1)
        
        reanalyzed <- Metaboseek:::analyzeFT(with_ints, MSD$data, FTA_param)
        
        previousStep(reanalyzed)
        
        #run all analyses at once:
        all_param <- FTAnalysisParam(mzMatchParam = list(db =system.file("db","smid-db_pos.csv",package = "Metaboseek"),
                                                         ppm = 5,
                                                         mzdiff = 0.001))
        
        fullyAnalyzed <- Metaboseek:::analyzeFT(tab1, MSD$data, all_param)
        
        #all steps wothout error?
        expect_false(any(sapply(processHistory(fullyAnalyzed), hasError)))
   
})

# context("xcms integration and object histories")
# 
# test_that("xcmsRunner functions work",{
#     
#     
#     analyzeFT(ttt)
#     
#     if(file.exists("./AA_Local")){
#         setwd("./AA_Local/")
#     }
#     
#     progress <- savetable(xset2,
#                           status = NULL,
#                           fill = xcms::FillChromPeaksParam(expandMz = 0.005,
#                                                            expandRt = 5, ppm = 3),
#                           nonfill = T,
#                           filename = "tableout_xset",
#                           bparams = BiocParallel::bpparam(),
#                           intensities = list(ppm = 5,
#                                              rtw = 5,
#                                              rtrange = T),
#                           rawdata = NULL,
#                           saveR = T,
#                           postProc = NULL)
#     
#     progressAN <- savetable(an,
#                             status = NULL,
#                             fill = xcms::FillChromPeaksParam(expandMz = 0.005,
#                                                              expandRt = 5, ppm = 3),
#                             nonfill = T,
#                             filename = "tableoutxxAN",
#                             bparams = BiocParallel::bpparam(),
#                             intensities = list(ppm = 5,
#                                                rtw = 5,
#                                                rtrange = T),
#                             rawdata = NULL,
#                             saveR = T,
#                             postProc = NULL)
#     
#     getwd()
#     
#     processHistory(an)
# })