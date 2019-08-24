context("methods MseekFT")


MseekExamplePreload()

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

tab1 <- Metaboseek:::buildMseekFT(xset_rtcorr_grouped)

tab2<- Metaboseek:::buildMseekFT(CAMERA_res)
test_that("utility functions", {

    expect_equal(rtexport(CAMERA_res),
                 rtexport(xset_grouped))

    
})
    
    
test_that("removeNAs works", {
    tab1wNAs <- tab1
    
    tab1$df[is.na(tab1$df)] <- 0
    
#    tab1wNAs$df[tab1wNAs$df == 0] <- NA
    
    expect_true(any(is.na(tab1wNAs$df)))
    
    afterRemoval <- removeNAs(tab1wNAs)
    
    expect_equal(tab1$df,
                 afterRemoval$df)
    
    expect_equal(length(processHistory(tab1))+1,
                 length(processHistory(afterRemoval)))
    
  }
)

test_that("we can get Mseek intensities",{
    
    expect_true(hasError(previousStep(getMseekIntensities(tab1, MSD$data[1:2]))))
    expect_true(all(paste0(basename(names(MSD$data[1:2])), "__XIC") %in% colnames(getMseekIntensities(ints, MSD$data[1:2], adjustedRT = FALSE)$df)))
})

with_ints <- getMseekIntensities(ints, MSD$data, adjustedRT = FALSE)

test_that("intensityCols and FTNormalize work",{
  expect_true(hasError(previousStep(FTNormalize(with_ints))))
  
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
  
})

##a data.frame for testing mass deffect function
dfdef <- data.frame(mz = c(101.11111, 102.22222, 103.33333, 104.44444, 105.55555),
                    rt = c(201:205)
)  
  
test_that("raiseZeros arg works after normalization", {
  #mxt is a test matrix and should only exists within the test
  mxt <- featureTableNormalize(mx1, raiseZeros = 20) 
  expect_that(mxt[1,1], equals(20))
})

test_that("log arg works after normalization", {
  mxt <- featureTableNormalize(mx1, log = 1)
  expect_that(mxt[2,1], equals(0))
})





test_that("foldChanges are calculated correctly",{
  
  df1fC1 <- data.frame(maxint = c(1000,1000,1500,2000,3000),
                       topgroup = c("G2", "G1", "G1", "G1", "G1"),
                       maxfold = c(2,Inf,1,5,4),
                       maxfoldover2 = c(2,Inf,1,5,4),
                       G1__minInt = c(0,1000,1000,1000,1000),
                       G1__meanInt = c(500,1000,1000,1500,2000),
                       G1__foldOverRest = c(0.5,Inf,1,5,4),
                       G1__minFold = c(0,Inf,2/3,2,1),
                       G1__minFoldMean = c(0.5,Inf,2/3,3,2),
                       G1__foldOverCtrl = c(1,1,1,1,1),
                       G1__minFoldOverCtrl = c(0,1,1,0.5,1/3),
                       G2__minInt = c(1000, 0, 500, 100, 0),
                       G2__meanInt = c(1000,0,1000,300,500),
                       G2__foldOverRest = c(2,0,1,0.2,0.25),
                       G2__minFold = c(1,0,0.5,0.05,0),
                       G2__minFoldMean = c(1,0,1,3/20,1/6),
                       G2__foldOverCtrl = c(2,0,1,0.2,0.25),
                       G2__minFoldOverCtrl = c(1,0,0.5,0.05,0),
                       best_minFold = c(1,Inf,2/3,2,1),
                       best_minFoldMean = c(1,Inf,1,3,2),
                       best_minFoldCtrl = c(1,1,1,0.5,1/3),
                       stringsAsFactors = F)
  
  
  
  expect_that(foldChange(as.matrix(df1[,c("file1__XIC","file2__XIC","file3__XIC", "file4__XIC")]),
                         groups = list(G1 = c("file1__XIC","file2__XIC"),
                                       G2 = c("file3__XIC","file4__XIC")),
                         ctrl = "G1", calc = "mean", topgroup = T, 
                         maxFold = T,  foldMaxK = 2, foldmode = "simple"),
              equals(df1fC1))
  
  
  
})


test_that("mass defect is correctly calculated",{
  dfdeft <- featureCalcs(dfdef, massdef = T)
  expect_equal(dfdeft[[1]][[1]], 1098.890122)
}
)

test_that("analyzeFT S4 method works",{
  
    df1 <- data.frame(mz = c(101:105),
                      rt = c(201:205),
                      file1__XIC = c(1000,1000,1000,1000,1000),
                      file2__XIC = c(0,1000, 1000,2000,3000),
                      file3__XIC = c(1000,0,1500,500,0),
                      file4__XIC = c(1000,0,500,100,1000)
    )
    expect_s3_class({
        Metaboseek:::analyzeFT(object = df1,
              MSData = NULL,
              param = FTAnalysisParam(intensities = c("file1__XIC","file2__XIC",
                                                       "file3__XIC", "file4__XIC"),
                                  groups = list(G1 = c("file1__XIC","file2__XIC"),
                                                G2 = c("file3__XIC","file4__XIC")),
                                  analyze = "Basic analysis",
                                  controlGroup = "G1")
               )
    
    tab1 <- constructFeatureTable (df= read.csv(system.file("extdata","examples", "example_projectfolder", "mini_example_features.csv", package = "Metaboseek"), stringsAsFactors = F),# data frame 
                                   mzcol= "mz", #
                                   rtcol= "rt", #column in df with mz values (columnname)
                                   commentcol = "comments",
                                   fragmentcol = "fragments",
                                   rtFormat = "sec", # "sec" or "min" 
                                   anagrouptable = read.csv(system.file("extdata","examples", "example_projectfolder", "analysis_groups.csv", package = "Metaboseek"), stringsAsFactors = F),
                                   tablename = "mini_example_features.csv",
                                   editable = F)
    
    Metaboseek:::analyzeFT(object = tab1,
              MSData = NULL,
              param = FTAnalysisParam(analyze = "Basic analysis",
                                      controlGroup = "G1")
    )
    }, "MseekFT")
})


context("xcms integration and object histories")

test_that("xcmsRunner functions work",{
    
    
    analyzeFT(ttt)
    
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