context("Feature table analysis")

##some simple data frames
df1 <- data.frame(mz = c(101:105),
                  rt = c(201:205),
                  file1__XIC = c(1000,1000,1000,1000,1000),
                  file2__XIC = c(0,1000, 1000,2000,3000),
                  file3__XIC = c(1000,0,1500,500,0),
                  file4__XIC = c(1000,0,500,100,1000)
)

##some simple matrices
mx1 <- matrix(0:15, 4,4)

test_that("feature tables are returned as matrices after normalization", {
  expect_that(featureTableNormalize(mx1), is_a("matrix"))
  }
)

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

test_that("normalization arg works after normalization",{
  mxt <- featureTableNormalize(mx1, normalize = 1)
  expect_that(mxt[2,1], equals (5))
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
