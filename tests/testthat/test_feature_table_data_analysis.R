context("Feature table analysis")

##some simple data frames
df1 <- data.frame(mz = c(101:105),
                  rt = c(201:205),
                  file1__XIC = c(1000,1000,1000,1000,1000),
                  file2__XIC = c(0,1000, 1000,2000,3000),
                  file3__XIC = c(1000,0,1500,500,0),
                  file4__XIC = c(1000,0,500,100,1000)
)


test_that("foldChanges are calculated correctly",{
  
  df1fC1 <- data.frame(maxint = c(1000,1000,1500,2000,3000),
                       topgroup = c("G2", "G1", "G1", "G1", "G1"),
                       maxfold = c(2,Inf,1,5,4),
                       maxfoldover2 = c(2,Inf,1,5,4),
                       G1__meanInt = c(500,1000,1000,1500,2000),
                       G1__foldOverRest = c(0.5,Inf,1,5,4),
                       G1__minFold = c(0,Inf,2/3,2,1),
                       G1__minFoldMean = c(0.5,Inf,2/3,3,2),
                       G1__foldOverCtrl = c(1,1,1,1,1),
                       G1__minFoldOverCtrl = c(0,1,1,0.5,1/3),
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