context("Modifications of feature tables")

test_that("UpdateDF transfers data from one df to the other correctly",{
  
  df2_1 <- data.frame(mz = c(301:305),
                             rt = c(201:205),
                             file1__XIC = c(1000,1000,1000,1000,1000),
                             file2__XIC = c(0,1000, 1000,2000,3000),
                             file3__XIC = c(1000,0,1500,500,0),
                             file4__XIC = c(1000,0,500,100,1000),
                      results = c(1:5)
  )
  
  expect_that(updateDF(df2,df1), equals(df2_1))
  
  
  
})