
context("Run Mosaic")


#recordTest("./tests/testthat/FullMosaic", loadTimeout = 120000)


test_that("Mosaic app starts up", {
  expect_pass(testApp("FullMosaic", "mytest.R", compareImages = FALSE, quiet = T))
  
})

#' NOTES:
#' in "MinimalMosaicExampleData", 1714942.5 is rounded down to 1714942 in Linux, but rounded up to 1714943 in Windows.
#' Expected example output is the linux value
#' 
#' 