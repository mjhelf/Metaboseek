
context("Run Mseek")


#recordTest("./tests/testthat/FullMseek", loadTimeout = 120000)


test_that("Mseek app starts up", {
  expect_pass(testApp("FullMseek", "mytest.R", compareImages = FALSE, quiet = T))
  
})

#' NOTES:
#' in "MinimalMseekExampleData", 1714942.5 is rounded down to 1714942 in Linux, but rounded up to 1714943 in Windows.
#' Expected example output is the linux value
#' 
#' 