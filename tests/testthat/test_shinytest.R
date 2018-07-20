
context("Shiny tests")

test_that("Application works", {
  expect_pass(testApp("MinimalMosaic", "mytest.R", compareImages = FALSE))
  expect_pass(testApp("MinimalMosaicExampleData", "mytest.R", compareImages = FALSE))
})

#' NOTES:
#' in "MinimalMosaicExampleData, 1714942.5 is rounded down to 1714942 in Linux, but rounded up to 1714943 in Windows.
#' Expected example output is the linux value
#' 
#' 