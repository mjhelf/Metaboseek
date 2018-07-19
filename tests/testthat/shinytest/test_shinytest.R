
context("Shiny tests")

print(dir.exists("MinimalMosaic"))

test_that("Application works", {
  expect_pass(testApp("MinimalMosaic", "mytest.R", compareImages = FALSE))
  expect_pass(testApp("MinimalMosaicExampleData", "mytest.R", compareImages = FALSE))
})