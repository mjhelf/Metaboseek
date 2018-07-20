library(testthat)
library(Mosaic)
library(shinytest)
library(xcms) #getgauss() does not work otherwise -- needs fix!

#test_dir("./tests/testthat/")
#test_dir("./tests/testthat/shinytest")

test_check("Mosaic")


