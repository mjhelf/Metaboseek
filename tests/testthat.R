library(testthat)
library(Metaboseek)
library(shinytest)
# 
  if (!dependenciesInstalled()) {installDependencies()}
  message("Using phantom.js from ", shinytest:::find_phantom(), "\n")

#test_dir("./tests/testthat/")

test_check("Metaboseek")


