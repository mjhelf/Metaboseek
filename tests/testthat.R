library(testthat)
library(Metaboseek)
library(shinytest)
library(xcms)
# 
  if (!dependenciesInstalled()) {installDependencies()}
  message("Using phantom.js from ", shinytest:::find_phantom(), "\n")

#test_dir("./tests/testthat/")
  mutfiles <-  list.files(system.file("extdata", "examples", "ms1", "mut", package = "Metaboseek"), full.names = T, pattern = ".mzXML")
  
  wtfiles <-  list.files(system.file("extdata", "examples", "ms1", "wt", package = "Metaboseek"), full.names = T, pattern = ".mzXML")
  
  BiocParallel::register(
      BiocParallel::bpstart(
          if(Sys.info()['sysname'] == "Windows"){
              BiocParallel::SnowParam(2)
          }else{
              BiocParallel::MulticoreParam(2)
          }
      )
  )
    
 
  
test_check("Metaboseek")


