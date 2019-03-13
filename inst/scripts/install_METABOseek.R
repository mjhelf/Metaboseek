install.packages("devtools")

try({remove.packages("BiocInstaller")})


source("https://bioconductor.org/biocLite.R")
biocLite(c("xcms", "CAMERA","BiocParallel"), ask = F)


devtools::install_github("berlinguyinca/spectra-hash", subdir="splashR")

devtools::install_github("mjhelf/METABOseek",
                         dependencies = TRUE,
                         ref = if(length(commandArgs(trailingOnly=TRUE)) > 0 ){
                           commandArgs(trailingOnly=TRUE)[1]
                         }else{
                           "master"
                         })

devtools::install_github("thomasp85/shinyFiles", ref = "master")

## Start Mosaic; this is all you need to run once you have installed Mosaic.
library(METABOseek)

## run this line if you are using Linux or MacOS - replace by whichever folder contains your MS data (or any of its parent folders)

runMseek(launch.browser = T)