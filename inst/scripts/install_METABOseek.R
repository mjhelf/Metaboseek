install.packages("devtools")

try({remove.packages("BiocInstaller")})

if (!requireNamespace("BiocManager", quietly = TRUE)){
install.packages("BiocManager")}

BiocManager::install(c("xcms", "CAMERA","BiocParallel"))

#devtools::install_github("berlinguyinca/spectra-hash", subdir="splashR")
devtools::install_github("mjhelf/Rdisop")  ##forked from "sneumann/Rdisop"; forked version will not change and should always work. Will change once new Rdisop version is in bioc-release
devtools::install_github("mjhelf/MassTools")

install.packages("shinyFiles")


devtools::install_github("mjhelf/METABOseek",
                         dependencies = TRUE,
                         ref = if(length(commandArgs(trailingOnly=TRUE)) > 0 ){
                           commandArgs(trailingOnly=TRUE)[1]
                         }else{
                           "master"
                         })


## Start Metaboseek; this is all you need to run once you have installed Mosaic.
library(METABOseek)

runMseek(launch.browser = T)