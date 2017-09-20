##INSTALLING MOSAIC

install.packages("graphics", "Hmisc", "MASS", "plot3D", "heatmaply", 
                 "pvclust","data.table", "DT", "shiny", "rhandsontable", "TeachingDemos", "jsonlite","rcdk")

source("https://bioconductor.org/biocLite.R")
biocLite()
biocLite("xcms","CAMERA","BiocParallel")