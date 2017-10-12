library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(rhandsontable)
library(shinyBS)
library(BiocParallel)
library(xcms)
library(shinyFiles)
#source("Functions/EIC_reader_v8-functions_p10.R")
enableBookmarking("server")
timeStamp <-  strftime(Sys.time(),"%Y%m%d_%H%M%S")
Mosaic_mode <- "server"
servermode <- F


# activate features in servermode:
activateXCMS <- F
activateLocalFiles <- F
rootpath <- c(root = getwd())  ##the root path for file selection (for server - set with care)

enabledCores <- 10
options(scipen = 5)
#source functions:
#source("functions/class_feature_table.R")
#source("functions/feature_table_data_analysis.R")
#source("functions/plotting_stats.R")
#source("functions/class_msdata.R")
#source("functions/plotting_msdata.R")



#source modules:
source("modules/Statistics.R")
source("modules/Filter.R")