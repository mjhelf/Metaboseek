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

filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]",
                 "[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
filepattern <- paste(paste("\\.", filepattern, "$", sep = ""), collapse = "|")

Mosaic_mode <- "server"
servermode <- F
fppage <- as.integer(100) #feats per page

# activate features in servermode:
activateXCMS <- T
activateLocalFiles <- F
rootpath <- c(root = default__root)  ##the root path for file selection (for server - set with care)

enabledCores <- 4
options(scipen = 5)



#source modules:
source("modules/Statistics.R")
source("modules/Filter.R")