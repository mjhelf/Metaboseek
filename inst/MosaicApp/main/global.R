library(Mosaic)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(rhandsontable)
library(shinyBS)
library(BiocParallel)
library(xcms)
library(shinyFiles)

timeStamp <-  strftime(Sys.time(),"%Y%m%d_%H%M%S")

filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]",
                 "[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
filepattern <- paste(paste("\\.", filepattern, "$", sep = ""), collapse = "|")


fppage <- as.integer(100) #feats per page

# set up servermode:
servermode <- F
activateXCMS <- T
activateLocalFiles <- T
rootpath <- c(root = "C:/xcms")#default__root)  ##the root path for file selection (for server - set with care)
print(rootpath)

enabledCores <- 4
options(scipen = 5)