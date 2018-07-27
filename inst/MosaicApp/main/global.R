library(Mosaic)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(rhandsontable)
library(shinyBS)
library(BiocParallel)
library(xcms)
library(shinyFiles)

filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]",
                 "[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
filepattern <- paste(paste("\\.", filepattern, "$", sep = ""), collapse = "|")


fppage <- as.integer(100) #feats per page

# set up servermode:
servermode <- F
activateXCMS <- T
activateLocalFiles <- F
rootpath <- c(root = default__root)  ##the root path for file selection (for server - set with care)

enabledCores <- 4