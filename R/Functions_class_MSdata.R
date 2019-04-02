#' constructRawLayout
#' 
#' Constructor function for the rawLayout S3 class, holding information on MSdata grouping and layout options in METABOseek.
#' 
#' @param rawgrouptable a data.frame with columns File and Group , holding file paths and group names, respectively.
#' @param stem if the file paths in rawgrouptable are not full (e.g. subdirectories of the working directory), this should be the path of the working directory.
#' 
#' @export
constructRawLayout <- function(rawgrouptable, stem=NULL){
    
    MSD = list()
    MSD$stem <- stem
    MSD$rawgrouptable <- rawgrouptable
    if(is.null(MSD$rawgrouptable$Group2)){
      MSD$rawgrouptable$Group2 <- MSD$rawgrouptable$Group
    }
    MSD$filelist <- paste0(stem, rawgrouptable$File)
    MSD$grouping = rawGrouping(data.frame(File = MSD$filelist, Group = rawgrouptable$Group))
    
    MSD$grouping2 = rawGrouping(data.frame(File = MSD$filelist, Group = if(!is.null(rawgrouptable$Group2)){rawgrouptable$Group2}else{rawgrouptable$Group}) )


    MSD$settings = list(rtw = 30,
                        ppm = 5,
                        cols = 1,
                        colr = 'Mseek.colors',
                        alpha = 0.8)
    
    class(MSD) <- "rawLayout"
    return(MSD)
}

#' updateRawLayout
#' 
#' Updates the file path information in a rawLayout object
#' 
#' @param MSD a rawLayout object with a defined stem
#' @param stem if the file paths in rawgrouptable are not full (e.g. subdirectories of the working directory), this should be the path of the working directory.
#' 
#' @export
updateRawLayout <- function(MSD, new.stem=NULL){
    
    MSD$filelist <- gsub(MSD$stem,new.stem,MSD$filelist)
    MSD$grouping = rawGrouping(data.frame(File = MSD$filelist, Group = MSD$rawgrouptable$Group))
    MSD$grouping2 = rawGrouping(data.frame(File = MSD$filelist,
                                           Group = if(!is.null(MSD$rawgrouptable$Group2)){MSD$rawgrouptable$Group2}else{MSD$rawgrouptable$Group}))

    MSD$stem <- new.stem
    return(MSD)
}


#' loadRawM
#' 
#' TIME CONSUMING. This step does not need to be repeated when adjusting other parameters
#' (e.g. feature list, EIC ppm or RT) Generates an R-readable data structure (a list of xcmsRaw objects) 
#' in memory from MS data files defined in the file list.
#' 
#' Note that there is an xcms function of the same name.
#' 
#' @param filelist a list of mzXML or mzML files (character vector)
#' @param MSn should MSn data be read in? defaults to TRUE
#' @param workers How many cores to use (cf. BiocParallel and SnowParam, argument only used if more than 10 files are loaded).
#' @param rnames names of the xcmsRaw objects in the list returned, defaults to the filepaths of the source files.
#' 
#' @importFrom BiocParallel SnowParam bplapply
#' @importFrom xcms xcmsRaw
#' 
#' @export
##Parallel enabled version for larger number of files
##NOTE: not equivalent to xcms function loadRaw
loadRawM <- function (filelist= mzxml_pos, MSn = T, workers=10, rnames = filelist){

    if (length(filelist)<=10){workers<-1}
    param <- SnowParam(workers = workers)
    suppressWarnings({
    rawcoll <- bplapply(filelist,xcmsRaw,  profstep=0, includeMSn = MSn, BPPARAM= param)
    })
    names(rawcoll)<- rnames
    
    return(rawcoll)}



#' rawGrouping
#' 
#' Groups a rawgrouptable into a named list.
#' 
#' @param rawgrouptable a data.frame with columns File and Group , holding file paths and group names, respectively.
#' 
#' @export
rawGrouping <- function(rawgrouptable){
    ## Make list object of grouped column names                                        
    colme <- list()
    for (l in unique(rawgrouptable$Group)){
        colme[[l]] <- as.character(rawgrouptable$File[which(rawgrouptable$Group==l)])
    }
    
    return(colme)
}

#' makeColorscheme
#'
#' Use two grouing lists to generate a color scheme for groupPlot() and EICgeneral().
#' 
#' @param maingroup main plot grouping scheme (e.g. grouping in rawlayouts)
#' @param colorgroup grouping to be used for coloring (e.g. grouping2 in rawlayouts)
#' @param colrange function to use to make color range
#' @param transparency setting for transparent coloring
#'
#'
#' @export
makeColorscheme <- function(maingroup = MSD$layouts[[MSD$active]]$grouping,
                            colorgroup = MSD$layouts[[MSD$active]]$grouping,
                            colrange = "Mseek.colors",
                            transparency = 0.8){
  
  colvec <- do.call(colrange,
                    list(n=length(colorgroup), alpha = transparency))
  
  labs <- unlist(sapply(seq(length(colorgroup)),function(n){rep(names(colorgroup)[n],length(colorgroup[[n]]))}))
  colvec <- unlist(sapply(seq(length(colorgroup)),function(n){rep(colvec[n],length(colorgroup[[n]]))}))
  srch <- unlist(colorgroup)
  
  colrs <- list()
  for(i in 1:length(maingroup)){
    
    sel <- sapply(maingroup[[i]],function(s){match(s, srch)})
    
    colrs[[i]] <- data.frame(color = colvec[sel],
                             label = labs[sel],
                             stringsAsFactors = FALSE)
  }
  return(colrs)
}