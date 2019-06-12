#' constructRawLayout
#' 
#' Constructor function for the rawLayout S3 class,
#'  holding information on MSdata grouping and layout options in METABOseek.
#' 
#' @param rawgrouptable a data.frame with columns File and Group ,
#'  holding file paths and group names, respectively.
#' @param stem if the file paths in rawgrouptable are not full
#'  (e.g. subdirectories of the working directory),
#'   this should be the path of the working directory.
#' @param msnExp if not NULL, must be an \code{OnDiskMSnExp} object,
#'  see \code{details}
#'  
#' @return a \code{rawLayout} object
#' 
#' @details
#' If \code{msnExp} is provided, will generate an element called 
#' \code{MSnExp_summary} in the resulting \code{rawLayout} which is a data.frame
#' containing information that can be used for normalization. Columns in
#'  \code{MSnExp_summary}:
#' \itemize{
#' \item\code{sampleNames}File names of raw files
#' \item\code{bpmeans} mean values of the basepeak intensity for each file
#' \item\code{ticmeans} mean values of the TIC intensity for each file
#' \item\code{normfactor_bp} normalization factor for each file based on base peak
#' \item\code{normfactor_tic} normalization factor for each file based on TIC intensity peak
#' }
#' Multiplying all intensity values for each file with their normfactors will
#' yield the same mean intensity for all files. 
#' 
#' @export
constructRawLayout <- function(rawgrouptable, stem=NULL, msnExp = NULL){
    
    MSD = list()
    MSD$stem <- stem
    MSD$rawgrouptable <- rawgrouptable
    if(is.null(MSD$rawgrouptable$Group2)){
      MSD$rawgrouptable$Group2 <- MSD$rawgrouptable$Group
    }
    MSD$filelist <- paste0(stem, rawgrouptable$File)
    MSD$grouping = rawGrouping(data.frame(File = MSD$filelist,
                                          Group = rawgrouptable$Group))
    
    MSD$grouping2 = rawGrouping(data.frame(File = MSD$filelist,
                                           Group = if(!is.null(rawgrouptable$Group2)){
                                               rawgrouptable$Group2
                                               }else{rawgrouptable$Group}) )


    MSD$settings = list(rtw = 30,
                        ppm = 5,
                        cols = 1,
                        colr = 'Mseek.colors',
                        alpha = 0.8)
    
    if(!is.null(msnExp)){
      
      findex <- match(MSD$filelist,
                      as.character(msnExp@phenoData@data$sampleNames))
      
      if(!any(is.na(findex))){
        
        
    MSD$MSnExp_summary <- data.frame(sampleNames = as.character(msnExp@phenoData@data$sampleNames[findex]),
                                     stringsAsFactors = F)
                                     
    MSD$MSnExp_summary$bpmeans <- sapply(findex,
                                    function(n){mean(msnExp@featureData@data$basePeakIntensity[msnExp@featureData@data$fileIdx == n 
                                                                                                             & msnExp@featureData@data$msLevel == 1])})
    MSD$MSnExp_summary$ticmeans <- sapply(findex,
                                     function(n){mean(msnExp@featureData@data$totIonCurrent[msnExp@featureData@data$fileIdx == n  
                                                                                                          & msnExp@featureData@data$msLevel == 1])})
    
    #calculate normalization factors for each file in this rawLayout
    if(!any(MSD$MSnExp_summary$bpmeans ==0)){
    MSD$MSnExp_summary$normfactor_bp <- mean( MSD$MSnExp_summary$bpmeans)/ MSD$MSnExp_summary$bpmeans
    }
    
    
    if(!any(MSD$MSnExp_summary$ticmeans ==0)){
    MSD$MSnExp_summary$normfactor_tic <- mean( MSD$MSnExp_summary$ticmeans)/ MSD$MSnExp_summary$ticmeans
    }
    
      }
    }
    
    class(MSD) <- "rawLayout"
    return(MSD)
}

#' updateRawLayout
#' 
#' Updates the file path information in a \code{rawLayout} object
#' 
#' @param MSD a \code{rawLayout} object with a defined stem
#' @param stem if the file paths in rawgrouptable are not full
#'  (e.g. subdirectories of the working directory), this should 
#'  be the path of the working directory.
#' 
#' @return An updated \code{rawLayout} object
#' 
#' @export
updateRawLayout <- function(MSD, new.stem=NULL){
    
    MSD$filelist <- gsub(MSD$stem,new.stem,MSD$filelist)
    MSD$grouping = rawGrouping(data.frame(File = MSD$filelist,
                                          Group = MSD$rawgrouptable$Group))
    MSD$grouping2 = rawGrouping(data.frame(File = MSD$filelist,
                                           Group = if(!is.null(MSD$rawgrouptable$Group2)){MSD$rawgrouptable$Group2}else{MSD$rawgrouptable$Group}))

    MSD$stem <- new.stem
    return(MSD)
}


#' loadRawM
#' 
#' TIME CONSUMING. This step does not need to be repeated when adjusting
#'  other parameters (e.g. feature list, EIC ppm or RT) Generates an R-readable
#'   data structure (a list of xcmsRaw objects) in memory from MS data files 
#'   defined in the file list.
#' 
#' This is a wrapper for \code{xcms::\link[xcms]{loadRaw}} that can process
#'  multiple filenames at a time and supports parallel processing
#' 
#' @return a list of \code{xcmsRaw} objects
#' 
#' @param filelist a list of mzXML or mzML files (character vector)
#' @param MSn should MSn data be read in? defaults to TRUE
#' @param workers How many cores to use (cf. BiocParallel and SnowParam,
#' argument only used if more than 10 files are loaded).
#' @param rnames names of the xcmsRaw objects in the list returned,
#'  defaults to the filepaths of the source files.
#' 
#' @importFrom BiocParallel SnowParam bplapply
#' @importFrom xcms xcmsRaw
#' 
#' @export
loadRawM <- function (filelist, MSn = T, workers=1, rnames = filelist){

    if (length(filelist)<=10){workers<-1}
    param <- SnowParam(workers = workers)
    suppressWarnings({
    rawcoll <- bplapply(filelist,xcmsRaw,  profstep=0,
                        includeMSn = MSn, BPPARAM= param)
    })
    names(rawcoll)<- rnames
    
    return(rawcoll)}



#' rawGrouping
#' 
#' Groups a rawgrouptable into a named list.
#' 
#' @param rawgrouptable a data.frame with columns File and Group,
#'  holding file paths and group names, respectively.
#'  
#' @return a named list
#' 
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
#' Use two grouping lists to generate a color scheme
#' that can be passed as an argument for \code{\link{groupPlot}()}
#'  and \code{\link{EICgeneral}()}.
#' 
#' @param maingroup a named list defining the main plot grouping scheme
#' (e.g. grouping in rawlayouts)
#' @param colorgroup a named list defining the grouping to be used for coloring 
#' (e.g. grouping2 in rawlayouts)
#' @param colrange name of the function to use to make color range
#' @param transparency setting for transparent coloring
#'
#' @return a named list of data.frames with color values and labels
#'
#' @export
makeColorscheme <- function(maingroup,
                            colorgroup,
                            colrange = "Mseek.colors",
                            transparency = 0.8){
  
  colvec <- do.call(colrange,
                    list(n=length(colorgroup), alpha = transparency))
  
  labs <- unlist(sapply(seq(length(colorgroup)),function(n){
      rep(names(colorgroup)[n],length(colorgroup[[n]]))}))
  colvec <- unlist(sapply(seq(length(colorgroup)),function(n){
      rep(colvec[n],length(colorgroup[[n]]))}))
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