#' getScanInfo
#'
#' get metadata for a scan or multiple scans
#' 
#' @param file  character vector of MS data file basenames. Files must be loaded in data.
#' @param number scan for which to get metadata. Can be an acquisition number or an index of an ms1 or ms2 scan
#' @param data an MSnExp or OnDiskMSnExp object. NYI: If NULL, will assume that values$MSData$MSnExp is defined in parent environment
#' @param type specify what the scan number represents, either "acquisition", "ms1" or "ms2".
#'
#'
#' @export
getScanInfo <- function(file, number, data, type = c("acquisition", "ms1", "ms2")){
  
#  if(is.null(data)){data <- get("values")$MSData$MSnExp}
  

  if(is.null(data) || length(file) != length(number)){
   
    simpleError("No MS data loaded or lengths of file names and scan numbers differ.")
    
  }
  
  
  findex <- match(file, as.character(data@phenoData@data$sampleNames))
  
  if(any(is.na(findex))){
    if(all(is.na(findex))){
      simpleError("None of the requested files are loaded")
    }else{
    
    warning("Some of these files are not loaded")
    }
    
    
    }
  switch(type[1],
         acquisition = {res <- data@featureData@data[unlist(lapply(which(!is.na(findex)),
                                                    function(n){which(data@featureData@data$fileIdx == findex[n]
                                                                                      & data@featureData@data$acquisitionNum == number[n])})),]},
         ms1 = {res <- data@featureData@data[unlist(lapply(which(!is.na(findex)),
                                            function(n){which(data@featureData@data$fileIdx == findex[n]
                                                        & data@featureData@data$msLevel == 1)[number[n]]})),]},
         ms2 = {res <- data@featureData@data[unlist(lapply(which(!is.na(findex)),
                                                           function(n){which(data@featureData@data$fileIdx == findex[n]
                                                                             & data@featureData@data$msLevel == 2)[number[n]]})),]}
         
         
  )
  
  res$filename <- as.character(data@phenoData@data$sampleNames)[res$fileIdx]
  
  return(res)
  
}
