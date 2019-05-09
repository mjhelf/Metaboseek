# setMethod("getScanInfo", 
#           signature = c("character", "numeric", "MSnExp"),
#           function(f,n,d, ...){
#               getScanInfo(f,n,d, ...)
#           })



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


#' rawSpectrum2
#'
#' create a list of Spectrum (Spectrum1 and/or Spectrum2) objects from a list of xcmsRaw onjects if available
#'
#' 
getSpectra <- function(index, MSnData, xcmsRawobject = NULL){
    
    if(!is(MSnData, "OnDiskMSnExp")){
        simpleError("No valid OnDiskMSnExp object provided") 
    }
    
    # if(!is(MSnData, "OnDiskMSnExp")){
    #     spectra(MSnData)[index] 
    #     simpleError("No valid OnDiskMSnExp object provided") 
    #     
    # }
    
    if(is.null(xcmsRawobject)){
        
        #  return( spectra(MSnData)[index])
        
        res <- lapply(index, function(i){MSnData[[i]]} )
        
    }else{
        res <-  lapply(index,function(i){
            meta <- MSnData@featureData@data[i,]
            
            switch(meta$msLevel,
                   {
                       
                       
                       spec <- xcms::getScan(xcmsRawobject[[as.numeric(meta$fileIdx)]],
                                             which(xcmsRawobject[[as.numeric(meta$fileIdx)]]@acquisitionNum == meta$acquisitionNum))
                       
                       MSnbase:::Spectrum1(peaksCount = length(spec[,1]), rt = meta$retentionTime,
                                           acquisitionNum = meta$acquisitionNum, scanIndex = meta$spIdx,
                                           tic = meta$totIonCurrent, mz = spec[,1], intensity = spec[,2],
                                           fromFile = meta$fileIdx, centroided = meta$centroided,
                                           smoothed = meta$smoothed,
                                           polarity = meta$polarity)
                   },
                   { 
                       spec <- xcms::getMsnScan(xcmsRawobject[[as.numeric(meta$fileIdx)]],
                                                which(xcmsRawobject[[as.numeric(meta$fileIdx)]]@msnAcquisitionNum == meta$acquisitionNum))
                       
                       Spectrum2(msLevel = meta$msLevel, peaksCount = length(spec[,1]), rt = meta$retentionTime,
                                 acquisitionNum = meta$acquisitionNum, scanIndex = meta$spIdx,
                                 tic = meta$totIonCurrent, mz = spec[,1], intensity = spec[,2],
                                 fromFile = meta$fileIdx, centroided = meta$centroided,
                                 smoothed = meta$smoothed,
                                 polarity = meta$polarity,
                                 merged = meta$mergedScan,
                                 precScanNum = meta$precursorScanNum,
                                 precursorMz = meta$precursorMZ, precursorIntensity = meta$precursorIntensity,
                                 precursorCharge = meta$precursorCharge,
                                 collisionEnergy = meta$collisionEnergy)
                       
                   })
            
            
            
        })
    }
    
    names(res) <- row.names(MSnData@featureData@data[index,])
    
    return(res)
}




#' getAllScans
#'
#' get a list of all scans in a scanlist
#' 
#' @param scanlist as returned by makeScanlist2()
#' @param MSData list of xcmsRaw objects
#' @inheritParams getScanInfo
#'
#'
#' @export
getAllScans <- function(scanlist, MSData, removeNoise = NULL, type = c("ms2", "ms1", "acquisition")){
  
  
  
  
  makeSpeclist <- function(bnames,scannums,MSData, MS2 = T, removeNoise = NULL){
    
    
    fx <- function(MSfile, scan, MS2, removeNoise = NULL){
      # print(MSfile@filepath)
      tryCatch({
        if(MS2){
          if(!is.null(removeNoise)){
            s <- xcms::getMsnScan(MSfile, scan)
            
            sel <- which(s[,2] >= removeNoise*max(s[,2]))
            return(s[sel,, drop = F])
            
          }
          return(xcms::getMsnScan(MSfile, scan))
        }else{
          if(!is.null(removeNoise)){
            s <- xcms::getScan(MSfile, scan)
            
            sel <- which(s[,2] >= removeNoise*max(s[,2]))
            return(s[sel,, drop = F])
          }
          return(xcms::getScan(MSfile, scan))
        }
      },
      error = function(e){NULL})
      
    }
    
    speclist <- mapply(fx,
                       MSfile = MSData[match(basename(bnames),basename(names(MSData)))],
                       scan = scannums,
                       MoreArgs = list(MS2 = MS2,
                                       removeNoise = removeNoise),
                       SIMPLIFY = F
    )
    
    #remove NULL results (from errors)
    speclist <- speclist[which(!sapply(speclist, is.null))]
    
    return(speclist)
  }
  
  
  return(
    mapply(makeSpeclist,
           bnames =  scanlist$file,
           scannums = scanlist$scan,
           MoreArgs = list(MSData = MSData,
                           MS2 = (type[1] == "ms2"),
                           removeNoise = removeNoise),
           SIMPLIFY = T)
  )
}