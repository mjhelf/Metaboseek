
#' getSpectra
#'
#' get a list of spectra using indices from the OnDiskMSnExp object
#' 
#' @param data an OnDiskMSnExp or MseekValues (reactivevalues) object
#' @param index a numeric vector of scan indices
#' @param xcmsRawobject list of xcmsRaw objects
#' @param SpectrumClass if TRUE, will return a list of Spectum class objects, 
#' otherwise will return a list of matrices with mz and intensity values
#' @param ... arguments passed to \code{getSpectra.OnDiskMSnExp}
#' 
#' @return a list of Spectrum1 or Spectrum2 objects
#'
#' @rdname getSpectra
#' @export
getSpectra <- function(data, ...){
    
    UseMethod('getSpectra', data)
    
}

#' @rdname getSpectra
#' @export
getSpectra.reactivevalues <- function(data, ...){
    
    getSpectra(data = data$MSData$MSnExp, xcmsRawobject = data$MSData$data, ...)
    
}

#' @rdname getSpectra
#' @export
getSpectra.OnDiskMSnExp <- function(data, index,
                                    xcmsRawobject = NULL, SpectrumClass = F){
    
    if(!is(data, "OnDiskMSnExp")){
        simpleError("No valid OnDiskMSnExp object provided") 
    }
    
    if(is.null(xcmsRawobject)){
        
        #  return( spectra(data)[index])
        
        res <- lapply(index, function(i){data[[i]]} )
        if(!SpectrumClass){
            res <- lapply(res,function(x){matrix(c(x@mz, x@intensity),ncol = 2, dimnames = list(NULL, c("mz", "intensity")))})
        }
        
    }else{
        res <-  lapply(index,function(i){
            meta <- data@featureData@data[i,]
            
            switch(meta$msLevel,
                   {
                       
                       
                       spec <- xcms::getScan(xcmsRawobject[[as.numeric(meta$fileIdx)]],
                                             which(xcmsRawobject[[as.numeric(meta$fileIdx)]]@acquisitionNum == meta$acquisitionNum))
                       
                       if(SpectrumClass){
                           MSnbase:::Spectrum1(peaksCount = length(spec[,1]), rt = meta$retentionTime,
                                               acquisitionNum = meta$acquisitionNum, scanIndex = meta$spIdx,
                                               tic = meta$totIonCurrent, mz = spec[,1], intensity = spec[,2],
                                               fromFile = meta$fileIdx, centroided = meta$centroided,
                                               smoothed = meta$smoothed,
                                               polarity = meta$polarity)
                       }else{return(spec)}
                   },
                   { 
                       spec <- xcms::getMsnScan(xcmsRawobject[[as.numeric(meta$fileIdx)]],
                                                which(xcmsRawobject[[as.numeric(meta$fileIdx)]]@msnAcquisitionNum == meta$acquisitionNum))
                       
                       if(SpectrumClass){
                           
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
                       }else{return(spec)}
                   })
            
            
            
        })
    }
    
    names(res) <- row.names(data@featureData@data[index,])
    
    return(res)
}


#' findMSnScans
#'
#' Get a list of MS scans based on parent m/z and retention time
#' Will return all scans with parentMZ within mz +/- ppm OR mzwid,
#'  AND retentionTime within rt +/- rtwid.
#' 
#' @return a subset of the \code{data@@featureData@@data} data.frame matching
#' the search criteria, with an additional \code{filename} column
#'   
#' @param data an OnDiskMSnExp or MseekValues (reactivevalues) object
#' @param mz parentMZ value to search for. mz and rt can be vectors,
#'  but need to be of equal length
#' @param rt retention time to search for. mz and rt can be vectors,
#'  but need to be of equal length
#' @param ppm mz tolerance in ppm
#' @param mzwid mz tolerance in absolute mz values 
#' @param rtwid rt tolerance in seconds
#' @param MSlevel which mz level to 
#' @param ... arguments passed to \code{findMSnScans.OnDiskMSnExp}
#' 
#' @rdname findMSnScans
#'
#' @export
findMSnScans <- function(data, ...){
    
    UseMethod('findMSnScans', data)
    
}

#' @rdname findMSnScans
#' @export
findMSnScans.reactivevalues <- function(data, ...){
    
    findMSnScans(data$MSData$MSnExp, ...)
    
}

#' @rdname findMSnScans
#' @export
findMSnScans.OnDiskMSnExp <- function(data, mz, rt,
                                      ppm = 5, mzwid = 0,
                                      rtwid = 30, MSlevel = 2){
    
    
    if(length(mz) > 1 && length(mz) == length(rt)){
        presel <- data@featureData@data$msLevel == MSlevel &  data@featureData@data$retentionTime
        
        allmzdiffs <-  abs(outer(mz, data@featureData@data$precursorMZ[presel], "-"))
        
        sel <- unique(which(allmzdiffs/mz <= ppm*1e-6 | allmzdiffs <= mzwid, arr.ind = T)[,2])
        
        rtsel <- unique(which(abs(outer(rt, data@featureData@data$retentionTime[presel][sel], "-")) <= rtwid, arr.ind = T)[,2])
        
        res <- data@featureData@data[presel,][sel,][rtsel,]
        
    }else{
        if(length(mz) != length(rt)){warning("mz and rt were not of equal length in call to METABOseek::findMSnScans")}
        
        
        sel <- (data@featureData@data$msLevel == MSlevel 
                & (abs(data@featureData@data$precursorMZ - mz[1])/mz[1] <= ppm*1e-6 | abs(data@featureData@data$precursorMZ - mz[1]) <= mzwid )
                & abs(data@featureData@data$retentionTime - rt[1]) <= rtwid)
        
        res <- data@featureData@data[sel,]
    }
    
    res$filename <- as.character(data@phenoData@data$sampleNames)[res$fileIdx]
    
    return(res)
    
}


#' getScanInfo
#'
#' get metadata for a scan or multiple scans
#' 
#' @return a subset of the \code{data@@featureData@@data} data.frame matching
#' the search criteria, with an additional \code{filename} column
#' 
#' @param file  character vector of MS data file basenames.
#'  Files must be loaded in data.
#' @param number scan for which to get metadata. Can be an acquisition 
#' number or an index of an ms1 or ms2 scan
#' @param data an MSnExp or OnDiskMSnExp object. NYI: If NULL, will assume 
#' that values$MSData$MSnExp is defined in parent environment
#' @param type specify what the scan number represents, either 
#' "acquisition", "ms1" or "ms2".
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
