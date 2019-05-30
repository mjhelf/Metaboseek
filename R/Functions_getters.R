#' FeatureTable
#' 
#' @param values reactiveValues or list object with at least a values$featureTables$tables
#' @param tableID table name
#' 
#' @export
FeatureTable <- function(x, ...){
    
    UseMethod('FeatureTable', x)
    
}

#' @export
FeatureTable.reactivevalues <- function(x, update = F, tableID = NULL){
    
    if(update){        
        #make sure this part does not trigger the observer containing the setter function
        isolate({
            updateFT(x)
        })
    }
    
    if(is.null(tableID) || missing(tableID)){
        return(x$featureTables$tables[[x$featureTables$active]])
    }else{
        return(x$featureTables$tables[[tableID]])
    }
    
}




#' FeatureTable<-
#'
#' Getter and setter methods to retireve a Feature Table from \code{values} 
#'
#'
#' @param x the \code{values} object
#' @param value the value that gets set
#' @param replace
#'
#' @export
'FeatureTable<-' <- function(x, value, ...){
    
    UseMethod('FeatureTable<-', value)
    
}

#' @export
'FeatureTable<-.data.frame' <- function(x, value, replace = F, update = T, tableID = NULL){
    
    if(!missing(value) && !is.null(value)){
        
        if(update){        
            #make sure this part does not trigger the observer containing the setter function
            isolate({
                  updateFT(x)
                })
        }
        
        
        if(is.null(tableID) || missing(tableID)){
        tableID <- x$featureTables$active
        }
        if(replace){
        x$featureTables$tables[[tableID]]$df <- value
        }else{
        x$featureTables$tables[[tableID]] <- updateFeatureTable(x$featureTables$tables[[tableID]], value)
        }
        
    }
    
    #if x is not returned, values becomes value in the local environment of the observer,
    #breaking any line of code expecting the MseekValues type values inside the observer 
    return(x)
    
    
    
}

#' @export
'FeatureTable<-.MseekFT' <- function(x, value, replace = T, tableID = NULL){
    
    if(!missing(value) && !is.null(value)){
        if(is.null(tableID) || missing(tableID)){
            tableID <- x$featureTables$active
        }
        x$featureTables$tables[[tableID]] <- value
    }
    #if x is not returned, values becomes value in the local environment of the observer,
    #breaking any line of code expecting the MseekValues type values inside the observer 
    return(x)
    
}



#' updateFT
#' 
#' 
#' @export
updateFT <- function(values){
 
    if(!is.null(values$featureTables) 
       && values$featureTables$MainTable$hasUpdates
    ){
        if((!is.null(values$featureTables$tables[[values$featureTables$active]]$editable) 
            && values$featureTables$tables[[values$featureTables$active]]$editable)
           || is.null(values$featureTables$MainTable$liveView$comments) ){
            values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$MainTable$liveView),colnames(values$featureTables$MainTable$liveView)] <- values$featureTables$MainTable$liveView
        }else{
            values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$MainTable$liveView),"comments"] <- values$featureTables$MainTable$liveView$comments
        }
    }   
    
}

#' FTselection
#'
#' get the rows that are currently selected in MainTable window.
#' Note: by default, will get its data from the underlying Feature Table,
#'  not using changes (such as comments) made immediately prior to selection, 
#'  but including all columns, including those not visible in the current table view.
#'
#' @param x a MseekValues (reactivevalues) object
#' @param liveView if TRUE, will get data from the liveView of the MainTable directly (default: FALSE)
#'
#' @export
FTselection <- function(x, ...){

    UseMethod('FTselection', x)
    
    
    }

#' @export
FTselection.reactivevalues <- function(x, liveView = F){
    if(liveView){
    return(
    x$featureTables$Maintable$liveView[x$featureTables$Maintable$selected_rows,]
    )
    }else{
        return(
            FeatureTable(x)$df[row.names(x$featureTables$Maintable$liveView)[x$featureTables$Maintable$selected_rows],]
        )  
    }
    
}

# setMethod("getScanInfo", 
#           signature = c("character", "numeric", "MSnExp"),
#           function(f,n,d, ...){
#               getScanInfo(f,n,d, ...)
#           })


#' findMSnScans
#'
#' Get a list of MS scans based on parent m/z and retention time
#' Will return all scans with parentMZ within mz +/- ppm OR mzwid, AND retentionTime within rt +/- rtwid.
#' 
#'   
#' @param data an OnDiskMSnExp or MseekValues (reactivevalues) object
#' @param mz parentMZ value to search for. mz and rt can be vectors, but need to be of equal length
#' @param rt retention time to search for. mz and rt can be vectors, but need to be of equal length
#' @param ppm mz tolerance in ppm
#' @param mzwid mz tolerance in absolute mz values 
#' @param rtwid rt tolerance in seconds
#' @param MSlevel which mz level to 
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
findMSnScans.OnDiskMSnExp <- function(data, mz, rt, ppm = 5, mzwid = 0, rtwid = 30, MSlevel = 2){
    
    
    if(length(mz) > 1 && length(mz) == length(rt)){
        presel <- data@featureData@data$msLevel == MSlevel &  data@featureData@data$retentionTime
        
       allmzdiffs <-  abs(outer(mz, data@featureData@data$precursorMZ[presel], "-"))
        
       sel <- unique(which(allmzdiffs/mz <= ppm*1e-6 | allmzdiffs <= mzwid, arr.ind = T)[,2])
       
       rtsel <- unique(which(abs(outer(rt, data@featureData@data$retentionTime[presel][sel], "-")) <= rtwid, arr.ind = T)[,2])
       
       res <- data@featureData@data[presel,][sel,][rtsel,]
       
    }else{
        if(length(mz) != length(rt)){warning("mz and rt were not of equal length in call to METABOseek::findMSnScans")}
        
    sel <- (data@featureData@data$msLevel == MSlevel 
            & (abs(data@featureData@data$precursorMZ - mz[1])/mz[1] <= ppm*1e-6*mz[1] | abs(data@featureData@data$precursorMZ - mz[1]) <= mzwid )
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

#' getSpectra
#'
#' get a list of spectra using indices from the OnDiskMSnExp object
#' 
#' @param data an OnDiskMSnExp or MseekValues (reactivevalues) object
#' @param scanlist as returned by makeScanlist2()
#' @param index a numeric vector of scan indices
#' @param xcmsRawobject list of xcmsRaw objects
#' @param SpectrumClass if TRUE, will return a list of Spectum class objects, 
#' otherwise will return a list of matrices with mz and intensity values
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
getSpectra.OnDiskMSnExp <- function(data, index, xcmsRawobject = NULL, SpectrumClass = F){
    
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