
#' mergeMS
#' 
#' Merge MS spectra by combining peaks that are within a ppm distance
#' 
#' NOTE: If multiple peaks inside a spectrum match another spectrum, only the one with higher(?) mz will be retained
#' 
#' @param speclist data.frame or matrix containing mz and intensity values of a spectrum (mz in column 1)
#' @param ppm accuracy
#' @param mergeOnly if TRUE, only the merged spectrum is returned
#' 
#' @export
mergeMS <- function(speclist,
                    ppm = 5,
                    mergeOnly = F){
  
  #set up the mergeMS object
  res <- list(mz = NULL,
              intensity = NULL,
              #spectra = speclist,
              names = names(speclist),
              counts = NULL,
              merged = NULL
  )  
  
  res$mz <- data.frame(speclist[[1]][,1])
  res$intensity <- data.frame(speclist[[1]][,2])
  class(res) <- "mergeMS"
  
  #merge find common peaks across spectra
  if(length(speclist)>1){
    for (i in 2:length(speclist)){
      #prepare the data frame
      res$mz[,i] <- rep(NA,nrow(res$mz))
      res$intensity[,i] <- rep(NA,nrow(res$mz))
      
      #for first iteration (comparison with spectrum #1), use all mz values
      rest <- speclist[[i]][,1]
      rest_i <- speclist[[i]][,2]
      
      
      #commpare with all previously analyzed spectra
      for (n in seq(i-1)){
        if(length(rest) > 0){
          if(length(rest) == 1){
            dists <- (res$mz[,n] - rest)/rest*1e6
            pos <- which(abs(dists) < ppm, arr.ind = T)
            
            if(length(pos) > 0){
              res$mz[pos,i] <- rest
              res$intensity[pos,i] <- rest_i
              rest <- numeric(0)
              rest_i <- numeric(0)
            }
          }else{
            dists <- sapply(res$mz[,n], "-", rest)/rest*1e6
            pos <- which(abs(dists) < ppm, arr.ind = T)
            res$mz[pos[,2],i] <- rest[pos[,1]]
            res$intensity[pos[,2],i] <- rest_i[pos[,1]]
            
            #keep unmatched entries for next iteration
            if(length(pos[,1]) > 0){ #this prevents bug if there are no hits
              rest <- rest[-pos[,1]]
              rest_i <- rest_i[-pos[,1]]
            }
          }
        }
        
      }
      
      if(length(rest > 0)){
        fill <- (nrow(res$mz)+1):(nrow(res$mz)+length(rest))
        res$mz[fill,] <- NA
        res$intensity[fill,] <- NA
        
        res$mz[fill,i] <- rest
        res$intensity[fill,i] <- rest_i
        
      }
    }
    
    #how many spectra contain each peak?
    res$counts <- BiocGenerics::ncol(res$mz) - BiocGenerics::rowSums(is.na(res$mz))
    
    res$merged <- as.matrix(data.frame(mz = BiocGenerics::rowSums(res$mz*res$intensity, na.rm = T)/BiocGenerics::rowSums(res$intensity, na.rm = T),
                                       intensity = BiocGenerics::rowSums(res$intensity, na.rm = T)/BiocGenerics::ncol(res$mz)
    ))
    
    #order all data in ascending average mz order:
    ord <- order(res$merged[,1])
    
    
    res$mz <- res$mz[ord,]
    res$intensity <- res$intensity[ord,]
    res$counts <- res$counts[ord]
    res$merged <- res$merged[ord,]
    
    
  }else{
    res$counts <- rep(1,length(res$mz[,1]))
    res$merged <- speclist[[1]]
  }
  
  if(mergeOnly){
    return(res$merged)
  }else{
    
    if(length(res$names) == length(speclist)){
      colnames(res$mz) <- basename(res$names)
      colnames(res$intensity) <- basename(res$names)
    }
    
    return(res)
  }
}


#' makeScanlist
#' 
#' Make a scan list
#' 
#' @param splitme character string with format filename:scannumber###filename:scannumber###... as found in gnps networking output tables
#' @param MSData list of xcmsRaw objects. Only scans from files loaded in this object will be returned
#' 
#' @export
makeScanlist <- function(splitme, MSData = NULL){
  pounds <- gregexpr('###',splitme)[[1]]
  pounds <- c(-2,pounds)
  
  subs <- character(length(pounds)-1)
  for(i in seq(length(pounds)-1)){
    subs[i] <- substr(splitme,pounds[i]+3,pounds[i+1]-1)
  }
  
  scantab <- data.frame(file = character(length(subs)),
                        acquisition = integer(length(subs)),
                        scan = integer(length(subs)),
                        rt = numeric(length(subs)),
                        parentMz = numeric(length(subs)),
                        parentIntensity = numeric(length(subs)),
                        stringsAsFactors = F)
  
  for(i in seq(length(subs))){
    colon <- regexpr(':',subs[i])
    scantab$file[i] <- substr(subs[i],1,colon-1)
    scantab$acquisition[i] <- as.integer(substr(subs[i],colon+1,nchar(subs[i])))
    
    if(!is.null(MSData) && sum(basename(names(MSData)) == scantab$file[i]) ==1){
      rawsel <- which(basename(names(MSData)) == scantab$file[i])
      
      
      scantab$file[i] <- basename(names(MSData)[rawsel])
      
      
      scantab$scan[i] <- which(MSData[[rawsel]]@msnAcquisitionNum == scantab$acquisition[i])
      scantab$rt[i] <- MSData[[rawsel]]@msnRt[scantab$scan[i]]
      scantab$parentMz[i] <- MSData[[rawsel]]@msnPrecursorMz[scantab$scan[i]]
      scantab$parentIntensity[i] <- MSData[[rawsel]]@msnPrecursorIntensity[scantab$scan[i]]
      
      
    }
  }
  
  return(scantab)
  
}

#' Parentsearch
#'
#' Make a list of all MS2 scans with the defined parent masses at a given retention time
#' 
#'  @param xcmsRaws list of xcmsRaw objects
#'  @param mz parent mz values (mumeric vector)
#'  @param rt parent retention time (in seconds, numeric vector), needs to be same length as mz
#'  @param ppm parent mz tolerance in ppm
#'  @param rtw parent rt tolerance in seconds
#'
#' @export
Parentsearch <- function (xcmsRaws,
                          mz = c(499.11085),
                          rt = c(366.65),
                          ppm = 5,
                          rtw = 200){
  
  fx <- function(rfile, mz, rt, ppm, rtw){
    
    if(length(rfile@msnPrecursorMz) > 0 ){
      sel <- which( abs((rfile@msnPrecursorMz - mz)) < ppm*mz*1e-6
                    &  abs(rfile@msnRt - rt ) < rtw )
      
      if(length(sel) >0){
        scantab <- data.frame(file = rep( basename(rfile@filepath[[1]]) ,length(sel)),
                              acquisition = rfile@msnAcquisitionNum[sel],
                              scan = sel,
                              rt = rfile@msnRt[sel],
                              parentMz = rfile@msnPrecursorMz[sel],
                              parentIntensity = rfile@msnPrecursorIntensity[sel],
                              charge = rfile@msnPrecursorCharge[sel],
                              ppm = 1e6*(rfile@msnPrecursorMz[sel]-mz)/mz,
                              stringsAsFactors = F)
        
        return(scantab)
      }
    }
    
    
    return(NULL)
  }
  res <- list()
  for(i in length(mz)){
    res <- c(res, lapply(xcmsRaws, fx, mz[i], rt[i], ppm, rtw))
  }
  
  return(data.table::rbindlist(res))
}


#' saveScanlist
#'
#' makes a string representation of a MS2 parent list, including only the basenames and scan # (not acquisition #) of MS2 scans in that list.
#'
#' @param scanlist a scanlist as generated by makeScanlist or Parentsearch()
#'
#'
#' @export
saveScanlist <- function(scanlist){
  
  if(nrow(scanlist) <1){
    return("")
  }
  
  return(paste(sapply(split(scanlist, scanlist$file), function(x){paste0(x$file[1],":",paste(x$scan, collapse = " "))}), collapse = "|"))
  
}

#' listMS2scans
#'
#' Searches for MS2 scans with a given parent m/z and directly ceonverts the resulting table into a single string
#'
#' Also, see \code{\link{Parentsearch}}
#' 
#' @param mz expected target (parent) m/z
#' @param rt expected target (parent) retention time in seconds
#' @param ppm target (parent) m/z tolerance in ppm
#' @param rtw target (parent) rt tolerance in seconds
#' @param MSData list of xcmsRaw objects
#'
#' @export
listMS2scans <- function(mz,rt,ppm,rtw,MSData){
  return(saveScanlist(Parentsearch(MSData, mz, rt, ppm, rtw)))
}

#' makeScanlist2
#' 
#' Make a scan list
#' 
#' @param splitme character string with format filename:scannumber scannumber|filename: scannumber... as generated by saveScanlist()
#' @param MSData list of xcmsRaw objects. Only scans from files loaded in this object will be returned
#' 
#' @export
makeScanlist2 <- function(splitme, MSData = NULL){
  
  split1 <-  strsplit(splitme, "\\|")
  
  split2 <-  lapply(split1, strsplit, "\\:")
  
  split3 <- lapply(split2, function(x){ lapply(x, function(y){ scs <- strsplit(y[2], " ")[[1]]
  return(data.table(file = rep(y[1], length(scs)),
                    scan = as.integer(scs)))}) })
  #now consolidate into data.tables
  split4 <- lapply(split3, data.table::rbindlist)
  
  
  return(split4)
  
}