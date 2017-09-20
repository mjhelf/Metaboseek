
## recalculate original rt values from rt values as reported in featuretables after rt correction
rtadjust <- function(xset, # XCMSnExp object
                     rts){ #df, each column is expected to contain rt values
  
  if(class(xset)=="XCMSnExp"){
    if(!hasAdjustedRtime(xset)){
      message("XCMSnExp object did not contain RT correction information.")
    }
    noncorr <- rtime(xset, bySample=T, adjusted = F)
    corr <- rtime(xset, bySample=T, adjusted = T)
  }
  
  #get the embedded xcmsSet out of a CAMERA object
  if(class(xset)=="xsAnnotate"){
    xset <- xset@xcmsSet
  }
  
  if(class(xset)=="xcmsSet"){
    noncorr <- xset@rt$raw
    corr <- xset@rt$corrected
  }
  
  fx <- function(num, rtcorr, rtdiffs){
    sel <- which.min(abs(num - rtcorr))
    return(num + rtdiffs[sel])
  }
  
  
  rtdiff <- list()
  res <- list()
  for(i in c(1:length(noncorr))){
    rtdiff[[i]] <- noncorr[[i]]-corr[[i]]
    
    res[[i]] <- rts
    for(n in c(1:ncol(rts))){
      res[[i]][,n] <- sapply(rts[,n],
                             fx,
                             rtcorr = corr[[i]],
                             rtdiffs = rtdiff[[i]])
      
    }
  }
  
  if(class(xset)=="XCMSnExp"){
    names(res) <- basename(xset@processingData@files)
  }
  if(class(xset)=="xcmsSet"){
    names(res) <- basename(xset@filepaths)
    
  }
  
  
  return(res)
}