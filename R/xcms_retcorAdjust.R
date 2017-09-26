#' rtexport
#' 
#' Extract corrected and uncorrected retention times for each scan from an xcms::XCMSnExp, xcms::xcmsSet or xcms::xsAnnotate object
#' 
#' @param xset an xcms::XCMSnExp, xcms::xcmsSet or xcms::xsAnnotate object
#' 
#' @export

rtexport <- function(xset){
if(class(xset)=="XCMSnExp"){
  if(!hasAdjustedRtime(xset)){
    message("XCMSnExp object did not contain RT correction information.")
  }
  return(
    list(
      noncorr = rtime(xset, bySample=T, adjusted = F),
  corr = rtime(xset, bySample=T, adjusted = T),
  fnames = xset@processingData@files
      
    )
    )
  
}

#get the embedded xcmsSet out of a CAMERA object
if(class(xset)=="xsAnnotate"){
  xset <- xset@xcmsSet
}

if(class(xset)=="xcmsSet"){
  
  return(
    list(
  noncorr = xset@rt$raw,
  corr = xset@rt$corrected,
  fnames = xset@filepaths
    )
  )
}

  warning("something went wrong - rtexport did not return anything - was the input an XCMSnExp, xcmsSet or xsAnnotate object?")
  
}

#' rtadjust
#' 
#' recalculate original rt values from rt values as reported in featuretables after rt correction for each file
#' 
#' @param rtexportObj rtexport output object
#' @param rts data.frame with rt values 
#' 
#' @export
rtadjust <- function(rtexportObj, # rtexport output object
                     rts){ #df, each column is expected to contain rt values
  
  
   fx <- function(num, rtcorr, rtdiffs){
    sel <- which.min(abs(num - rtcorr))
    return(num + rtdiffs[sel])
  }
  
  
  rtdiff <- list()
  res <- list()
  for(i in c(1:length(rtexportObj$noncorr))){
    rtdiff[[i]] <- rtexportObj$noncorr[[i]]-rtexportObj$corr[[i]]
    
    res[[i]] <- rts
    for(n in c(1:ncol(rts))){
      res[[i]][,n] <- sapply(rts[,n],
                             fx,
                             rtcorr = rtexportObj$corr[[i]],
                             rtdiffs = rtdiff[[i]])
      
    }
  }
  

    names(res) <- basename(rtexportObj$fnames)

  return(res)
}