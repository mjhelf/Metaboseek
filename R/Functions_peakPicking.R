#' find_max
#' 
#' find maxima in an EIC object using peakPick::peakpick
#' 
#' @param EIC a single EIC object (list item of multiEIC output)
#' @param getRTs return retention time values instead of indices of maxima
#' @param ... further arguments passed to peakPick::peakpick
#' 
#' @importFrom peakPick peakpick
#' 
find_max <- function(EIC, ..., getRTs = F){
  
  intens <- EIC[,"intensity"]
  
  imx <- matrix(nrow = max(sapply(intens,length)), ncol = length(intens))
  
  for(i in seq(length(intens))){
    
    imx[1:length(intens[[i]]), i] <- intens[[i]]/max(intens[[i]])
    
    
  }
  
  pks <- peakpick(imx,...)
  
  pksw <- apply(pks, 2, which)
  if(getRTs){
    return(mapply("[",EIC[,"rt"], pksw, SIMPLIFY = F))
  }
  
  return(pksw)
  
}

#' find_min
#' 
#' find minima in an EIC object using peakPick::peakpick
#' 
#' @param EIC a single EIC object (list item of multiEIC output)
#' @param getRTs return retention time values instead of indices of minima
#' @param ... further arguments passed to peakPick::peakpick
#' 
#' @importFrom peakPick peakpick
#' 
find_min <- function(EIC, ..., getRTs = F){
  
  intens <- EIC[,"intensity"]
  
  imx <- matrix(nrow = max(sapply(intens,length)), ncol = length(intens))
  
  for(i in seq(length(intens))){
    
    imx[1:length(intens[[i]]), i] <- 1-intens[[i]]/max(intens[[i]])
    
    
  }
  
  pks <- peakpick(imx,...)
  
  pksw <- apply(pks, 2, which)
  if(getRTs){
    return(mapply("[",EIC[,"rt"], pksw, SIMPLIFY = F))
  }
  
  return(pksw)
}


#' find_peaks
#' 
#' find minima in an EIC object using peakPick::peakpick
#' 
#' @param EIC a single EIC object (list item of multiEIC output)
#' @param maxdetect arguments passed to find_max
#' @param mindetect arguments passed to find_min
#' 
#' 
#' @export
find_peaks <- function(EIC, maxdetect = list(neighlim = 4, deriv.lim = 0.5, peak.min.sd = 0.7, peak.npos = 10),
                       mindetect = list(neighlim = 4, deriv.lim = 0.5, peak.min.sd = 0.7, peak.npos = 10)){
  
  maxs <- do.call(find_max, c(list(EIC = EIC), maxdetect))
  mins <- do.call(find_min, c(list(EIC = EIC), mindetect))
  
  if(length(mins) == 0 || length(maxs) == 0){
    
    return(
      list(
      data.frame(rtmin = numeric(0),
                 rtmax = numeric(0),
                 rt = numeric(0),
                 maxint = numeric(0),
                 file = character(0),
                 stringsAsFactors = F)
      
    )
    )
    
  }
  
  peaks_detected <-  mapply(function(mins, maxs, EICrow, EIC){
    
    
    res <- data.frame(rtmin = numeric(0),
                      rtmax = numeric(0),
                      rt = numeric(0),
                      maxint = numeric(0),
                      file = character(0),
                      stringsAsFactors = F)
    
    for(i in seq(length(mins)-1)){
      sel <- which( mins[i] < maxs & maxs < mins[i+1]  )
      
      if(length(sel) == 1){
        
        res[nrow(res)+1,1:4] <- c(EIC[[EICrow,"rt"]][mins[i]], EIC[[EICrow,"rt"]][mins[i+1]], EIC[[EICrow,"rt"]][maxs[sel]], EIC[[EICrow,"intensity"]][maxs[sel]])
        
      }
    }
    
    res$file <- rep(row.names(EIC)[EICrow], nrow(res))
    
    return(res)
    
  }, mins, maxs, EICrow = seq(nrow(EIC)), MoreArgs = list(EIC = EIC), SIMPLIFY = F
  
  )
  
  return(peaks_detected)
  
}


#' plotpeaks
#' 
#' plot detected peaks for each file in an EIC object
#' 
#' @param EIC a single EIC object (list item of multiEIC output)
#' @param peaklist a peaklist, such as list items returned by find_peaks() (or its rbind product)
#' 
#' 
plotpeaks <- function(EIC, peaklist){
  
  par(mfrow = c(ceiling(nrow(EIC)/5), 5))
  
  for(i in seq(nrow(EIC))){
    
    plot(EIC[[i,"rt"]], EIC[[i,"intensity"]], type = "l", xlab = ("RT (sec)"), ylab = "intensity", main = basename(row.names(EIC)[i]))
    
    selpeaks <- grep(row.names(EIC)[i], peaklist$file)
    
    if(length(selpeaks) > 0){

            cols <- Mseek.colors(n = length(selpeaks), alpha = 0.8)
      
      for (k in seq(length(selpeaks))){
        sel <- which(EIC[[i,"rt"]] >= peaklist$rtmin[selpeaks][k] 
                     &  EIC[[i,"rt"]] <= peaklist$rtmax[selpeaks][k])
        
        
        pol_x <- EIC[[i,"rt"]][sel]
        pol_y <- EIC[[i,"intensity"]][sel]
        
       
        
        
        polygon(c(pol_x[1], pol_x, pol_x[length(pol_x)]), c(min(pol_y), pol_y, min(pol_y)), col = cols[k], border = NA )
        
      }
      
      # abline(v = prts[[i]], col = "red")  
      #  abline(v = prts_min[[i]], col = "green")  
      
      
    }
  }
}

#' mergepeaks
#' 
#' merge peaks in a peaklist
#' 
#' @param pl a peaklist, such as list items returned by find_peaks() (or its rbind product)
#' @param minint minimum intensity for a peak to be retained (before merging)
#' @param minrelint minimum intensity of a peak to be retained (before merging), relative to largest peak
#' @param topN number of highest intensity peaks retained after merging
#' 
#' @export
mergepeaks <- function(pl, minint = 10000, minrelint = 0.02, topN = 3){
  
  if(nrow(pl) < 2){
    return(pl)
  }
  
  pl <- pl[pl$maxint >= minrelint*max(pl$maxint),]
  pl <- pl[pl$maxint >= minint,]
  
  if(nrow(pl) < 2){
    return(pl)
  }
  
  pl$group <- numeric(nrow(pl))
  counter <- 1
  
  for(i in seq(nrow(pl))){
    
    sel <- pl$rtmax > pl$rtmin[i]  & pl$rtmin < pl$rtmax[i]
    
    oldgroup <- max(pl$group[sel])
    
    
    if(oldgroup !=0){
      
      pl$group[i] <- oldgroup 
      
      pl$group[which(pl$group != 0 & pl$group %in% pl$group[sel])] <- oldgroup
      
    }else{
      
      pl$group[i] <- counter
      pl$group[sel] <- counter
      counter <- counter +1
      
    }
    
    
    
  }
  
  
 mergedpls <-  lapply(unique(pl$group), function(n, pl){
   
   if(is.null(pl) || nrow(pl) == 0){
     return(pl)
   } 
   
    sc <- pl[pl$group == n,]
    
    data.frame(rtmin = sum(sc$rtmin*sc$maxint)/ sum(sc$maxint),
               rtmax = sum(sc$rtmax*sc$maxint)/ sum(sc$maxint),
               rt = sum(sc$rt*sc$maxint)/ sum(sc$maxint),
               maxint = max(sc$maxint),
               file = paste(basename(sc$file), collapse =  "|"),
               stringsAsFactors = F)
    
  }, pl)
 
 mergedpls <- do.call(rbind, mergedpls)
 
 return(mergedpls[na.omit(order(mergedpls$maxint, decreasing = T)[1:topN]), ])
  
  
  
  
  
}

#' getpeaks
#' 
#' find and then merge peaks for an EIC object. wrapper function for find_peaks and mergepeaks
#' 
#' @param EIC a single EIC object (list item of multiEIC output), or a list of EIC items (multiEIC output)
#' @param findProps arguments passed to find_peaks
#' @param mergeProps arguments passed to mergepeaks
#' 
#' 
#' @export
getpeaks <- function(EIC, findProps = list(maxdetect = list(neighlim = 4, deriv.lim = 0.5, peak.min.sd = 0.7, peak.npos = 10),
                                           mindetect = list(neighlim = 4, deriv.lim = 0.5, peak.min.sd = 0.7, peak.npos = 10)),
                          mergeProps = list(minint = 10000, minrelint = 0.02, topN = 3)){
  
  if(is.matrix(EIC)){
  
    prepl <- do.call(find_peaks, c(list(EIC = EIC), findProps))

 pl <- do.call(rbind,prepl)

 outp <- do.call(mergepeaks, c(list(pl = pl), mergeProps))

 return(outp)
  
  }else{
    
    return(lapply(EIC, getpeaks, findProps, mergeProps))
    
  }
 
}

#' makeRTlist
#' 
#' find and then merge peaks, given a set of mz values in a dataframe and a set of xcmsRaw objects
#' 
#' @param df a data.frame that contains a mz column with mz values of interest
#' @param rawdata a list of xcmsRaw objects
#' @param ppm ppm tolerance for EIC construction
#' @param retainColumns keep and copy these columns from the original df when making the result data.frame
#' 
#' @export
makeRTlist <- function(df, rawdata, ppm = 5, retainColumns = NULL){
  
  aEICs <- multiEIC(rawdata,
                    mz = data.frame(mzmin = df$mz-ppm*1e-6*df$mz, mzmax= df$mz+ppm*1e-6*df$mz),
                    rt = NULL,
                    rnames = row.names(df),
                    byFile = F,#if true, table will be sorted by rawfile, otherwise by feature
                    XIC = F,
                    getgauss = F,
                    RTcorr = NULL
  )
  

  allpeaks <- getpeaks(aEICs)


return(lapply(seq(nrow(df)), function(i, df, allpeaks, retainColumns, ppm){
    
    if(nrow(allpeaks[[i]]) > 0){
    
      pt <- allpeaks[[i]]
      
      pt <- pt[,colnames(pt) != "group"]
      
      pt$mz <- df$mz[i]
      pt$mzmin <- df$mz[i]-ppm*1e-6*df$mz[i]
      pt$mzmax <- df$mz[i]+ppm*1e-6*df$mz[i]
      
    
    if(length(retainColumns) == 0 || !any(retainColumns %in% colnames(df))){
    return(pt)
    }else{
      
      #fix issue when only a single column is selected
      if(length(retainColumns) == 1){
        
        pt[[retainColumns]] <- df[[retainColumns]][i]
        return(pt)
      }
      
      tdf <- df[rep(i, nrow(allpeaks[[i]])),retainColumns]
      
      return(updateDF(pt, tdf))
      
    }
    }
    else{
      return(NULL)
    }
    
    
  }, df, allpeaks, retainColumns, ppm))
  
  
}


#' peakDetect
#'
#' more recent try at peak detection
#'
#' @param trace numeric vector of intensity values. it is assumed that the data points were collected in regular intervals (problems may occur e.g. with variable scan rate due to ddMS2 experiments)
#' @param SN factor by which the maximum of a peak has to be larger than its edges (start  and  end scan) in final filter step
#' @param minwidth minimum width of a peak to not be thrown out in initial filter step
#' @param localNoise window size for local noise level calculation - default 7 means data from each point plus/minus 3 scans is used
#' @param localNoiseFactor  factor to adjust local noise level. can be negative to allow very small peaks
#' @param globalNoiseFactor factor to adjust global noise level (based on entire trace). can be negative to allow very small peaks
#' @param extend if true, will try to expand peak boundaries to include downstream tails that may have been cut of before. Aslo depends on local noise settings.
#'
#' @export
peakDetect  <- function(trace,
                        SN = 1,
                        minwidth = 4,
                        localNoise = 7,
                        localNoiseFactor = 0.5,
                        globalNoiseFactor = 0.5,
                        extend = T
){
  
  slope <- c(NA,diff(trace))
  
  #Find maxima
  max <- which(slope >= 0 & c(na.omit(slope),NA) < 0)
  
  #find minima at end of peaks
  min_end <- which(slope < 0 & c(na.omit(slope),NA) >= 0)
  
  #find minima at beginning of peaks
  min_start <- which(slope <= 0 & c(na.omit(slope),NA) > 0)
  
  if(length(max) == 0 | length(min_end) == 0 | length(min_start) == 0){return(data.frame(max = numeric(0), start = numeric(0), end = numeric(0)))}
  
  #summary of all found peaks
  allpeaks <- matrix(unlist(lapply(seq_along(max), function(n){
    
    
    start <- min_start[which(min_start < max[n] & if(n >1){min_start > max[n-1]}else{TRUE})]
    
    end <- min_end[which(min_end > max[n] & if(n < length(max) ){min_end < max[n+1]}else{TRUE})]
    
    if(n == length(max) && length(end) == 0) {end <- length(trace)}
    
    if(length(start) == 0 | length(end) == 0){return(NULL)}
    
    return(c(max[n], start[1], end[1]))
    
    
  })), ncol = 3,byrow = T,  dimnames = list(NULL, c("max","start","end")))
  
  #get peak intensities at begining, end and maximum
  allpeakints <- matrix(trace[allpeaks], ncol = 3,byrow = F,  dimnames = list(NULL, c("max","start","end")))
  
  
  globalnoise <- (max(trace)+mean(trace))/100 + globalNoiseFactor*(sum(abs(trace-mean(trace)))/length(trace))
  # print((max(trace)+mean(trace))/100)
  # print(globalNoiseFactor*(sum(abs(trace-mean(trace)))/length(trace)))
  
  #define local noise levels
  #noise1 <- runmed(trace, localNoise)*localNoiseSN
  
  # noise1 <- sapply(seq(length(allpeaks[,"max"])), function(n){
  #    
  #    localtrace <- trace[na.omit(seq(max(allpeaks[n,"start"]- localNoise, 1),     allpeaks[n,"end"]+ localNoise))]
  # 
  #    
  #    return((max(localtrace)+mean(localtrace))/2 + localNoiseSN*(sum(abs(localtrace-mean(localtrace)))/length(localtrace)))
  #    
  #  })
  
  noise1 <- sapply(seq(length(trace)), function(n){
    
    localtrace <- na.omit(trace[seq(max(n - localNoise, 1),  n + localNoise)])
    
    
    return((max(localtrace)+mean(localtrace))/2 + localNoiseFactor*(sum(abs(localtrace-mean(localtrace)))/length(localtrace)))
    
  })
  
  #filter by local noise levels
  allpeaknoises <- matrix(noise1[allpeaks], ncol = 3,byrow = F,  dimnames = list(NULL, c("max","start","end")))
  filvec <- allpeakints[,"max"] > globalnoise & allpeakints[,"max"] >  allpeaknoises[,"max"] #noise1
  
  filpeaks <- allpeaks[filvec,, drop = F]
  filpeakints <- allpeakints[filvec,, drop = F]
  filpeaknoises <- allpeaks[filvec,, drop = F]
  
  if(nrow(filpeaks) == 0){return(as.data.frame(filpeaks))}
  
  if(nrow(filpeaks)>1){
    
    #find peaks that are adjacent
    checkmerge <- !c(TRUE, filpeaks[seq(1,nrow(filpeaks)-1),"end"] != filpeaks[seq(2,nrow(filpeaks)),"start"])
    
    
    #check if peaks really should be merged with the previous adjacent peak
    for(i in which(checkmerge)){
      
      checkme <- filpeakints[i,"start"]
      
      #if the peak start intensity is lower than a third of either of the surrounding peaks, don't merge it with previous peak
      if(checkme < min(filpeakints[i-1,"max"]/3,filpeakints[i,"max"]/3)){
        checkmerge[i] <- FALSE
      }
      
    }
  }else{
    checkmerge <- FALSE
  }
  #group peaks that will be merged
  groups <- cumsum(!checkmerge)
  
  #merge peaks by group
  mergedpeaks <- matrix(unlist(lapply(unique(groups),function(g){
    
    item <- filpeaks[groups == g,, drop = F]
    ints <- filpeakints[groups == g,, drop = F]
    
    c(item[which.max(ints[,"max"]),"max"], min(item[,"start"]), max(item[,"end"]), nrow(item))
    
  })), ncol = 4, byrow = T,  dimnames = list(NULL, c("max","start","end", "npeaks")))
  
  #addtional filtering
  # print((mergedpeaks))
  # min. peak width filter
  goodpeaks <- mergedpeaks[mergedpeaks[,"end"] - mergedpeaks[,"start"] >= minwidth,, drop = F]
  if(nrow(goodpeaks) <1 ){
    return(as.data.frame(goodpeaks))
  }
  # remove peaks that where the maximum does not stand out enough over the intensities in entire peak width:
  thresh <- sapply(seq(nrow(goodpeaks)),function(n){mean(trace[seq(goodpeaks[n,"start"],goodpeaks[n,"end"])])})
  
  goodpeaks <- as.data.frame(goodpeaks[trace[goodpeaks[,"max"]] > SN*thresh,, drop = F])
  
  goodpeaks$maxint <- trace[goodpeaks$max]
  
  if(extend & nrow(goodpeaks) > 0){
    localminimum <- sapply(seq(length(trace)), function(n){
      
      localtrace <- na.omit(trace[seq(max(n - localNoise, 1),  n + localNoise)])
      
      
      return(min(localtrace))
      
    })
    #now, extend the good peaks on their tails:
    stops <- which(trace < noise1/20 | seq_along(trace) %in% goodpeaks$start | trace <= localminimum) 
    if(length(stops)>0){
    goodpeaks$end <- sapply(goodpeaks$end, function(ends){
      
      sel <- stops >= ends
      if(!any(sel)){return(ends)}
      return(min(stops[sel]))
      })
    }
  }
  
  return(goodpeaks)
}

#' findPeaks2
#'
#' Find peaks in a multiEIC output list item
#' 
#' @param EIC an EIC output item
#' @param ... arguments passed to peakDetect()
#'
#' @export
find_peaks2 <- function(EIC, ...){
  
  if(!is.matrix(EIC)){
    warning("EIC input in wrong format")
    return(NULL)}
  
  pl <- lapply(row.names(EIC), function(fname){
    tr <- EIC[fname,]
    
    dtp <- peakDetect(trace = tr$intensity,
                      ... )
    
    
    dtp$rt <- tr$rt[dtp$max]
    dtp$rtmin <- tr$rt[dtp$start]
    dtp$rtmax <- tr$rt[dtp$end]
    dtp$file <- if(nrow(dtp)<1){character(0)}else{basename(fname)} 
    
    return(dtp)
  })
  return(do.call(rbind,pl))
  
}

#' mergepeaks2
#' 
#' merge peaks in a peaklist
#' 
#' @param pl a peaklist, such as list items returned by find_peaks() (or its rbind product)
#' @param rttol retention time tolerance in seconds to merge peaks
#' @param minint minimum intensity for a peak to be retained (before merging)
#' @param minrelint minimum intensity of a peak to be retained (before merging), relative to largest peak
#' @param topN number of highest intensity peaks retained after merging
#' 
#' @export
mergepeaks2 <- function(pl,
                        rttol = 3,
                        minint = 0,
                        minrelint = 0,
                        topN = 100){
  
  if(nrow(pl) < 2){
    return(pl)
  }
  
  pl <- pl[pl$maxint >= minrelint*max(pl$maxint),]
  pl <- pl[pl$maxint >= minint,]
  
  if(nrow(pl) < 2){
    return(pl)
  }
  
  if(is.null(pl$file) | is.null(pl$rt)){warning("wrong peak list format (file or rt columns missing)!")}
  
  
  #group peaks together if their retention time is within a tolerance AND they come from different files
  pl <- pl[order(pl$rt),]
  #note that these are the conditions under which a peak is NOT added to the same group as the previous one, and a new group starts
  #EITHER: rt difference is too large
  #OR: peaks are from the same file (these should be merged already in previous step
  #OR: rt difference of start and end of peak are both too large  | (c(TRUE, diff(pl$rtmin) > rttol) &  c(TRUE, diff(pl$rtmax) > rttol))
  
  pl$group <- cumsum((c(TRUE, diff(pl$rt) > rttol) | pl$file == c("",pl$file[seq(length(pl$file)-1)])   ))
  
  #double check no 2 peaks from the same file end up in the same group:
  checkdoubles <- c(F,sapply(seq(2,length(pl$group)), function(n){
    # print(pl$group[1:(n-1)] == pl$group[n] & pl$file[1:(n-1)] == pl$file[n])
    any(pl$group[1:(n-1)] == pl$group[n] & pl$file[1:(n-1)] == pl$file[n])
  }))
  
  while(any(checkdoubles)){
    
    pl$group <- pl$group + cumsum(checkdoubles)
    
    checkdoubles <- c(F,sapply(seq(2,length(pl$group)), function(n){
      
      any(pl$group[1:(n-1)] == pl$group[n] & pl$file[1:(n-1)] == pl$file[n])
      
      }))
    
  }
  
  
  mergedpls <-  lapply(unique(pl$group), function(n, pl){
    
    if(is.null(pl) || nrow(pl) == 0){
      return(pl)
    } 
    
    sc <- pl[pl$group == n,]
    
    data.frame(rtmin = sum(sc$rtmin*sc$maxint)/ sum(sc$maxint),
               rtmax = sum(sc$rtmax*sc$maxint)/ sum(sc$maxint),
               rt = sum(sc$rt*sc$maxint)/ sum(sc$maxint),
               maxint = max(sc$maxint),
               file = paste(unique(basename(sc$file)), collapse =  "|"),
               stringsAsFactors = F)
    
  }, pl)
  
  mergedpls <- do.call(rbind, mergedpls)
  
  return(mergedpls[na.omit(order(mergedpls$maxint, decreasing = T)[1:topN]), ])
  
}


#' getpeaks2
#' 
#' find and then merge peaks for an EIC object. wrapper function for find_peaks and mergepeaks
#' 
#' @param EIC a single EIC object (list item of multiEIC output), or a list of EIC items (multiEIC output)
#' @param findProps arguments passed to find_peaks
#' @param mergeProps arguments passed to mergepeaks
#' 
#' 
#' @export
getpeaks2 <- function(EIC, findProps = list(SN = 1,
                                            minwidth = 4,
                                            localNoise = 7,
                                            localNoiseFactor = 0.5,
                                            globalNoiseFactor = 0.5,
                                            extend = T),
                      mergeProps = list(rttol = 3, minint = 0, minrelint = 0, topN = 100)){
  
  if(is.matrix(EIC)){
    
    outp <- do.call(find_peaks2, c(list(EIC = EIC), findProps))
    #print(outp)
    return(do.call(mergepeaks2, c(list(pl = outp),mergeProps)))
    
  }else{
    
    return(lapply(EIC, getpeaks2, findProps, mergeProps))
    
  }
  
}

#' makeRTlist2
#' 
#' find and then merge peaks, given a set of mz values in a dataframe and a set of xcmsRaw objects
#' 
#' @param df a data.frame that contains a mz column with mz values of interest
#' @param rawdata a list of xcmsRaw objects
#' @param ppm ppm tolerance for EIC construction
#' @param retainColumns keep and copy these columns from the original df when making the result data.frame
#' @param ... arguments passed to getpeaks2()
#' 
#' @export
makeRTlist2 <- function(df, rawdata, ppm = 5, retainColumns = NULL, ...){
  
  aEICs <- multiEIC(rawdata,
                    mz = data.frame(mzmin = df$mz-ppm*1e-6*df$mz, mzmax= df$mz+ppm*1e-6*df$mz),
                    rt = NULL,
                    rnames = row.names(df),
                    byFile = F,#if true, table will be sorted by rawfile, otherwise by feature
                    XIC = F,
                    getgauss = F,
                    RTcorr = NULL
  )
  
  
  allpeaks <- getpeaks2(aEICs, ...)
  
  
  return(do.call(rbind,lapply(seq(nrow(df)), function(i, df, allpeaks, retainColumns, ppm){
    
    if(nrow(allpeaks[[i]]) > 0){
      
      pt <- allpeaks[[i]]
      
      pt <- pt[,colnames(pt) != "group"]
      
      pt$mz <- df$mz[i]
      pt$mzmin <- df$mz[i]-ppm*1e-6*df$mz[i]
      pt$mzmax <- df$mz[i]+ppm*1e-6*df$mz[i]
      
      
      if(length(retainColumns) == 0 || !any(retainColumns %in% colnames(df))){
        return(pt)
      }else{
        
        #fix issue when only a single column is selected
        if(length(retainColumns) == 1){
          
          pt[[retainColumns]] <- df[[retainColumns]][i]
          return(pt)
        }
        
        tdf <- df[rep(i, nrow(allpeaks[[i]])),retainColumns]
        
        return(updateDF(pt, tdf))
        
      }
    }
    else{
      return(NULL)
    }
    
    
  }, df, allpeaks, retainColumns, ppm)))
  
  
}