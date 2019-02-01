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
      
      data.frame(rtmin = numeric(0),
                 rtmax = numeric(0),
                 rt = numeric(0),
                 maxint = numeric(0),
                 file = character(0),
                 stringsAsFactors = F)
      
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
               file = paste(sc$file, collapse =  " "),
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
  
 pl <- do.call(rbind,do.call(find_peaks, c(list(EIC = EIC), findProps)))
 

 return(do.call(mergepeaks, c(list(pl = pl), mergeProps)))
  
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
  
 # print(aEICs[[1]])
  
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
