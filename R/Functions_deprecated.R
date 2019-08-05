#' TableUpdateChunk
#' 
#' @section NOTE:
#' This function is deprecated. Use \code{\link{updateFT}} instead!
#' 
#' Code chunk to update the active Feature Table before triggering a
#'  rerendering of the rhandsontable object.
#'  
#' Chunks are evaluated in their parent environment, and therefore require
#'  all objects they work on to be present under their correct names.
#' 
#' @details 
#' \subsection{requires}{
#' 
#' values$featureTables$Maintable - pointing to the MainTableObject
#' values$featureTables
#' }
#' 
TableUpdateChunk <- function(){
    eval.parent(quote({
        if(!is.null(values$featureTables)
           && !is.null(values$featureTables$Maintable) 
           && values$featureTables$Maintable$hasUpdates){
            if((!is.null(values$featureTables$tables[[values$featureTables$active]]$editable) 
                && values$featureTables$tables[[values$featureTables$active]]$editable)
               || is.null(values$featureTables$Maintable$liveView$comments) ){
                values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$Maintable$liveView),colnames(values$featureTables$Maintable$liveView)] <- values$featureTables$Maintable$liveView
            }else{
                values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$Maintable$liveView),"comments"] <- values$featureTables$Maintable$liveView$comments
            }
        }
        
    }))
}

#' getAllScans
#'
#' get a list of all scans in a scanlist
#' 
#' @param scanlist as returned by makeScanlist2()
#' @param MSData list of xcmsRaw objects
#' @param removeNoise if not NULL, a numeric specifying a relative intensity
#' cutoff (<1, relative to maximum peak intensity)
#' @inheritParams getScanInfo
#' 
#' @return a list of MS spectra (each in matrix format)
#'
#'
#' @export
getAllScans <- function(scanlist, MSData, removeNoise = NULL,
                        type = c("ms2", "ms1", "acquisition")){
    
    
    
    
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

#' specplot
#' 
#' Plot an MS spectrum
#' 
#' NOTE: This function is deprecated. Use \code{\link{specplot2}} instead
#' 
#' @return plots a spectrum view in the current plotting device
#' 
#' @param x mz coordinates
#' @param y intensity coordinates
#' @param norm normalize by
#' @param cx font size
#' @param k top k intensity peaks will be labeled
#' @param fileName plot title
#' @param yrange y axis range
#' @param xrange x axis range
#' @param maxi max intensity to be plotted on side
#' @param labels data.frame containing at least x and y coordinates,
#'  plus optional columns: label and color
#' @param mar margins passed to par()
#' @param ylab y axis label
#' @param ylabshift shift horizontal position of y axis label 
#'
#' @importFrom TeachingDemos spread.labs
#' @importFrom Hmisc minor.tick
#' @export
specplot <- function (x,
                      y,
                      norm=max(y)/100,
                      cx=1.5,
                      k = 10,
                      fileName = "title",
                      yrange = c(0,100),
                      xrange = range(x),
                      maxi = max(y),
                      labels = NULL,
                      mar = c(4,6,6,2),
                      ylab = "Relative Intensity (%)",
                      ylabshift = 0
){
    
    pd <- data.frame(x=x,
                     y=y/norm)
    
    if(nrow(pd)>0){
        pd$label <- format(round(pd$x,5), nsmall = 5, scientific = F)
        pd$color <- "blue"
        
    }
    
    par(mar = mar,
        xpd = FALSE, xaxs = "i", yaxs = "i")
    PlotWindow(cx, 
               ylim = yrange, 
               xlim = xrange,
               heading = fileName,
               single = T,
               par = F,
               relto = norm,
               ylab,
               xlab = "m/z",
               textadj = 1,
               ylabshift = ylabshift
    )
    
    points(pd$x,pd$y,type="h", bty="n")
    
    
    
    currview <- pd[which(pd$y <= max(yrange)
                         & pd$y >= min(yrange)
                         & pd$x <= max(xrange) 
                         & pd$x >= min(xrange)),]
    
    
    
    if (length(currview$y) >= k){
        kn <-  sort(currview$y, decreasing = T)[k]
        labs <- currview[which(currview$y>=kn),]
    }else{
        labs <- currview}
    
    
    #merge the automatically annotated peaks with labels 
    if(!is.null(labels) && nrow(labels)>0){
        labels$y <- labels$y/norm
        if(is.null(labels$color)){
            labels$color <- "red" 
        }
        
        
        if(is.null(labels$label)){
            labels$label <- format(round(labels$x,5), nsmall = 5, scientific = F)
        }
        
        
        labs <- rbind(labels, labs)
        
        labs <- labs[!duplicated(round(labs$x,5)),]
    }
    
    
    if(nrow(labs) > 0 ){
        par(xpd=NA)
        labs$xcorr <- suppressWarnings({spread.labs(labs[,1],
                                                    1.05*strwidth("A"),
                                                    maxiter=1000,
                                                    min=min(labs[,1]),
                                                    max=max(labs[,1]))})
        
        segments(labs[,1],labs[,2]+0.01*max(yrange),
                 labs$xcorr,labs[,2]+0.05*max(yrange),
                 col="olivedrab4", lwd=0.8)
        
        text(labs$xcorr,labs[,2]+0.055*max(yrange),labels=labs$label,
             col=labs$color, srt=90,adj=c(0,0.3), cex=1*cx)
        text(min(xrange), max(yrange)+1.5*strheight("M"),
             labels = format(maxi*(max(labs$y)/100),
                             scientific = T, digits =4),
             bty="n",font = 2, cex=cx*1)
        
        
        
    }
    
}


#' quickMergeMS
#'
#'
#' NOTE: This function is deprecated, use MassTools::mergeMS !
#' 
#' Merge MS spectra, best used with noise-free spectra. Will merge all peaks 
#' that are EITHER within ppm OR within mzdiff range (mzdiff typically used 
#' to allow larger ppm difference in low mz range)
#'
#' Replacement for mergeMS(..., mergeOnly = T)
#' 
#' @param speclist list of spectra (matrix with mz and intensity values)
#' @param ppm min difference between peaks in ppm
#' @param mzdiff min difference between peaks in m/z
#' @param removeNoise if not Null, will remove peaks with intensities below this fraction of maximum intensity
#' @param count if true, will count number of peaks that were merged into each 
#' peak in the resulting spectrum (will return a 3-column matrix)
#' 
#' @return a merged mass spectrum as a matrix with mz and intensity values 
#'
#' @export
quickMergeMS <- function(speclist, ppm =5, mzdiff = 0.0005,
                         removeNoise = NULL, count = F){
  
  if(length(speclist) == 0){return(NULL)}
  
  if(is.list(speclist)){
    aspec <- do.call(rbind,speclist)
  }else{
    aspec <- speclist
    
  }
  
  #lots of safeguards
  if(is.null(aspec)){return(NULL)}
  
  
  #remove 0 intensity peaks because they will produce NaN mz values downstream
  
  aspecsel <- aspec[,2] != 0
  
  if(sum(aspecsel) == 0){return(NULL)}
  
  if(sum(aspecsel) == 1){
    
    if(count){
      res_mx <- t(as.matrix(c(aspec[aspecsel,], 1)))
      colnames(res_mx) <- c("mz", "intensity", "count")
      
      return(res_mx)
      
    }else{
      return(t(as.matrix(aspec[aspecsel,])))
    }
    
  }
  else{
    aspec <- aspec[aspec[,2] != 0,]
  }
  
  
  if(is.null(aspec)){return(NULL)}
  
  
  if(nrow(aspec) != 1){
    aspec <- aspec[order(aspec[,1]),]
  }
  
  
  margins <- diff(aspec[,1])
  
  
  margins_ppm <- margins/aspec[-nrow(aspec),1]/(1e-6)
  
  groups <- c(0, cumsum(!(margins < mzdiff | margins_ppm < ppm)))
  
  
  res_mx <- sapply(unique(groups), function(g,ag,as, count){
    
    sel <- which(ag == g)
    if(count){
      
      return(c(sum(as[sel,1]*as[sel,2])/sum(as[sel,2]), mean(as[sel,2]), length(sel)))
      
    }
    return(c(sum(as[sel,1]*as[sel,2])/sum(as[sel,2]), mean(as[sel,2])))
    
  }, as = aspec, ag = groups, count)
  
  
  
  
  res_mx <- t(res_mx)
  if(count){
    if(!is.null(removeNoise) && nrow(res_mx) > 1){
      
      res_mx <- matrix(res_mx[res_mx[,2] > removeNoise*max(res_mx[,2]),], ncol = 3)
    }
    colnames(res_mx) <- c("mz", "intensity", "count")
    
    return(res_mx)
    
    
  }
  if(!is.null(removeNoise) && nrow(res_mx) > 1){
    
    res_mx <- matrix(res_mx[res_mx[,2] > removeNoise*max(res_mx[,2]),], ncol = 2)
  }
  colnames(res_mx) <- c("mz", "intensity")
  
  return(res_mx)
  
}





#' mergeMSold
#' 
#' Merge MS spectra by combining peaks that are within a ppm distance
#' 
#' NOTE: If multiple peaks inside a spectrum match another spectrum,
#'  only the one with higher(?) mz will be retained
#' 
#' @param speclist data.frame or matrix containing mz and intensity 
#' values of a spectrum (mz in column 1)
#' @param ppm accuracy
#' @param mergeOnly if TRUE, only the merged spectrum is returned
#' 
mergeMSold <- function(speclist,
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