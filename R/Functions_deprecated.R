#' mergeMSold
#' 
#' Merge MS spectra by combining peaks that are within a ppm distance
#' 
#' NOTE: If multiple peaks inside a spectrum match another spectrum, only the one with higher(?) mz will be retained
#' 
#' @param speclist data.frame or matrix containing mz and intensity values of a spectrum (mz in column 1)
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
