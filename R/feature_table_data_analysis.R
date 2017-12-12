#testtable <- data.frame(s1 = c(0, rnorm(10, mean = 1000000, sd = 100000)), s2 = c(rnorm(10, mean = 1000000, sd = 100000), 10), s3 = c(rnorm(7, mean = 1000000, sd = 100000), 0,0,0,0))
#mx <- as.matrix(testtable)
#featureTableNormalize(mx)

library(Biobase)

#' featureTableNormalize
#' 
#' Function to normalize data in a matrix.
#' 
#' 
#' @param mx a matrix of numeric (intensity) values
#' @param raiseZeros if not NULL, values of 0 will be raised to a level defined by a character string. "min" will raise all zeros to the lowest non-zero value in mx.
#' @param log if not NULL, log10 will be applied to values in mx
#' @param normalize if not NULL, column values will be normalized by column averages
#' @param threshold numeric(1). 
#' @param thresholdMethod if not NULL, removes all rows in mx in which no value is above threshold
#' 
#' @export
featureTableNormalize <- function (mx,
                      raiseZeros = NULL,#"min",
                      log = NULL,#"log10",
                      normalize = NULL,#"columnMean",
                      threshold = NULL,#0.1,
                      thresholdMethod = NULL#"rowMax"
                      ){
    
    if(!is.null(mx)){
    
    if(!is.null(raiseZeros)){
        #add switch for dirrerent options later
        mx[which(mx==0, arr.ind=T)]  <- raiseZeros
    }
    
    if(!is.null(log)){
        mx <- log10(mx)
    }
    
    if(!is.null(normalize)){
        #calculate correction factor for each column
        mxmeans <- colMeans(mx)
        mxmeans <- mxmeans/mean(mxmeans)
        #apply it
        for (i in 1:ncol(mx)){
        mx[,i] <- mx[,i]/mxmeans[i]} 
    }
    
    if(!is.null(threshold) & !is.null(thresholdMethod)){
        mx <- mx[which(Biobase::rowMax(mx)>threshold),]
    }
    return(mx)
    
    }
}


#' featureCalcs
#' 
#' 
#' Calculate simple parameters from feature table data.frame
#' 
#' @param df a feature table data.frame with a column "mz"
#' @param massdef if true, calculate the mass defect for each entry in df from their mz values
#' 
#' @export
featureCalcs <- function(df,
                         massdef = T# calculate mass defect for each feature
){
  if(massdef){
    df$massdefppm <- ((df$mz-floor(df$mz))/df$mz)*1e6
  
    return(data.frame(massdefppm = df$massdefppm))
  }

    }




#' foldChange
#' 
#' 
#' calculate fold changes between grouped columns of a matrix
#' 
#' @param mx a matrix of numeric (intensity) values
#' @param groups named list of intensity columns listed by group (as supplied by $anagroupnames or $anagroupnames_norm of MosaicFT objects)
#' @param ctrl character() naming the control group(s)
#' @param calc currently has to be "mean", compare rowMeans of one group vs. rowMeans of other groups (and optionally rowMeans of controls only)
#' @param topgroup if TRUE, return group with highest intensity for each feature
#' @param maxFold if TRUE, make a column with maximum fold change between any two groups for each feature
#' @param foldMaxK if not NULL, make column with fold change of highest group value over foldMaxK largest group value.
#' @param foldmode if "complex", gives ratios between all groups
#'  
#' @export
foldChange <- function(mx,
                       groups, #
                       ctrl = NULL, #control group
                       calc = "mean", #use  rowMeans (vs. rowMean), rowMedians (vs. row), or rowMax to calculate upingroup and maxFold
                       topgroup = T, #return group with highest intensity for each feature
                       maxFold = T, #make a column with maximum fold change between any two groups for each feature
                       foldMaxK = 2, #make column with fold change of Max over kth largest
                       foldmode = "simple" #or "complex" (which gives ratios between all groups)
                       ){
    
    out <- data.frame(pholder=integer(nrow(mx)))   
    out$maxint <- if(ncol(mx)==1){mx}else{rowMax(mx)}
    if(calc == "mean"){
        
        #make rowMeans for each group
         rme <- function(cols,mx){return(if(length(cols)==1){mx[,cols]}else{rowMeans(mx[,cols])})}
         rmeans <- sapply(groups,rme, mx)
         rmax <- function(cols,mx){return(if(length(cols)==1){mx[,cols]}else{rowMax(mx[,cols])})}
         rmaxes <- sapply(groups,rmax, mx)
         rmin <- function(cols,mx){return(if(length(cols)==1){mx[,cols]}else{rowMin(mx[,cols])})}
         rmins <- sapply(groups,rmin, mx)
         
        out$topgroup <- colnames(rmeans)[apply(rmeans,1,which.max)]
        out$maxfold <- rowMax(rmeans)/rowMin(rmeans)
     
        if(!is.null(foldMaxK)){
        k <- if(foldMaxK >= ncol(rmeans)){1}else{ncol(rmeans) - foldMaxK + 1}
        out[[paste0("maxfoldover",foldMaxK)]] <- rowMax(rmeans)/rowQ(rmeans,k)
        }
        
        #make columns for each group, fold over all the other groups, and fold over ctrl if ctrl is defined
        
        for(i in colnames(rmeans)){
            
            #remove everything after double underscore (new default notation)
            #remove the XIC tag if it has only one underscore (old notation)
            #remove trailing underscores if any
            
            barename <- gsub("_$","",gsub("_XIC","",gsub("__(.*)","",i)))
            
            out[[paste0(barename,"__meanInt")]] <- rmeans[,i]
            if(!is.null(foldmode) && foldmode=="complex"){
              out[[paste0(barename,"__foldOver_")]] <- rmeans[,i]/rmeans[,which(colnames(rmeans)!=i)]
              out[[paste0(barename,"__minFoldOver_")]] <- rmins[,i]/rmaxes[,which(colnames(rmaxes)!=i)]
            }else{
              out[[paste0(barename,"__foldOverRest")]] <- if(length(colnames(rmeans)) >2){
                unname(rowMeans(rmeans[,i]/rmeans[,which(colnames(rmeans)!=i)]))
              }else{
                unname(rmeans[,i]/rmeans[,which(colnames(rmeans)!=i)])
              }
              out[[paste0(barename,"__minFold")]] <- if(length(colnames(rmeans)) >2){
                rowMin(rmins[,i]/rmaxes[,which(colnames(rmaxes)!=i)])  
              }else{
                rmins[,i]/rmaxes[,which(colnames(rmaxes)!=i)]
              }
              out[[paste0(barename,"__minFoldMean")]] <- if(length(colnames(rmeans)) >2){
                rowMin(rmeans[,i]/rmaxes[,which(colnames(rmeans)!=i)])  
              }else{
                rmeans[,i]/rmaxes[,which(colnames(rmeans)!=i)]
              }
            }
            
            if(!is.null(ctrl)){
            out[[paste0(barename,"__foldOverCtrl")]] <- rmeans[,i]/rmeans[,ctrl]
            out[[paste0(barename,"__minFoldOverCtrl")]] <- rmins[,i]/rmaxes[,ctrl]
            }
            }
    }
    
    
    if(is.null(foldmode) || !foldmode=="complex"){
      barenames <- gsub("_$","",gsub("_XIC","",gsub("__(.*)","",colnames(rmeans))))
      minFoldCols <- paste0(barenames,"__minFold")
      minFoldMeansCols <- paste0(barenames,"__minFoldMean")
      minFoldCtrlCols <- paste0(barenames,"__minFoldOverCtrl")
      
      out$best_minFold <- rowMax(as.matrix(out[,minFoldCols]))
      out$best_minFoldMean <- rowMax(as.matrix(out[,minFoldMeansCols]))
      out$best_minFoldCtrl <- rowMax(as.matrix(out[,minFoldCtrlCols]))
      
      }
    
    return(out[,which(colnames(out)!="pholder")])
        
       
    }
    


#' multittest
#' 
#' 
#' Calculate per-row p-values between grouped columns of a data.frame.
#' Note that for column groups with less than 2 members, a pseudo-ttest will be calculated via Mosaic::ttestx to avoid throwing errors.
#' 
#' @param df a data.frame with numeric (intensity) values
#' @param groups named list of intensity columns listed by group (as supplied by $anagroupnames or $anagroupnames_norm of MosaicFT objects)
#' @param ttest if TRUE, ttest will be calculated
#' @param adjmethod method to adjust p values (passed on to stats::p.adjust)
#'  
#' @export
multittest <- function (df = as.data.frame(mx),
                    groups,
                    ttest=T,
                    adjmethod='bonferroni'){
   # withProgress(message = 'Please wait!', detail = "calculating pvalues", value = 0.03,{

    out = data.frame(pholder= numeric(nrow(df)))
    #calculate parameters for each group
    for (n in c(1:length(groups))){
        noni <- unlist(groups)[which(!unlist(groups) %in% groups[[n]])]
        i <- groups[[n]]
        
       # meanint <- rowMeans(as.matrix(df[,i]))
        #maxint <- rowMax(as.matrix(df[,i]))
        #fold2 <- rowMeans(as.matrix(df[,i]))/rowMeans(as.matrix(df[,noni]))
        sdev <- sapply(as.list(data.frame(t((df[,i])))),sd)/if(length(i)==1){df[,i]}else{rowMeans(as.matrix(df[,i]))}
       # sdev2 <- sapply(split(df[,i],seq(nrow(df))),sd)/rowMeans(as.matrix(df[,i]))
        sdev[which(is.na(sdev))] <-0 #if there is only one data point in a line, sd returns 0
        
        if(ttest){if (min(length(i),length(noni)) > 1){
            pval <- mttest(x=df[,i],y=df[,noni])}
            else{ pval <- sapply(as.list(data.frame(t((df[,c(i,noni)])))),
                                 ttestx,calc=1,over=c(2:(length(i)+length(noni))))
            pval[which(pval>0.5)] <- 1-pval[which(pval>0.5)]}
            padj <- p.adjust(pval, method = adjmethod)}
        
        
        #options(scipen = 100, digits = 4)
        
        out[[paste0(names(groups)[n],"__sdev")]] <- sdev
        
        #options(scipen = -100, digits = 4)
        out[[paste0(names(groups)[n],"__pval")]] <- pval
        out[[paste0(names(groups)[n],"__pval_adj")]] <- padj
        print(n)
    }
    

    return(out[,which(colnames(out) !="pholder")])
 #   })
}



#' mttest
#' 
#' helper function for multittest, calculating the p.value between two numeric vectors
#'  
#' @param x numeric vector
#' @param y numeric vector
#'  
#' @export
mttest <- function (x,y){
    
    if(nrow(x)==1){
        xl <- x
        yl <- y
        return(sttest(x=xl,y=yl)[[3]])
    }else{
    
    xl <- as.list(data.frame(t((x))))
    yl <- as.list(data.frame(t((y))))
    
    listo <- mapply(sttest,x=xl, 
                    y=yl, SIMPLIFY = F)
    
    return(sapply(listo,"[[",3))}}

#' sttest
#' 
#' helper function for mttest, calculating the p.value between two numeric vectors
#'  
#' @param out what to return if there is an error in the call to t.test, defaults to NA
#' @param ... arguments passed on to stats::t.test
#'  
#' @export
sttest <- function(out=NA,...){
    res <-try(t.test(...), silent = T)
    if(is(res,"try-error")){return(out)}else{return(res)}
}

#' ttestx
#' 
#' Upper or Lower Tail Test of Population Mean with Unknown Variance
#'  
#' @param x numeric vector
#' @param ltail TRUE or FALSE, passed on as lower.tail to stats::pt
#' @param over indexes of values in x to be used as population
#' @param calc indexes of values in x representing the hypothesized upper bond of population mean
#'  
#' @export
ttestx <- function(x=c(1,2,2,3,3,3,4,4,4,4,4,5,5,5,6,6,7) #input vector
                   , ltail=T,
                   over= NULL,
                   calc = NULL){
    #http://www.r-tutor.com/elementary-statistics/hypothesis-testing/upper-tail-test-population-mean-unknown-variance
    
    if (is.null(over)){over <- c(1:length(x))} 
    if (is.null(calc)){calc <- c(1:length(x))} 
    
    
    m <- mean(x[over])
    s <-sd(x[over])
    n <- length(x[over])
    
    #test statistic
    fx <- function(x) (m-x)/(s/sqrt(n))
    t <- sapply(x[calc], fx )
    
    #ttest
    fy <- function(x) pt(x, df=n-1, lower.tail = ltail)
    pvals <- sapply(t, fy)
    
    return(pvals)
    
}

#' MosCluster
#' 
#' Cluster a matrix
#'  
#' @param mx matrix (of intensity values)
#' @param method "clara" (from the cluster package)
#' @param ... additional arguments to the clustering method
#'
#' @importFrom cluster clara
#' 
#' @export
MosCluster <- function(method = "clara",
                       ...){
  #requireNamespace("cluster")
  res <- do.call(paste0(method), list(...))
  
  return(data.frame(cluster__clara = res$clustering))
  
}    
