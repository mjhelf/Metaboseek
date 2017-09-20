#testtable <- data.frame(s1 = c(0, rnorm(10, mean = 1000000, sd = 100000)), s2 = c(rnorm(10, mean = 1000000, sd = 100000), 10), s3 = c(rnorm(7, mean = 1000000, sd = 100000), 0,0,0,0))
#mx <- as.matrix(testtable)
#featureTableNormalize(mx)

library(Biobase)


    
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

featureCalcs <- function(df,
                         massdef = T# calculate mass defect for each feature
){
    df$massdefppm <- ((df$mz-floor(df$mz))/df$mz)*1e6
    return(data.frame(massdefppm = df$massdefppm))
}

foldChange <- function(mx,
                       groups, #intensity columns listed by group
                       ctrl = NULL, #control group
                       calc = "mean", #use  rowMeans (vs. rowMean), rowMedians (vs. row), or rowMax to calculate upingroup and maxFold
                       topgroup = T, #return group with highest intensity for each feature
                       maxFold = T, #make a column with maximum fold change between any two groups for each feature
                       foldMaxK = 2, #make column with fold change of Max over kth largest
                       foldmode = "simple", #or "complex" (which gives ratios between all groups)
                       massdef = T #calculate mass defect
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
     
        
        k <- if(foldMaxK >= ncol(rmeans)){1}else{ncol(rmeans) - foldMaxK + 1}
        out[[paste0("maxfoldover",foldMaxK)]] <- rowMax(rmeans)/rowQ(rmeans,k)
        
        #make columns for each group, fold over all the other groups, and fold over ctrl if ctrl is defined
        
        for(i in colnames(rmeans)){
            
            #remove everything after double underscore (new default notation)
            #remove the XIC tag if it has only one underscore (old notation)
            #remove trailing underscores if any
            
            barename <- gsub("_$","",gsub("_XIC","",gsub("__(.*)","",i)))
            
            out[[paste0(barename,"__meanInt")]] <- rmeans[,i]
            if(foldmode=="complex"){
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
            }
            
            if(!is.null(ctrl)){
            out[[paste0(barename,"__foldOverCtrl")]] <- rmeans[,i]/rmeans[,ctrl]
            out[[paste0(barename,"__minFoldOverCtrl")]] <- rmins[,i]/rmaxes[,ctrl]
            }
            }
    }
    return(out[,which(colnames(out)!="pholder")])
        
       
    }
    
multittest <- function (pl = as.data.frame(mx),
                    groups,
                    ttest=T,
                    adjmethod='bonferroni',
                    fold =10){
   # withProgress(message = 'Please wait!', detail = "calculating pvalues", value = 0.03,{

    out = data.frame(pholder= numeric(nrow(pl)))
    #calculate parameters for each group
    for (n in c(1:length(groups))){
        noni <- unlist(groups)[which(!unlist(groups) %in% groups[[n]])]
        i <- groups[[n]]
        
       # meanint <- rowMeans(as.matrix(pl[,i]))
        #maxint <- rowMax(as.matrix(pl[,i]))
        #fold2 <- rowMeans(as.matrix(pl[,i]))/rowMeans(as.matrix(pl[,noni]))
        sdev <- sapply(as.list(data.frame(t((pl[,i])))),sd)/if(length(i)==1){pl[,i]}else{rowMeans(as.matrix(pl[,i]))}
       # sdev2 <- sapply(split(pl[,i],seq(nrow(pl))),sd)/rowMeans(as.matrix(pl[,i]))
        sdev[which(is.na(sdev))] <-0 #if there is only one data point in a line, sd returns 0
        
        if(ttest){if (min(length(i),length(noni)) > 1){
            pval <- mttest(x=pl[,i],y=pl[,noni])}
            else{ pval <- sapply(as.list(data.frame(t((pl[,c(i,noni)])))),
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
    
   # pl$massdefppm <- ((pl$mz-floor(pl$mz))/pl$mz)*1e6
    
    return(out[,which(colnames(out) !="pholder")])
 #   })
}

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

##failsafe ttest
sttest <- function(out=NA,...){
    res <-try(t.test(...), silent = T)
    if(is(res,"try-error")){return(out)}else{return(res)}
}

#####ttestx()####

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

    
