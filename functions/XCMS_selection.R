############################################################
## getEICNew
## what's different in this method?
## 1) we're not (re-) calculating the profile matrix if it already exists and if the step argument
##    is the same.
## 2) by not using the buffer with the fixed (max) size of 100 we're no longer limited to small m/z
##    ranges, thus we can use the method to extract the EIC for the full m/z range (i.e. the base
##    peak chromatogram BPC).
## 3) the method might be slower.
## We've got a problem if step = 0! (relates to issue #39)
getEICNew <- function(object, mzrange, rtrange = NULL,
                      step = 0.1, BPPARAM = bpparam()) {
    ## if mzrange and rtrange is not provided use the full range.
    if(missing(mzrange)){
        if(length(object@mzrange) == 2){
            mzrange <- matrix(object@mzrange, nrow=1)
        }else{
            mzrange <- matrix(c(min(object@env$mz), max(object@env$mz)), nrow=1)
        }
        colnames(mzrange) <- c("mzmin", "mzmax")
    }
    if(is.null(rtrange)){
        rtrange <- matrix(range(object@scantime), nrow=1)
        colnames(rtrange) <- c("rtmin", "rtmax")
    }
    ## rtrange and mzrange have to have the same number of rows!
    if(nrow(rtrange)!=nrow(mzrange)){
        stop("rtrange and mzrange have to have the same number of rows!")
    }
    profFun <- match.profFun(object)
    if (all(c("mzmin","mzmax") %in% colnames(mzrange)))
        mzrange <- mzrange[,c("mzmin", "mzmax"),drop=FALSE]
    
    ## check if we have the profile and if, if the profile step fits the step...
    if(any(names(object@env) == "profile" )){
        pStep <- profStep(object)
        if (length(pStep) == 0)
            pStep <- step
        if(pStep != step){
            ## delete that profile matrix since the step differs.
            rm(list="profile", envir=object@env)
        }
    }
    
    mass <- seq(floor(min(object@env$mz)/step)*step,
                ceiling(max(object@env$mz)/step)*step, by = step)
    ## check if we've got already the profile matrix available, if yes, we don't have to
    ## re-calculate anything.
    if(!any(names(object@env) == "profile")){
        ## calculate the profile matrix.
        object@env$profile <- profFun(object@env$mz, object@env$intensity,
                                      object@scanindex, length(mass), mass[1],
                                      mass[length(mass)], TRUE, object@profparam)
    }
    
    ## once we've got the full profile matrix we go on and extract the EICs.
    parms <- vector("list", length=nrow(rtrange))
    for(i in 1:length(parms)){
        parms[[i]] <- list( mzrange=mzrange[i, ], rtrange=rtrange[i, ] )
    }
    ## check if we could run the code on multiple cpus...
    eic <- bplapply(parms, FUN=function(z){
        imz <- findRange(mass, c(z$mzrange[1]-.5*step, z$mzrange[2]+0.5*step), TRUE)
        irt <- which(object@scantime >= z$rtrange[1] & object@scantime <= z$rtrange[2])
        e <- matrix(c(object@scantime[irt],
                      colMax(object@env$profile[imz[1]:imz[2], irt, drop=FALSE])), ncol=2)
        colnames(e) <- c("rt", "intensity")
        return(e)
    }, BPPARAM=BPPARAM)
    
    invisible(new("xcmsEIC", eic = list(xcmsRaw=eic), mzrange = mzrange, rtrange = rtrange,
                  rt = "raw", groupnames = character(0)))
}

############################################################