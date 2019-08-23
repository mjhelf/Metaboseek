#' @include Classes.R


#' @rdname analyzeFT
#' @aliases removeNAs
#' @export
setMethod("removeNAs", "MseekFT",
          function(object, replacement = 0){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              object$df[is.na(object$df)]<-0
              afterHash <- digest::digest(object$df,
                                          algo = "xxhash64")
              object <- addProcessHistory(object, FTProcessHistory(changes = afterHash != beforeHash,
                                                                   inputDFhash = beforeHash,
                                                                   outputDFhash = afterHash,
                                                                   sessionInfo = NULL,
                                                                   info = "Replaced NA values in df by 0!",
                                                                   param = FunParam(fun = "Metaboseek::removeNAs",
                                                                                    args = list(replacement = replacement))
              ))
          })

#' @describeIn exIntensities
#' @aliases getMseekIntensities
#' 
#' @param ... additional arguments passed to \code{exIntensities}
#' 
#' @export
setMethod("getMseekIntensities", signature(object = "MseekFT", rawdata = "list"),
          function(object, rawdata, ppm = 5, rtrange = TRUE, rtw = 5){
               beforeHash <- digest::digest(object$df,
                                               algo = "xxhash64")
               
               err <- list()
               
              tryCatch({
               
                  rta <- rtadjust(object$RTcorr, object$df[,c("rt","rtmin","rtmax")])
                  
                  ###Get Mseek Intensities
                  intens <- lapply(seq_len(length(rawdata)), function(i){
                      if(rtrange){
                          rtwin <- data.frame(rtmin = rta[[i]]$rtmin-rtw,
                                              rtmax = rta[[i]]$rtmax+rtw)
                          rtwin[rtwin < 0]<-0
                      }else{
                          rtwin <- data.frame(rtmin = rta[[i]]$rt-rtw,
                                              rtmax = rta[[i]]$rt+rtw)
                          rtwin[rtwin < 0]<-0
                      }
                      
                      exIntensities(rawfile= rawdata[[i]],
                                    mz = object$df$mz,
                                    ppm= ppm,
                                    rtw= rtwin
                      )
                  })
                  
                  names(intens) <- paste0(basename(names(rawdata)),"__XIC")
                  
                 # return(intens)
                  
                  # ints <- as.data.frame(lapply(bplapply(rawdata,
                  #                                 exIntensities,
                  #                                 mz, ppm, rtw,
                  #                                 baselineSubtract = F,
                  #                                 SN = 10,
                  #                                 BPPARAM = SnowParam(workers = if(length(mz) > 10000){workers}else{1}
                  #                                 )),
                  #                        unlist))
                  # 
                  
                 
                  object$df <- cbind(object$df,intens) 
                  
                  
                  
              },
              error = function(e){
                  print(e)
                  err <- e
              }
              )
              print(rta)
               
              afterHash <- digest::digest(object$df,
                                              algo = "xxhash64")
              object <- addProcessHistory(object, FTProcessHistory(changes = afterHash != beforeHash,
                                                                   inputDFhash = beforeHash,
                                                                   outputDFhash = afterHash,
                                                                   error = if(length(err)){list(getMseekIntensities = err)}else{list()},
                                                                   sessionInfo = NULL,
                                                                   info = "Added Mseek intensities.",
                                                                   param = FunParam(fun = "Metaboseek::getMseekIntensities",
                                                                                    args = list(ppm = ppm,
                                                                                                 rtrange = rtrange,
                                                                                                 rtw = rtw),
                                                                                    longArgs = list(rawdata = summary(rawdata)))
              ))
              return(object)
              
          })

#' @aliases FTNormalize
#' 
#' @param selCols selected columns (with intensity values)
#' @param logNormalized if TRUE, applies a log10 to intensity values after normalization
#' @rdname featureTableNormalize
#' @export
setMethod("FTNormalize", "data.frame",
          function(object, selCols, logNormalized = FALSE){
              
              #normalize data and save it in matrix
              mx <- as.matrix(object[,selCols])
              mx <- featureTableNormalize(mx,
                                          raiseZeros =  min(mx[which(!mx==0, arr.ind=T)]))
              # 
              mx <- featureTableNormalize(mx, normalize = "colMeans")
              if(!is.null(logNormalized) && logNormalized){
                  mx <- featureTableNormalize(mx, log =  "log10")
              }
              
              #make copy of normalized intensities in active table df
              mx <- as.data.frame(mx)
              colnames(mx) <- paste0(colnames(mx),"__norm")
              object <- updateDF (mx,object)
              return(object)
          })

#' @rdname featureTableNormalize
#' @export
setMethod("FTNormalize", "MseekFT",
          function(object, logNormalized = FALSE){
              
              
              #normalize data and save it in matrix
              mx <- as.matrix(object$df[,object$intensities])
              mx <- featureTableNormalize(mx,
                                          raiseZeros =  min(mx[which(!mx==0, arr.ind=T)]))
              # 
              mx <- featureTableNormalize(mx, normalize = "colMeans")
              if(!is.null(logNormalized) && logNormalized){
                  mx <- featureTableNormalize(mx, log =  "log10")
              }
              
              #make copy of normalized intensities in active table df
              mx <- as.data.frame(mx)
              colnames(mx) <- paste0(colnames(mx),"__norm")
              object$df <- updateDF (mx,object$df)
              return(object)
          })