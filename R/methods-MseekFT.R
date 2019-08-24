#' @include Classes.R

#' previousStep
#'
#' extract the most recent \code{ProcessHistory} object from an object,
#'  representing the last recorded changes made to the object.
#'
#' @rdname MseekFT-class
#' @export
setMethod("previousStep", "MseekFT",
          function(object){
              object$.processHistory[[length(object$.processHistory)]]
})

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
#' @param adjustedRT use adjusted RTs for all samples for which it is available
#' @param ... additional arguments passed to \code{exIntensities}
#' 
#' @export
setMethod("getMseekIntensities", signature(object = "MseekFT", rawdata = "list"),
          function(object, rawdata, adjustedRT = TRUE, ppm = 5, rtrange = TRUE, rtw = 5){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              p1 <- proc.time()
              
              err <- list()
              
              tryCatch({
                  if(adjustedRT){
                  rta <- rtadjust(object$RTcorr, object$df[,c("rt","rtmin","rtmax")])
                  }else{
                      rta <- NULL
                      }
                  #print(names(rta))
                  ###Get Mseek Intensities
                  intens <- lapply(seq_len(length(rawdata)), function(i){
                      
                      if(!is.null(rta)){
                      indx <- which(names(rta) == basename(names(rawdata))[i])
                      if(!length(indx)){
                          stop(paste0("No rt correction information available for file ",
                                  names(rawdata)[i],
                                  "! Try rerunning with adjustedRT = FALSE!"))
                          }
                      }else{
                          indx <- character()
                          }
                      
                      
                      if(length(indx)){
                          if(rtrange){
                          rtwin <- data.frame(rtmin = rta[[i]]$rtmin-rtw,
                                              rtmax = rta[[i]]$rtmax+rtw)
                          rtwin[rtwin < 0]<-0
                      }else{
                          rtwin <- data.frame(rtmin = rta[[i]]$rt-rtw,
                                              rtmax = rta[[i]]$rt+rtw)
                          rtwin[rtwin < 0]<-0
                      }
                      
                      }else{
                          
                          if(rtrange){
                              rtwin <- data.frame(rtmin = object$df$rtmin-rtw,
                                                  rtmax = object$df$rtmax+rtw)
                              rtwin[rtwin < 0]<-0
                          }else{
                              rtwin <- data.frame(rtmin = object$df$rt-rtw,
                                                  rtmax = object$df$rt+rtw)
                              rtwin[rtwin < 0]<-0
                          }
                          
                          
                          }
                      
                      exIntensities(rawfile= rawdata[[i]],
                                    mz = object$df$mz,
                                    ppm= ppm,
                                    rtw= rtwin)
                  })
                  
                  names(intens) <- paste0(basename(names(rawdata)),"__XIC")
                  
                  
                  object$df <- updateDF(as.data.frame(intens,
                                                      stringsAsFactors = FALSE),
                                        object$df) 
                  
                  object$MseekIntensities <- unique(c(object$MseekIntensities,
                                               names(intens)))

              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$getMseekIntensities <<- paste(e)
              },
              finally = {
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- digest::digest(object$df,
                                              algo = "xxhash64")
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputDFhash = beforeHash,
                                                               outputDFhash = afterHash,
                                                               fileNames = names(rawdata),
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = "Added Mseek intensities.",
                                                               param = FunParam(fun = "Metaboseek::getMseekIntensities",
                                                                                args = list(adjustedRT = adjustedRT,
                                                                                    ppm = ppm,
                                                                                            rtrange = rtrange,
                                                                                            rtw = rtw),
                                                                                longArgs = list(rawdata = summary(rawdata)))
                                              ))
              }
              )
              #print(rta)
              
              
              return(object)
              
          })

#'
#' Get or set the Mseek intensity column names
#' @rdname MseekFT-class
#' @export
setMethod("intensityCols", "MseekFT",
          function(object){
           object$intensities   
          })
#' @rdname MseekFT-class
#' @export         
setReplaceMethod("intensityCols", "MseekFT",
                    function(object, value){
                        object$intensities <- value
                        object
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
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
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
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$getMseekIntensities <<- paste(e)
              },
              finally = {
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- digest::digest(object$df,
                                              algo = "xxhash64")
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputDFhash = beforeHash,
                                                               outputDFhash = afterHash,
                                                               fileNames = character(),
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = "Added Mseek intensities.",
                                                               param = FunParam(fun = "Metaboseek::getMseekIntensities",
                                                                                args = list(intensities = object$intensities,
                                                                                            logNormalized = logNormalized),
                                                                                longArgs = list())
                                              ))
              }
              )
              return(object)
          })
