#' @include Classes.R

#' @title MseekFT-class
#' @aliases previousStep
#'
#' \code{previousStep()} extract the most recent \code{ProcessHistory} object from an object,
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
                                                                   info = "Replaced NA values in df by 0.",
                                                                   param = FunParam(fun = "Metaboseek::removeNAs",
                                                                                    args = list(replacement = replacement))
              ))
          })

#' @rdname MseekFT-class
#' @aliases getMseekIntensities
#' 
#' @param adjustedRT use adjusted RTs for all samples for which it is available
#' @param ... additional arguments passed to \code{exIntensities}
#' 
#' @export
setMethod("getMseekIntensities", signature(object = "MseekFT", rawdata = "listOrNULL", importFrom = "missing"),
          function(object, rawdata, adjustedRT = TRUE, ppm = 5, rtrange = TRUE, rtw = 5){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              p1 <- proc.time()
              
              err <- list()
              
              tryCatch({
                  if(is.null(rawdata)){
                      stop("Peak shapes analysis was not performed because no MS data is loaded.")
                      
                  }
                  
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
#' @export
setMethod("getMseekIntensities", signature(object = "MseekFT", rawdata = "listOrNULL", importFrom = "MseekFTOrNULL"),
          function(object, rawdata, importFrom, adjustedRT = TRUE, ppm = 5, rtrange = TRUE, rtw = 5){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              
              p1 <- proc.time()
              
              tryCatch({
                  
                  if(missing(importFrom) || is.null(importFrom)){
                   stop("no importFrom object")   
                  }
                  
                  this.FunParam <- FunParam(fun = "Metaboseek::getMseekIntensities",
                                            args = list(adjustedRT = adjustedRT,
                                                        ppm = ppm,
                                                        rtrange = rtrange,
                                                        rtw = rtw),
                                            longArgs = list(rawdata = summary(rawdata)))
                  
                  oldParams <- searchFunParam(importFrom, "Metaboseek::getMseekIntensities")
                  
                  oldParams <- oldParams[!sapply(oldParams, hasError)]
                  
                  oldParams <- oldParams[[length(oldParams)]]
                  
                  if(!identical(oldParams@param,this.FunParam)){
                      stop("used different settings before")

                  }
                  
                  if(!identical(importFrom$MseekIntensities, paste0(basename(names(rawdata)), "__XIC"))){
                      stop("importFrom has additional MseekIntensities")
                      
                  }
                  
                  if(!identical(object$df[,c("rt","rtmin", "rtmax", "mz")], importFrom$df[,c("rt", "rtmin", "rtmax", "mz")])){
                      stop("data frames are different")
                      
                  }
                  
                  if(!identical(object$RTcorr, importFrom$RTcorr)){
                      stop("RT correction information differs")
                      
                  }
                  
                  object$df <- updateDF(importFrom$df[,importFrom$MseekIntensities],
                                        object$df) 
                  
                  object$MseekIntensities <- unique(c(object$MseekIntensities,
                                                      importFrom$MseekIntensities))
                  
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- digest::digest(object$df,
                                              algo = "xxhash64")
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputDFhash = beforeHash,
                                                               outputDFhash = afterHash,
                                                               fileNames = names(rawdata),
                                                               error = list(),
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = paste0("Added Mseek intensities, safely transferred from ", importFrom$tablename),
                                                               param = FunParam(fun = "Metaboseek::getMseekIntensities",
                                                                                args = list(adjustedRT = adjustedRT,
                                                                                            ppm = ppm,
                                                                                            rtrange = rtrange,
                                                                                            rtw = rtw),
                                                                                longArgs = list(rawdata = summary(rawdata)))
                                              ))
                  
                  },
                  error = function(e){
                      message(paste("skipped import:",e,"\ntrying to calculate MseekIntensities from scratch now..."))
                      object <<-  getMseekIntensities(object = object, rawdata = rawdata,
                                      adjustedRT = adjustedRT, ppm = ppm,
                                      rtrange = rtrange, rtw = rtw)
                      
                      },
                  warning = function(w){print(w)})
              
              return(object)
              
              })

#' Get a history entries for a function
#' @rdname MseekFT-class
#' @param index get index in list instead of actual value
#' @export
setMethod("searchFunParam", "MseekFT",
          function(object, fun = "", index = FALSE){
              if(!length(processHistory(object))){return(list())}
              
              hits <- sapply(processHistory(object), function(x){
                  if(!"param" %in% slotNames(x)){return(FALSE)}
                  if(!"fun" %in% slotNames(x@param)){return(FALSE)}
                  if(x@param@fun != fun){return(FALSE)}
                  return(TRUE)
              })
              if(index){
                  return(which(hits))
                  }else{
                  return(processHistory(object)[hits])
                      }
              
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

#' Get or set the Mseek intensity column grouping
#' @rdname MseekFT-class
#' @export
setMethod("groupingTable", "MseekFT",
          function(object){
              object$anagrouptable   
          })
#' @rdname MseekFT-class
#' @export         
setReplaceMethod("groupingTable", c("MseekFT", "data.frame"),
                 function(object, value){
                     object <- updateFTgrouping(object,value)
                     object
                 })
          

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

#' @aliases FTNormalize
#' 
#' @description \code{FTNormalize}: Normalize a feature table such that the mean
#'  values of all intensity columns will be equal.
#' @rdname MseekFT-class
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
                                                               info = "Normalized intensity columns.",
                                                               param = FunParam(fun = "Metaboseek::FTNormalize",
                                                                                args = list(intensityColumns = object$intensities,
                                                                                            logNormalized = logNormalized),
                                                                                longArgs = list())
                                              ))
              }
              )
              return(object)
          })

#' @rdname MseekFT-class
#' @export
setMethod("FTBasicAnalysis", "MseekFT",
          function(object, intensityCols = NULL, grouping = NULL, controlGroup = NULL){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if(missing(intensityCols) || is.null(intensityCols)){
                      intensityCols <- object$intensities
                  }
                  if(missing(grouping) || is.null(grouping)){
                      grouping <- object$anagroupnames
                  }
                  
                  
                  object <- updateFeatureTable(object,
                                               featureCalcs(object$df))
                  
                  object <- updateFeatureTable(object,
                                               foldChange(as.matrix(object$df[,intensityCols]),
                                            groups = grouping, ctrl = controlGroup))
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$FTBasicAnalysis <<- paste(e)
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
                                                               info = "Basic fold-change analysis.",
                                                               param = FunParam(fun = "Metaboseek::FTBasicAnalysis",
                                                                                args = list(intensityCols = intensityCols,
                                                                                            grouping = grouping,
                                                                                            controlGroup = controlGroup),
                                                                                longArgs = list())
                                              ))
              }
              )
              return(object)
          })
#' @rdname MseekFT-class
#' @export
setMethod("FTOldPeakShapes", c("MseekFT", "listOrNULL"),
          function(object, rawdata, ppm = 5, workers = 1){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if(is.null(rawdata)){
                      stop("Peak shapes analysis was not performed because no MS data is loaded.")
                      
                  }
                  if (!"mz" %in% colnames(object$df) || !"rt" %in% colnames(object$df)){
                      stop("Peak shapes analysis was not performed because table does not contain 'rt' and 'mz' columns.")
                  }
                  
                              inp <- bestgauss(
                                  rawdata= rawdata,
                                  mz = data.frame(mzmin = object$df$mz-ppm*1e-6*object$df$mz,
                                                  mzmax=object$df$mz+ppm*1e-6*object$df$mz),
                                  rt = data.frame(rtmin = object$df$rt-10,
                                                  rtmax=object$df$rt+10),
                                  workers = workers,
                                  rnames = row.names(object$df)
                              )
                              
                              object <- updateFeatureTable(object, inp)
                              
                          
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$FTOldPeakShapes <<- paste(e)
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
                                                               info = "Scored peak shapes with old peak shape analysis.",
                                                               param = FunParam(fun = "Metaboseek::FTOldPeakShapes",
                                                                                args = list(ppm = ppm,
                                                                                            workers = workers),
                                                                                longArgs = list(rawdata = summary(rawdata)))
                                              ))
              }
              )
              return(object)
          })

#' @rdname MseekFT-class
#' @export
setMethod("FTPeakShapes", c("MseekFT", "listOrNULL"),
          function(object, rawdata, ppm = 5, workers = 1){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if(is.null(rawdata)){
                      stop("Peak shapes analysis was not performed because no MS data is loaded.")
                      
                  }
                  if (!"mz" %in% colnames(object$df) || !"rt" %in% colnames(object$df)){
                      stop("Peak shapes analysis was not performed because table does not contain 'rt' and 'mz' columns.")
                  }
                  
                  inp <- data.frame("Fast_Peak_Quality" = fastPeakShapes(
                      rawdata= rawdata,
                      mz = object$df$mz,
                      ppm = ppm,
                      rtw = data.frame(rtmin = object$df$rt-10, rtmax= object$df$rt+10),
                      workers = workers
                  ))
                  
                  object <- updateFeatureTable(object, inp)
                  
                  
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$FTPeakShapes <<- paste(e)
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
                                                               info = "Scored peak shapes with fast peak shape analysis.",
                                                               param = FunParam(fun = "Metaboseek::FTPeakShapes",
                                                                                args = list(ppm = ppm,
                                                                                            workers = workers),
                                                                                longArgs = list(rawdata = summary(rawdata)))
                                              ))
              }
              )
              return(object)
          })


#' @rdname MseekFT-class
#' @export
setMethod("FTMzMatch", c("MseekFT"),
          function(object, db, ppm = 5, mzdiff = 0.001){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if(missing(db) || is.null(db)){
                      stop("mzMatch db is missing")
                      
                  }
                  
                  #remove previous mzMatch results
                  remcols <-  grepl("mzMatch_", colnames(object$df)) | colnames(object$df) %in% c("mzMatches", "mzMatchError")
                  
                  object$df <- object$df[,!remcols,drop = F]
                  
                  inp <- mzMatch(object$df, db = db,
                                 ppm = ppm, mzdiff = mzdiff)
                  
                  object <- updateFeatureTable(object, inp)

                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$FTMzMatch <<- paste(e)
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
                                                               info = "Matched mz values with a database.",
                                                               param = FunParam(fun = "Metaboseek::FTMzMatch",
                                                                                args = list(ppm = ppm,
                                                                                            mzdiff = mzdiff),
                                                                                longArgs = list(db = if(is.character(db)){db}else{summary(db)}))
                                              ))
              }
              )
              return(object)
          })

#' @rdname MseekFT-class
#' @export
setMethod("FTT.test", c("MseekFT"),
          function(object, intensityCols = NULL,
                   grouping = NULL,
                   adjmethod = "bonferroni"){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              

              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if(missing(intensityCols) || is.null(intensityCols)){
                      intensityCols <- object$intensities
                  }
                  if(missing(grouping) || is.null(grouping)){
                      grouping <- object$anagroupnames
                  }
                  
                  inp <- multittest(df = object$df[,intensityCols],
                                            groups = grouping,
                                            ttest = T,
                                            adjmethod = adjmethod)
                  
                  object <- updateFeatureTable(object, inp)
                  
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$FTT.test <<- paste(e)
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
                                                               info = "Calculated t-test.",
                                                               param = FunParam(fun = "Metaboseek::FTT.test",
                                                                                args = list(grouping = grouping,
                                                                                            intensityCols = intensityCols,
                                                                                            adjmethod = adjmethod),
                                                                                longArgs = list())
                                              ))
              }
              )
              return(object)
          })


#' @rdname MseekFT-class
#' @export
setMethod("FTAnova", c("MseekFT"),
          function(object, intensityCols = NULL,
                   grouping = NULL){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              
              
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if(missing(intensityCols) || is.null(intensityCols)){
                      intensityCols <- object$intensities
                  }
                  if(missing(grouping) || is.null(grouping)){
                      grouping <- object$anagroupnames
                  }
                  
                  inp <- MseekAnova(df = object$df[,intensityCols],
                                                   groups = grouping)
                                    
                  
                  object <- updateFeatureTable(object, inp)
                  
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$FTAnova <<- paste(e)
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
                                                               info = "Calculated t-test.",
                                                               param = FunParam(fun = "Metaboseek::FTAnova",
                                                                                args = list(grouping = grouping,
                                                                                            intensityCols = intensityCols),
                                                                                longArgs = list())
                                              ))
              }
              )
              return(object)
          })


#' @rdname MseekFT-class
#' @export
setMethod("FTCluster", c("MseekFT"),
          function(object, intensityCols = NULL,
                   numClusters = 100L){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              
              
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if(missing(intensityCols) || is.null(intensityCols)){
                      intensityCols <- object$intensities
                  }
                  
                  if(numClusters >= nrow(object$df)){
                      numClusters <- nrow(object$df)-1
                      }

                  
                  if(length(intensityCols) <2 ){
                      stop("Clara cluster analysis was not performed because there is only one sample.")
                  }
           #using sqrt here to condense data values which may contain 0s (hence no log)
                      mx <- sqrt(as.matrix(object$df[,intensityCols])) 
                      
                      
                      inp <- MosCluster(x = mx / rowMeans(mx),
                                                k = numClusters,
                                                samples = 100)
                  
                      object <- updateFeatureTable(object, inp)

              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$FTCluster <<- paste(e)
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
                                                               info = "Clustered features by intensity values.",
                                                               param = FunParam(fun = "Metaboseek::FTCluster",
                                                                                args = list(numClusters = numClusters,
                                                                                            intensityCols = intensityCols),
                                                                                longArgs = list())
                                              ))
              }
              )
              return(object)
          })


#' @rdname MseekFT-class
#' @description \code{FTPCA}: Calculate Principal Component Analysis to cluster either features or samples
#' @param featureMode if TRUE, will cluster molecular features by intensities 
#' across samples. If FALSE, will cluster samples by intensities across features
#' @export
setMethod("FTPCA", c("MseekFT"),
          function(object,
                   intensityCols = NULL,
                   featureMode = FALSE){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              
              
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if(missing(intensityCols) || is.null(intensityCols)){
                      intensityCols <- object$intensities
                  }
                  
                  
                  if(length(intensityCols) < 2){
                      stop("PCA was not performed because there are less than two samples.")
                  }
                  #using sqrt here to condense data values which may contain 0s (hence no log)
                  if(featureMode){
                  pcamemx <- as.matrix(object$df[,intensityCols])
                  }else{
                  pcamemx <- t(as.matrix(object$df[,intensityCols]))
                  }
                  
                  
                  pcamemx <- scale(pcamemx, center = T, scale = T)
                  
                  #PCA to separate features
                  prin_comp <- prcomp(pcamemx)
                  
                  f <- function(x){sqrt(sum(x^2))}
                  
                  dists <- apply(prin_comp$x,1,f)
                  #keep up to 15 PCs
                  prin_comp <- as.data.frame(prin_comp$x[,1:min(ncol(prin_comp$x),15)])
                  prin_comp$vlength <- dists
                  colnames(prin_comp) <- paste0("PCA__", colnames(prin_comp))
                  
                  if(featureMode){
                      object <- updateFeatureTable(object, prin_comp)
                  }else{
                      
                      metaDF <- data.frame(Column = row.names(prin_comp),
                                           stringsAsFactors = FALSE,
                                           row.names = row.names(prin_comp))
                      #add grouing information for visualization purposes:
                      if(length(groupingTable(object))){
                          getgroups <- match(gsub("__norm$","",metaDF$Column),
                                             groupingTable(object)$Column)
                          if(all(!is.na(getgroups))){
                              metaDF$Group <- groupingTable(object)$Group[getgroups]
                              }
                          }
                          
                      object$PCA_samples <- cbind(metaDF,
                                                  prin_comp
                                                  )
                  }
                  
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$FTPCA <<- paste(e)
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
                                                               info = "Performed PCA.",
                                                               param = FunParam(fun = "Metaboseek::FTPCA",
                                                                                args = list(featureMode = featureMode,
                                                                                            intensityCols = intensityCols),
                                                                                longArgs = list())
                                              ))
              }
              )
              return(object)
          })

#' @rdname MseekFT-class
#' @description \code{FTFilter}: apply a list of filters to a MseekFT object
#' @param filters a list of filters
#' @export
setMethod("FTFilter", c("data.frame"),
          function(object,
                   filters = list(),
                   sortBy = charachter(),
                   decreasing = TRUE){

                  
                  if(!missing(filters) || !length(filters)){
                     
                  sel <- TRUE
                  
                  for(i in filters){
                      
                      names(i) <- gsub("Init$","",names(i))
                      
                      if(length(i$colSelected) == 0 || !i$colSelected %in% colnames(object)){
                          i$active <- F
                      }
                      
                      if(length(i$active) && i$active){
                          if(i$numeric){
                              sel <- sel & (object[,i$colSelected] >= as.numeric(i$minSel)
                                            & object[,i$colSelected] <= as.numeric(i$maxSel))
                          }else{
                              
                              
                              if(!is.null(i$modeSel) && i$modeSel=="contains"){
                                  sel <- sel &  grepl(i$txtSel,
                                                      as.character(object[,i$colSelected]),
                                                      fixed = T)
                              }else if(!is.null(i$modeSel) && i$modeSel=="does not contain"){
                                  sel <- sel & !grepl(i$txtSel,
                                                      as.character(object[,i$colSelected]),
                                                      fixed = T)
                              }else if(!is.null(i$modeSel) && i$modeSel=="is not"){
                                  sel <- sel &  ! (as.character(object[,i$colSelected]) == i$txtSel)
                                  
                              }
                              #if(input$modeSel=="is"){
                              else{
                                  sel <- sel &  as.character(object[,i$colSelected]) == i$txtSel
                              }
                          }
                          
                      }
                  }
                  
                  object <- object[sel,, drop = FALSE]
                  }
                  
                  if(length(sortBy) && sortBy %in% colnames(object)){
                      ord <- order(res[,sortBy],
                                   decreasing = decreasing)
                      object <- object[ord,]
                      }
                  
                  return(object)
                  
              })

#' @rdname MseekFT-class
#' @description \code{FTFilter}: apply a list of filters to a MseekFT object
#' @param filters a list of filters
#' @export
setMethod("FTFilter", c("MseekFT"),
          function(object,
                   filters = list(),
                   sortBy = charachter(),
                   decreasing = TRUE){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              
              
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if((missing(filters) 
                     || !length(filters)) && !length(sortBy)){
                      return(object)
                  }
                  
                  
                  object$df <- FTFilter(object$df,
                                        filters = filters,
                                        sortBy = sortBy,
                                        decreasing = decreasing)
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$FTFilter <<- paste(e)
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
                                                               info = "Filtered Feature Table.",
                                                               param = FunParam(fun = "Metaboseek::FTFilter",
                                                                                args = list(filters = filters,
                                                                                            sortBy = sortBy,
                                                                                            decreasing = decreasing),
                                                                                longArgs = list())
                                              ))
              }
              )
              return(object)
          })
