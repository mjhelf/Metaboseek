## Methods to analyze MseekFT objects
#' @include Functions_FeatureTable_analysis.R


#' @title analyzeFT
#' @aliases analyzeFT
#' @name analyzeFT
#' 
#' @description \code{analyzeFT()}: wrapper function for the methods described here. 
#' Analyze MseekFT objects, recording a processing history .
#'  Will use intensity columns and grouping information from the MseekFT object
#'  if available. See \code{\link{analyzeTable}} for the old version of this.
#'
#' @param object an MseekFT or data.frame object.
#' @param MSData,rawdata list of xcmsRaw objects
#' @param param a \code{\link{FTAnalysisParam}} object
#' @param intensityCols a vector of column names which contain intensity values 
#' to use for a calculation step. If not defined, will use the columns defined 
#' in the object as \code{$intensities}.
#' @param grouping named list of character vectors, defining column 
#' names for different sample groups.
#' 
#' 
#' @return an object of the same class as \code{object}, with analyses performed 
#' and recorded in the \code{processHistory}
#'   
#' @rdname analyzeFT
#' @export
setMethod("analyzeFT", 
          signature(object = "MseekFT",
                    MSData = "listOrNULL",
                    param = "FTAnalysisParam"),
          function(object, MSData, param){
              
              param@intensities <- object$intensities
              param@groups <- object$anagroupnames
              inputHash <- digest::digest(object$df, algo = "xxhash64")
              
              if(param@normalize || (param@useNormalized && !identical(grep("__norm",colnames(object$df), value = T), paste0(object$intensities,"__norm")))){ 
                  
                  object <- FTNormalize(object,
                                        logNormalized = param@logNormalized)
              }
              
              if(param@useNormalized){
                  param@intensities <- paste0(object$intensities,"__norm")
                  param@groups <- lapply(object$anagroupnames, paste0, "__norm")
              }
              
              if("Basic analysis" %in% param@analyze){
                  
                  object <- FTBasicAnalysis(object,
                                            intensityCols = param@intensities,
                                            grouping = param@groups,
                                            controlGroup = param@controlGroup)
                  
              }
              
              if("Peak shapes" %in% param@analyze){
                  
                  object <- FTOldPeakShapes(object,
                                            rawdata = MSData,
                                            ppm = param@ppm,
                                            workers = param@workers)
                  
              }
              
              if("Fast peak shapes" %in% param@analyze){
                  
                  object <- FTPeakShapes(object,
                                         rawdata = MSData,
                                         ppm = param@ppm,
                                         workers = param@workers)
                  
              }
              
              if("mzMatch" %in% param@analyze){
                  
                  object <- FTMzMatch(object,
                                      db = param@mzMatchParam$db,
                                      ppm = param@mzMatchParam$ppm,
                                      mzdiff = param@mzMatchParam$mzdiff
                  )
                  
              }  
              
              if("t-test" %in% param@analyze){
                  
                  object <- FTT.test(object,
                                     intensityCols = param@intensities,
                                     grouping = param@groups,
                                     adjmethod = "bonferroni"
                  )
                  
              }  
              
              if("anova" %in% param@analyze){
                  
                  object <- FTAnova(object,
                                    intensityCols = param@intensities,
                                    grouping = param@groups)
                  
              }  
              
              if("clara_cluster" %in% param@analyze){
                  
                  object <- FTCluster(object,
                                      intensityCols = param@intensities,
                                      numClusters = param@numClusters)
                  
              }  
              
              if("PCA features" %in% param@analyze){
                  
                  object <- FTPCA(object,
                                  intensityCols = param@intensities,
                                  featureMode = TRUE)
                  
              }  
              
              if("PCA samples" %in% param@analyze){
                  
                  object <- FTPCA(object,
                                  intensityCols = param@intensities,
                                  featureMode = FALSE)
                  
              }  
              
              
              return(object)
          })

#' @aliases removeNAs
#' 
#' @description \code{removeNAs}: remove NA values from a range of columns and replace them with another value
#' @param replacement value to put in place of NA values
#' 
#' @rdname analyzeFT
#' @export
setMethod("removeNAs", "MseekFT",
          function(object, intensityCols = NULL, replacement = 0){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              p1 <- proc.time()
              err <- list()
              tryCatch({
                  
                  if(missing(intensityCols) || is.null(intensityCols)){
                      intensityCols <- object$intensities
                  }
                  
                  if(!length(intensityCols)){
                      stop("no intensityCols defined")
                  }
                  
                  if(!all(intensityCols %in% colnames(object$df))){
                      stop("Some expected intensityCols are not in the dataframe")
                  }
                  
                  ints <- object$df[,intensityCols]
                  
                  ints[is.na(ints)] <-0
                  
                  object$df <- updateDF(ints, object$df)
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$removeNAs <<- paste(e)
              },
              finally = {
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- digest::digest(object$df,
                                              algo = "xxhash64")
                  object <- addProcessHistory(object, FTProcessHistory(changes = afterHash != beforeHash,
                                                                       inputDFhash = beforeHash,
                                                                       outputDFhash = afterHash,
                                                                       error = err,
                                                                       processingTime = p1,
                                                                       sessionInfo = NULL,
                                                                       info = "Replaced NA values in df by 0.",
                                                                       param = FunParam(fun = "Metaboseek::removeNAs",
                                                                                        args = list(
                                                                                            intensityCols = intensityCols,
                                                                                            replacement = replacement
                                                                                            ))
                                                                       
                  ))
              })
              return(object)
          })

#' @aliases FTNormalize
#' 
#' @description \code{FTNormalize}: Replaces zeroes by the globally smallest 
#' non-zero intensity value, then normalizes a feature table such that the mean
#'  values of all intensity columns will be equal. See also 
#'  \code{\link{featureTableNormalize}()}
#' @param logNormalized if TRUE, applies log10 to intensity values after normalization
#' @rdname analyzeFT
#' @export
setMethod("FTNormalize", "MseekFT",
          function(object, intensityCols = NULL, logNormalized = FALSE){
              beforeHash <- digest::digest(object$df,
                                           algo = "xxhash64")
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  if(missing(intensityCols) 
                     || is.null(intensityCols)){
                      print(intensityCols(object))
 
                      intensityCols <- intensityCols(object)
                  }
                  
                  if(!length(intensityCols)){
                      stop("no intensityCols defined")
                  }
                  
                  if(!all(intensityCols %in% colnames(object$df))){
                      stop("Some expected intensityCols are not in the dataframe")
                  }
                  
                  #normalize data and save it in matrix
                  mx <- as.matrix(object$df[,intensityCols])
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
                  object <- updateFeatureTable(object,mx)
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

#' @aliases FTBasicAnalysis
#' 
#' @description \code{FTBasicAnalysis}: calculate fold changes between groups of samples. See also 
#' \code{\link{foldChange}()} and \code{\link{featureCalcs}()}for a description of the resulting columns
#' @param controlGroup character() defining which sample group serves as control
#' (will calculate foldChanges over control if not NULL)
#' 
#' @rdname analyzeFT
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


#' @aliases getMseekIntensities
#' 
#' @param adjustedRT use adjusted RTs for all samples for which it is available
#' @param rtrange if TRUE, will use \code{rtw} starting out from the \code{rtmin} 
#' and \code{rtmax} values instead of \code{rt}
#' @param rtw retention time window to get the intensity from, +/- in seconds
#' @param areaMode if TRUE, will calculate peak areas rather than mean intensities
#' @description \code{getMseekIntensities}: get EIC-based intensities for each
#'  molecular feature in the MseekFT object for each file in \code{rawdata}.
#'  if another MseekFT object is supplied as \code{importFrom}, will try to transfer MseekIntensities from there if 
#'  all settings, features and MS data files are equivalent. See also \code{\link{exIntensities}}
#' @rdname analyzeFT
#' @export
setMethod("getMseekIntensities", signature(object = "MseekFT",
                                           rawdata = "listOrNULL",
                                           importFrom = "missing"),
          function(object, rawdata, adjustedRT = TRUE, ppm = 5,
                   rtrange = TRUE, rtw = 5,
                   areaMode = FALSE){
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
                                    rtw= rtwin,
                                    areaMode = areaMode)
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
                                                                                            rtw = rtw,
                                                                                            areaMode = areaMode),
                                                                                longArgs = list(rawdata = summary(rawdata)))
                                              ))
              }
              )
              #print(rta)
              
              
              return(object)
              
          })
#' @rdname analyzeFT
#' @param importFrom a \code{MseekFT} object to use as source for Mseek intensities.
#' @export
setMethod("getMseekIntensities", signature(object = "MseekFT",
                                           rawdata = "listOrNULL",
                                           importFrom = "MseekFTOrNULL"),
          function(object, rawdata, importFrom,
                   adjustedRT = TRUE, ppm = 5, rtrange = TRUE, rtw = 5,
                   areaMode = FALSE){
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
                                                        rtw = rtw,
                                                        areaMode = areaMode),
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
                                                                                            rtw = rtw,
                                                                                            areaMode = areaMode),
                                                                                longArgs = list(rawdata = summary(rawdata)))
                                              ))
                  
              },
              error = function(e){
                  message(paste("skipped import:",e,"\ntrying to calculate MseekIntensities from scratch now..."))
                  object <<-  getMseekIntensities(object = object, rawdata = rawdata,
                                                  adjustedRT = adjustedRT, ppm = ppm,
                                                  rtrange = rtrange, rtw = rtw, areaMode = areaMode)
                  
              },
              warning = function(w){print(w)})
              
              return(object)
              
          })

#' @aliases FTOldPeakShapes
#' 
#' @description \code{FTOldPeakShapes}: calculate score for peak shapes. This method is kept
#'  for backwards reproducibility and not recommended, because \code{FTPeakShapes()}
#' is much faster. See also \code{\link{bestgauss}()}
#' @param workers number of worker processes
#' 
#' @rdname analyzeFT
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

#' @aliases FTPeakShapes
#' 
#' @description \code{FTPeakShapes}: calculate score for peak shapes. Adds a 
#' \code{Fast_Peak_Quality} column to the data.frame in the MseekFT object. See 
#' also \code{\link{fastPeakShapes}()}
#'  
#' @rdname analyzeFT
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


#' @aliases FTMzMatch
#' 
#' @description \code{FTMzMatch}: Match mz values in this MseekFT with mz values from a data base. 
#' See also \code{\link{mzMatch}()}
#' @param db data base to search, either a vector of file paths .csv files
#'  or a data.frame, see \code{\link{mzMatch}()}
#' @param ppm ppm mz tolerance
#' @param mzdiff maximum mz difference. NOTE: either ppm OR mzdiff 
#' condition has to be met
#' 
#' @rdname analyzeFT
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

#' @aliases FTT.test
#' 
#' @description \code{FTT.test}: calculate t-test between samples. Works only if there are 
#' two groups in \code{grouping} with multiple members. See also \code{\link{multittest}()}
#' @param adjmethod method to use for p-value adjustment, see \code{\link[stats]{p.adjust}()}
#' 
#' @rdname analyzeFT
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


#' @aliases FTAnova
#' 
#' @description \code{FTAnova}: calculate two-way ANOVA between multiple sample groups.
#'  See also \code{\link{MseekAnova}()}
#' 
#' @rdname analyzeFT
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


#' @aliases FTCluster
#' 
#' @description \code{FTCluster}: calculate two-way ANOVA between multiple sample groups.
#'  See also \code{\link{MosCluster}()}
#' @param numClusters number of clusters to group the features in. Will 
#' automatically be set to be at most number of features - 1.
#' 
#' @rdname analyzeFT
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


#' @aliases FTPCA
#' @rdname analyzeFT
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