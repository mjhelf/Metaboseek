## Methods to analyze MseekFT objects
#' @include Functions_FeatureTable_analysis.R Functions_Labelfinder.R


#' @title analyzeFT
#' @aliases analyzeFT
#' @rdname analyzeFT
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
              
             
              if(!is.null(param@replaceNAs)){
                object <- removeNAs(object,
                                    replacement = param@replaceNAs)
              }
              
            
              # if(param@normalize 
              #    || (param@useNormalized 
              #        && !identical(grep("__norm",colnames(object$df), value = T),
              #                      paste0(object$intensities,"__norm")))){ 
                  
                  object <- FTNormalize(object,
                                        normalize = param@normalize,
                                        logNormalized = param@logNormalized,
                                        zeroReplacement = param@zeroReplacement,
                                        normalizationFactors = param@normalizationFactors
                                        )
             # }
             
             
             
              
              if(param@useNormalized){
                  param@intensities <- paste0(object$intensities,"__norm")
                  param@groups <- lapply(object$anagroupnames, paste0, "__norm")
              }
             
             if("Calculate M" %in% param@analyze){
               
               object <- FTcalculateM(object,
                                         intensityCols = NULL, #always use non-normalized values here
                                         maxInvalid = length(object$intensities)/10, #10% missing values allowed
                                         BPPARAM=bpparam())
               
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
                                     adjmethod = param@p.adjust.method,
                                     controlGroup = param@controlGroup
                  )
                  
              }  
              
              if("anova" %in% param@analyze){
                  
                  object <- FTAnova(object,
                                    intensityCols = param@intensities,
                                    adjmethod = param@p.adjust.method,
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
              beforeHash <- MseekHash(object)
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
                  
                  ints[is.na(ints)] <- replacement
                  
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
                  afterHash <- MseekHash(object)
                  object <- addProcessHistory(object, FTProcessHistory(changes = afterHash != beforeHash,
                                                                       inputHash = beforeHash,
                                                                       outputHash = afterHash,
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



#' @aliases FTcalculateM
#' 
#' @description \code{FTcalculateM}: Calculates M value as detailed by Vandesompele et al. (2002) 
#' @param maxInvalid maximum number of invalid values (0 or NA) allowed in rows that are used for M value calculation
#' @param ... arguments passed to \code{bplapply()}
#' 
#' @references 
#' \enumerate{
#' \item Vandesompele J. et al (2002) Accurate normalization of real-time quantitative RT-PCR data by geometric averaging of multiple internal control genes. Genome Biol. 3(7):research0034.1, doi: \href{https://dx.doi.org/10.1186%2Fgb-2002-3-7-research0034}{10.1186/gb-2002-3-7-research0034}
#' }
#'
#' @rdname analyzeFT
#' @export
setMethod("FTcalculateM", "MseekFT",
          function(object, intensityCols = NULL, maxInvalid = 0, ...){
            beforeHash <- MseekHash(object)
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
              
              ints <- as.matrix(object$df[,intensityCols])
              
              invalidCounts <- apply(ints, 1, function(r){sum(is.na(r) | r == 0)})
              
              res <- data.frame("M_Value" = rep(NA_real_,nrow(ints)), stringsAsFactors = FALSE)
              
              #input some low, random values
              ints[is.na(ints) | ints == 0] <- runif(sum(is.na(ints) | ints == 0), 
                                                         min = min(ints[!is.na(ints) & ints != 0])/4,
                                                         max = min(ints[!is.na(ints) & ints != 0])/2)
              
              ints <- log2(ints)
              
              res$M_Value[invalidCounts <= maxInvalid] <- .calculateM(ints[invalidCounts <= maxInvalid,], na.rm = FALSE, ...)
              
              object <- updateFeatureTable(object, res)
            },
            error = function(e){
              #this assigns to object err in function environment,
              #but err has to exist in the environment, otherwise
              #will move through scopes up to global environment..
              err$FTcalculateM <<- paste(e)
            },
            finally = {
              p1 <- (proc.time() - p1)["elapsed"]
              afterHash <- MseekHash(object)
              object <- addProcessHistory(object, FTProcessHistory(changes = afterHash != beforeHash,
                                                                   inputHash = beforeHash,
                                                                   outputHash = afterHash,
                                                                   error = err,
                                                                   processingTime = p1,
                                                                   sessionInfo = sessionInfo(),
                                                                   info = "Calculated M values.",
                                                                   param = FunParam(fun = "Metaboseek::FTcalculateM",
                                                                                    args = c(list(
                                                                                      intensityCols = intensityCols,
                                                                                      invalidCounts = invalidCounts),
                                                                                      list(...)
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
#'  
#' @param logNormalized if TRUE, applies log10 to intensity values after normalization
#' @param normalize if TRUE, run normalization
#' @param normalizationFactors normalizationFactors vector with factors to apply to each column for normalization.
#' @param zeroReplacement value to replace zeros with
#' 
#' @rdname analyzeFT
#' @export
setMethod("FTNormalize", "MseekFT",
          function(object,
                   normalize = TRUE,
                   intensityCols = NULL,
                   normalizationFactors = NULL,
                   logNormalized = FALSE,
                   zeroReplacement = NULL){
              beforeHash <- MseekHash(object)
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  if(missing(intensityCols) 
                     || is.null(intensityCols)){
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
                                              raiseZeros =  if(!is.numeric(zeroReplacement)){min(mx[which(!mx==0, arr.ind=T)])}else{zeroReplacement}
                                              )
                  if(normalize){ 
                  mx <- featureTableNormalize(mx,
                                              normalize = normalize,
                                              normalizationFactors = normalizationFactors)
                 
                  
                  if(!is.null(logNormalized) && logNormalized){
                      mx <- featureTableNormalize(mx, log =  "log10")
                  }
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
                  afterHash <- MseekHash(object)
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
                                                               fileNames = character(),
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = "Normalized intensity columns.",
                                                               param = FunParam(fun = "Metaboseek::FTNormalize",
                                                                                args = list(intensityColumns = object$intensities,
                                                                                            logNormalized = logNormalized,
                                                                                            normalize = normalize,
                                                                                            normalizationFactors = normalizationFactors),
                                                                                longArgs = list())
                                              ))
              }
              )
              return(object)
          })

#' @aliases FTNormalizationFactors
#' 
#' @description \code{FTNormalizationFactors}: Calculates normalization factors.
#' See also \code{\link{featureTableNormalize}()}
#' @param normalizeFrom can be an MseekFT object with normalization features or NULL (in which case object itself acts as base for calculation)
#' @param normalizationMethod function to apply to normalization feature intensities
#' @param transformation function to transform normalized intensity values, e.g. 'log10'
#' 
#' @rdname analyzeFT
#' @export
setMethod("FTNormalizationFactors", "MseekFT",
          function(object,
                   normalizeFrom = NULL,
                   normalizationMethod = c("mean", "gm_mean", "no normalization"),
                   transformation = NULL, 
                   zeroReplacement = NULL
                   ){
            beforeHash <- MseekHash(object)
            p1 <- proc.time()
            
            err <- list()
            tryCatch({
              normalizationSources <- "Undefined, likely an error occurred"
                intensityCols <- intensityCols(object)
              
              
              if(!length(intensityCols)){
                stop("no intensityCols defined")
              }
              
              if(!all(intensityCols %in% colnames(object$df))){
                stop("Some expected intensityCols are not in the dataframe")
              }
              
                if(normalizationMethod[1] == "no normalization"){
                  
                  normalizationSources <- list(Source = "No Normalization")
                  
                  object$normalizationFactors <- rep(1, length(intensityCols))
                }else{
                  
                  
                  if(!length(normalizeFrom)){
                    
                    fl <- list()
                    class(fl) <- c("FilterList", class(fl))
                    
                    normalizationSources <- normalizationSources <- list(Source = "Entire Feature Table",
                                                                         Filters = fl)
                   
                    intens <- object$df[,intensityCols]
                    
                    
                  }else{
                    if("FilterList" %in% class(normalizeFrom)){
                      normalizationSubset <- FTFilter(object,
                                                      filters = normalizeFrom)
                      
                      normalizationSources <- list(Source = "Subset of Feature Table",
                                                   Filters = rbindlist(lapply(normalizeFrom, data.frame, stringsAsFactors = FALSE), idcol = "Filter", fill = TRUE),
                                                   Features = normalizationSubset$df[,c("mz", "rt", "comments")])
                      
                      intens <- normalizationSubset$df[,intensityCols]
                      
                    }else if(is.MseekFT(normalizeFrom)){
                      if(length(intensityCols) != length(intensityCols(normalizeFrom))
                         || !all(intensityCols == intensityCols(normalizeFrom))){
                        stop("Normalization Source Table must have the same intensity column names as the currently active Feature Table!")}
                      
                      normalizationSources <- list(Source = "Another Feature Table",
                                                   Name = normalizeFrom$tablename,
                                                   Features = normalizeFrom$df[,c("mz", "rt", "comments")])
                      
                      intens <- normalizeFrom$df[,intensityCols]
                      
                      
                    }else{
                      stop("normalizeFrom must be either of length 0, a FilterList or an MseekFT object.")
                      }
                    
                    
                }
              
                  raiseZeros =  if(!is.numeric(zeroReplacement)){min(unlist(intens)[unlist(intens) != 0])}else{zeroReplacement}

                  intens <- data.frame(lapply(intens, function(d){
                    d[d == 0] <- raiseZeros
                    d
                  }))
                  
                  if(length(transformation)){
                    intens <- data.frame(lapply(intens, get(transformation)))
                  }
                                              
                  
              object$normalizationFactors <- sapply(lapply(intens, na.omit), #####################Throwing out NAs; TODO potentially reconsider this
                                                    get(normalizationMethod[1]))
                  
              object$normalizationFactors <- 1/(object$normalizationFactors/mean(object$normalizationFactors))
                }
              
            },
            error = function(e){
              #this assigns to object err in function environment,
              #but err has to exist in the environment, otherwise
              #will move through scopes up to global environment..
              err$FTNormalizationFactors <<- paste(e)
            },
            finally = {
              p1 <- (proc.time() - p1)["elapsed"]
              afterHash <- MseekHash(object)
              object <- addProcessHistory(object,
                                          FTProcessHistory(changes = afterHash != beforeHash,
                                                           inputHash = beforeHash,
                                                           outputHash = afterHash,
                                                           fileNames = character(),
                                                           error = err,
                                                           sessionInfo = NULL,
                                                           processingTime = p1,
                                                           info = "Updated normalization factors.",
                                                           param = FunParam(fun = "Metaboseek::FTNormalizationFactors",
                                                                            args = list(normalizationMethod = normalizationMethod[1],
                                                                                        zeroReplacement = zeroReplacement,
                                                                                        transformation = transformation),
                                                                            longArgs = list(normalizeFrom = normalizationSources))
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
              beforeHash <- MseekHash(object)
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
                  afterHash <- MseekHash(object)
                  
                  
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
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
#' @param BPPARAM Parallel processing settings, see 
#' \code{\link[BiocParallel]{BiocParallelParam-class}}and 
#' \code{\link[BiocParallel]{bpparam}}
#' @param columnSuffix suffix for new intensity columns generated by this function
#' @inheritParams exIntensities
#' 
#' @description \code{getMseekIntensities}: get EIC-based intensities for each
#'  molecular feature in the MseekFT object for each file in \code{rawdata}.
#'  if another MseekFT object is supplied as \code{importFrom}, will try to transfer MseekIntensities from there if 
#'  all settings, features and MS data files are equivalent. See also \code{\link{exIntensities}}
#' @importFrom BiocParallel SerialParam bpparam bplapply
#' @rdname analyzeFT
#' @export
setMethod("getMseekIntensities", signature(object = "MseekFT",
                                           rawdata = "listOrNULL",
                                           importFrom = "missing"),
          function(object, rawdata, adjustedRT = TRUE, ppm = 5,
                   rtrange = TRUE, rtw = 5,
                   areaMode = FALSE,
                   BPPARAM = SerialParam(),
                   baselineSubtract = TRUE,
                   SN = NULL,
                   columnSuffix = "__XIC"
                   ){
              beforeHash <- MseekHash(object)
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
                  intens <- bplapply(seq_len(length(rawdata)), function(i){
                      
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
                      #need ::: for bpparam, at least if SnowParam
                      Metaboseek:::exIntensities(rawfile= rawdata[[i]],
                                    mz = object$df$mz,
                                    ppm= ppm,
                                    rtw= rtwin,
                                    areaMode = areaMode,
                                    baselineSubtract = baselineSubtract,
                                    SN = SN)
                  },
                  BPPARAM = BPPARAM
                  )
                  
                  names(intens) <- paste0(basename(names(rawdata)), columnSuffix)
                  
                  
                  object <- updateFeatureTable(object, as.data.frame(intens,
                                                      stringsAsFactors = FALSE))
                  
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
                  afterHash <- MseekHash(object)
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
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
                                                                                            areaMode = areaMode,
                                                                                            baselineSubtract = baselineSubtract,
                                                                                            SN = SN,
                                                                                            columnSuffix = columnSuffix),
                                                                                longArgs = list(rawdata = summary(rawdata),
                                                                                                BPPARAM = capture.output(BPPARAM)))
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
                   areaMode = FALSE,
                   BPPARAM = SerialParam(),
                   baselineSubtract = TRUE,
                   SN = NULL,
                   columnSuffix = "__XIC"){
              beforeHash <- MseekHash(object)
              
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
                                                        areaMode = areaMode,
                                                        baselineSubtract = baselineSubtract,
                                                        SN = SN,
                                                        columnSuffix = columnSuffix
                                            ),
                                            longArgs = list(rawdata = summary(rawdata),
                                                            BPPARAM = capture.output(BPPARAM)))
                  
                  
                  oldParams <- searchFunParam(importFrom, "Metaboseek::getMseekIntensities")
                  
                  if(!length(oldParams)){
                      stop("no previous results")
                      
                  }
                  
                  oldParams <- oldParams[!sapply(oldParams, hasError)]
                  
                  if(!length(oldParams)){
                      stop("no previous results")
                      
                  }                  
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
                  
                  object <- updateFeatureTable(object, importFrom$df[,importFrom$MseekIntensities]) 
                  
                  object$MseekIntensities <- unique(c(object$MseekIntensities,
                                                      importFrom$MseekIntensities))
                  
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- MseekHash(object)
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
                                                               fileNames = names(rawdata),
                                                               error = list(),
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = paste0("Added Mseek intensities, safely transferred from ", importFrom$tablename),
                                                               param = this.FunParam
                                              ))
                  
              },
              error = function(e){
                  message(paste("skipped import:",e,"\ntrying to calculate MseekIntensities from scratch now..."))
                  object <<-  getMseekIntensities(object = object, rawdata = rawdata,
                                                  adjustedRT = adjustedRT, ppm = ppm,
                                                  rtrange = rtrange, rtw = rtw,
                                                  areaMode = areaMode,
                                                  baselineSubtract = baselineSubtract,
                                                  SN = SN,
                                                  columnSuffix = columnSuffix,
                                                  BPPARAM = BPPARAM)
                  
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
              beforeHash <- MseekHash(object)
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
                  afterHash <- MseekHash(object)
                  
                  
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
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
              beforeHash <- MseekHash(object)
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
                  afterHash <- MseekHash(object)
                  
                  
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
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
              beforeHash <- MseekHash(object)
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
                  afterHash <- MseekHash(object)
                  
                  
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
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
                   adjmethod = "bonferroni",
                   controlGroup = NULL){
              beforeHash <- MseekHash(object)
              
              
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
                                    adjmethod = adjmethod,
                                    controlGroup = controlGroup)
                  
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
                  afterHash <- MseekHash(object)
                  
                  
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
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
                   grouping = NULL,
                   adjmethod = 'bonferroni'){
              beforeHash <- MseekHash(object)
              
              
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
                                    groups = grouping,
                                    adjmethod = adjmethod)
                  
                  
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
                  afterHash <- MseekHash(object)
                  
                  
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
                                                               fileNames = character(),
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = "Calculated ANOVA.",
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
#' @description \code{FTCluster}: cluster the feature table with cluster::clara()
#'  See also \code{\link{MosCluster}()}
#' @param numClusters number of clusters to group the features in. Will 
#' automatically be set to be at most number of features - 1.
#' 
#' @rdname analyzeFT
#' @export
setMethod("FTCluster", c("MseekFT"),
          function(object, intensityCols = NULL,
                   numClusters = 100L){
              beforeHash <- MseekHash(object)
              
              
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
                  afterHash <- MseekHash(object)
                  
                  
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
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
              beforeHash <- MseekHash(object)
              
              
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
                  afterHash <- MseekHash(object)
                  
                  
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
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

#' @aliases FTMS2scans
#' 
#' @description \code{FTMS2scans}: find MS2 scans across files and save their file and scan numbers in
#'  the MseekFT object as a text column.
#' @param uniqueMatch if TRUE, assign MS2 scans only to the matching feature with the closest rt
#'  
#' @rdname analyzeFT
#' @export
setMethod("FTMS2scans", c("MseekFT", "listOrNULL"),
          function(object, rawdata, ppm = 5, rtw = 10, uniqueMatch = FALSE){
              beforeHash <- MseekHash(object)
              p1 <- proc.time()
              
              err <- list()
              tryCatch({
                  
                  if(is.null(rawdata)){
                      stop("Analysis was not performed because no MS data is loaded.")
                      
                  }
                  
                  inp <- data.frame(MS2scans = listMS2scans(mz = object$df$mz,
                                                            rt = object$df$rt,
                                                            ppm = ppm,
                                                            rtw = rtw,
                                                            MSData = rawdata,
                                                            rtMatch = uniqueMatch),
                                    stringsAsFactors = F)
                  
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
                  afterHash <- MseekHash(object)
                  
                  if(!length(err)){
                      msg <- paste("Found MS2 scans for", sum(object$df$MS2scans != ""), "Features")
                  }else{
                      msg <- "Failed to find MS2 scans"
                      }
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
                                                               fileNames = character(),
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = msg,
                                                               param = FunParam(fun = "Metaboseek::FTMS2scans",
                                                                                args = list(ppm = ppm,
                                                                                            rtw = rtw,
                                                                                            uniqueMatch = uniqueMatch),
                                                                                longArgs = list(rawdata = summary(rawdata)))
                                              ))
              }
              )
              return(object)
          })

#' @noRd
#' @importFrom MassTools mergeMS
setMethod("getSpecList", c("data.frame","list"),
          function(object, rawdata, merge = TRUE,
                   noiselevel = 0, ppm = 5, mzdiff = 0.0005) {
              
              res <- lapply(makeScanlist2(object$MS2scans), getAllScans, rawdata, removeNoise = noiselevel)
              
              if(merge){
                  res <- lapply(res, mergeMS, ppm = ppm, mzdiff = mzdiff, noiselevel = noiselevel)
              }
              
              return(res)
              
          })

#' @rdname analyzeFT
#' @description \code{getSpecList}: generate a list of MS2 spectra inside the object
#' from MS2 spectra that were identified with the \code{FTMS2scans()} method.
#' @param mzThreshold if not NULL, will remove all peaks with an mz below this value from the spectra.
#' @param merge if TRUE, will merge spectra for each molecular feature
#' @param noiselevel noise level to remove as a portion of largest peak in a spectrum
#' @export
setMethod("getSpecList", c("MseekFT","listOrNULL"),
          function(object, rawdata, merge = TRUE, noiselevel = 0, ppm = 5, mzdiff = 0.0005, mzThreshold = NULL){
              beforeHash <- MseekHash(object)
              
              p1 <- proc.time()
              err <- list()
              
              tryCatch({
                  
                  if(missing(rawdata) || is.null(rawdata)){
                      stop("No MS data available")   
                  }
                  
                  if(is.null(object$df$MS2scans)){
                      stop("No MS2scans defined yet. Run FTMS2scans() first")
                  }
                  
                  inp <- data.frame(specList = I(getSpecList(object$df, rawdata,
                                                           merge = merge,
                                                           noiselevel = noiselevel,
                                                           ppm = ppm,
                                                           mzdiff = mzdiff)),
                                    stringsAsFactors = FALSE)
                  
                  if(!is.null(mzThreshold)){
                      
                      inp$specList <- lapply(inp$specList,
                                             function(x){if(is.matrix(x)|is.data.frame(x)){
                                                 return(x[x[,1]>mzThreshold,,drop = FALSE]) }else{
                                                     return(x)
                                                 }})
                      
                      }
                  
                  object <- updateFeatureTable(object, inp)
                  
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$getSpecList <<- paste(e)
                  
              },
              finally = 
              {
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- MseekHash(object)
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
                                                               fileNames = names(rawdata),
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = paste0("Extracted MS2 spectra into specList column"),
                                                               param = FunParam(fun = "Metaboseek::getSpecList",
                                                                                args = list(merge = merge,
                                                                                            noiselevel = noiselevel,
                                                                                            ppm = ppm,
                                                                                            mzdiff = mzdiff),
                                                                                longArgs = list(rawdata = summary(rawdata)))
                                              ))
              })
              
              return(object)
              
          })

#' @rdname analyzeFT
#' @description \code{FTedges}: wrapper for the 
#' \code{MassTools::\link[MassTools]{makeEdges}()} function, matching a list of 
#' MS2 spectra available inside the object and generating similarity scores.
#' 
#' @param useParentMZs if TRUE, will also match neutral losses between spectra
#' @param minpeaks minimum number of peaks that have to match between two spectr
#' to allow calculation of a score
#' @param mzdiff mz tolerance in fragment ion matching
#' 
#' @export
setMethod("FTedges", c("MseekFT"),
          function(object, useParentMZs = TRUE, minpeaks = 6, mzdiff = 0.0005){
              beforeHash <- MseekHash(object)
              
              p1 <- proc.time()
              err <- list()
              
              tryCatch({
                  
                  
                  if(is.null(object$df$specList)){
                      stop("No MS2scans defined yet. Run getSpecList() first")
                  }
                  
                  #this will be in sync with the edge indices and is used by the NetworkingModule for node ID
                  object <- updateFeatureTable(object,data.frame(fixed__id = seq(nrow(object$df))))
                  
                  
                  object$edges <- MassTools::makeEdges(speclist = object$df$specList,
                                                       parentmasses = if(useParentMZs){object$df$mz}else{NULL},
                                                       minpeaks = minpeaks,
                                                       mztol = mzdiff)
                  
                  object$edges <- object$edges[object$edges$cosine > 0.001,, drop = FALSE]
                  
                  
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$getSpecList <<- paste(e)
                  
              },
              finally = {
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- MseekHash(object)
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = paste0("Generated networking edges"),
                                                               param = FunParam(fun = "Metaboseek::FTedges",
                                                                                args = list(useParentMZs = useParentMZs,
                                                                                            minpeaks = minpeaks,
                                                                                            mzdiff = mzdiff),
                                                                                longArgs = list())
                                              ))
              })
              
              return(object)
              
          })



#' @noRd
setMethod("matchReference", c("data.frame","data.frame"),
          function(object, query , parent_mztol = 0.001, parent_ppm = 5,
                   rttol = 5, getCosine = TRUE, cosineThreshold = NULL,
                   singleHits = TRUE, queryPrefix = "query__",
                   returnMapping = FALSE,
                   ...) {
              
              if((!length(query$specList) 
                 || !length(object$specList))
                 && getCosine){
                  stop("Need specList to getCosine. Add specLists to both objects or run with getCosine = FALSE")
              }
              
              #prepare to take up information about which ref matches each query item
              hitlist <- lapply(!logical(nrow(query)), rep, nrow(object))
              
              if(length(query$rt) 
                 && length(object$rt)
                 && length(rttol)){
                  
                  hitlist <- lapply(seq_len(length(hitlist)),function(n){(hitlist[[n]] 
                                                                          & abs(query$rt[n] - object$rt) < rttol)})
                  
              }
              
              if(length(query$mz) 
                 && length(object$mz)
                 && length(parent_mztol)
                 && length(parent_ppm)){
                  
                  
                  hitlist <- lapply(seq_len(length(hitlist)),function(n){(hitlist[[n]] 
                                                                          & (abs(query$mz[n] - object$mz) < parent_mztol
                                                                             | abs(query$mz[n] - object$mz)/query$mz[n] < parent_ppm*1e-6))})
                  
              }
              
              if(getCosine){
                  #make edges... from, to, cosine...
                  mapping <- do.call(rbind,lapply(seq_len(length(hitlist)),function(n){
                      if(!any(hitlist[[n]])){return(numeric())}
                      from <- rep(n, sum(hitlist[[n]]))
                      to <- which(hitlist[[n]])
                      cosine <- sapply(object$specList[hitlist[[n]]],MassTools::network1, query$specList[[n]], ...)
                      
                      matrix(c(from,to,cosine), ncol = 3, dimnames = list(rownames = NULL,
                                                                          colnames = c('query', 'ref', 'cosine')), byrow = FALSE)}))
              }else{
                  mapping <-  do.call(rbind,lapply(seq_len(length(hitlist)),function(n){
                      if(!any(hitlist[[n]])){return(numeric())}
                      from <- rep(n, sum(hitlist[[n]]))
                      to <- which(hitlist[[n]])
                      
                      matrix(c(from,to), ncol = 2, dimnames = list(rownames = NULL,
                                                                   colnames = c('query', 'ref')), byrow = FALSE)}))
                  
              }
              
              
              
              if(getCosine && length(cosineThreshold)){
                  mapping <- mapping[which(mapping[,'cosine'] > cosineThreshold),,drop = FALSE] #which gets rid of NAs
              }
              
              if(singleHits){
                  if(getCosine){
                      mapping <- mapping[order(mapping[,'cosine'], decreasing = TRUE),,drop = FALSE]
                  }
                  
                  mapping <- mapping[!duplicated(mapping[,'ref']),,drop = FALSE]
                  
              }
              
              addref <- seq_len(nrow(object))[!seq_len(nrow(object)) %in% mapping[,2]]
              
              #fill mapping for object entries that don't have a match
              if(getCosine){
                  filler <- matrix(c(rep(NA_real_, length(addref)),
                                     addref,
                                     rep(NA_real_, length(addref))),
                                   ncol = 3, dimnames = list(rownames = NULL,
                                                             colnames = c('query', 'ref', 'cosine')), byrow = FALSE)
              }else{
                  filler <- matrix(c(rep(NA_real_, length(addref)),
                                     addref),
                                   ncol = 2, dimnames = list(rownames = NULL,
                                                             colnames = c('query', 'ref')), byrow = FALSE)
                  
              }
              
              mapping <- rbind(mapping, filler)
              mapping <- mapping[order(mapping[,2],decreasing = FALSE),,drop = FALSE]
              
              
              if(returnMapping){return(mapping)}
              
              
              
              colnames(query) <- paste0(queryPrefix, colnames(query))
              
                  matched <- cbind(object[mapping[,2],], query[mapping[,1],])
                  if(ncol(mapping) >2){
                  matched[[paste0(queryPrefix,"matchscore")]] <- mapping[,3]
              }
              
              return(matched)
              
          })


#' @rdname analyzeFT
#' @param merge if TRUE, will merge spectra for each molecular feature
#' @param query an object that contains molecular features that will be matched to
#' \code{object}, by a customizable combination of retention time, m/z and MS2 similarity matching
#' @param parent_mztol parent m/z matching tolerance (absolute); matches have to differ 
#' by less than either \code{parent_mztol} or \code{parent_ppm}. If NULL, will ignore m/z for matching.
#' @param parent_ppm parent m/z matching tolerance in ppm; matches have to differ 
#' by less than either \code{parent_mztol} or \code{parent_ppm}.
#' @param rttol retention time tolerance in seconds. If NULL, will ignore rt for matching
#' @param getCosine if TRUE, will calculate MS2 scan similarity for features that
#' match by rt and m/z (or between all features if rttol and parent_mztol are not set)
#' @param cosineThreshold minimum cosine value between features for them to be considered matches.
#' will not filter for MS2 similarity score if NULL.
#' @param singleHits allow only one query hit for each reference molecular feature 
#' (will pick the one with best MS2 similarity)
#' @param queryPrefix prefix for columns transferred from the matched query object
#' @param returnMapping if true, returns a matrix defining the indices of matched features between object and query
#' @param ... additional arguments passed to internal methods (e. g. \code{\link[MassTools]{network1}()})
#' 
#' @description \code{matchReference}: Match molecular features between a 
#' \code{MseekGraph} or \code{MseekFT} object and another \code{MseekFT} object
#'  by a customizable combination of retention time, m/z and MS2 similarity
#'   matching
#' 
#' @export
setMethod("matchReference", c("MseekFT","MseekFT"),
          function(object, query , parent_mztol = 0.001, parent_ppm = 5,
                   rttol = 5, getCosine = TRUE, cosineThreshold = NULL,
                   singleHits = TRUE, queryPrefix = "query__",
                   returnMapping = FALSE,
                   ...) {
              beforeHash <- MseekHash(object)
              
              p1 <- proc.time()
              err <- list()
              
              tryCatch({
                  
                  
                  if(!is.null(cosineThreshold) 
                     && (!is.list(object$df$specList)
                         || !is.list(query$df$specList))){
                      stop("No specList found in query and/or object. Run getSpecList() first or set cosineThreshold to NULL to match only based on rt and mz values.")
                  }
                  
                  inp <- matchReference(object$df,
                                        query$df,
                                        parent_mztol = parent_mztol,
                                        parent_ppm = parent_ppm,
                                        rttol = rttol,
                                        getCosine = getCosine,
                                        cosineThreshold = cosineThreshold,
                                        singleHits = singleHits,
                                        queryPrefix = queryPrefix,
                                        returnMapping = returnMapping,
                                        ...)
                  
                  object <- updateFeatureTable(object, inp)
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$matchReference <<- paste(e)
                  
              },
              finally = 
              {
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- MseekHash(object)
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
                                                               # fileNames = names(rawdata),
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = paste0("Matched query MseekFT object ", MseekHash(query), " to this reference object"),
                                                               param = FunParam(fun = "Metaboseek::matchReference",
                                                                                args = c(list(...),
                                                                                         list(parent_mztol = parent_mztol,
                                                                                              parent_ppm = parent_ppm,
                                                                                              rttol = rttol,
                                                                                              getCosine = getCosine,
                                                                                              cosineThreshold = cosineThreshold,
                                                                                              singleHits = singleHits,
                                                                                              queryPrefix = queryPrefix,
                                                                                              returnMapping = returnMapping)),
                                                                                longArgs = list(queryHistory = processHistory(query)))
                                              ))
              })
              
              return(object)
              
          })

#' @rdname analyzeFT
#' @param merge if TRUE, will merge spectra for each molecular feature
#' @export
setMethod("matchReference", c("MseekGraph","MseekFT"),
          function(object, query , parent_mztol = 0.001, parent_ppm = 5,
                   rttol = 5, getCosine = TRUE, cosineThreshold = NULL,
                   singleHits = TRUE, queryPrefix = "query__",
                   returnMapping = FALSE,
                   ...) {
              beforeHash <- MseekHash(object)
              
              p1 <- proc.time()
              err <- list()
              
              tryCatch({
                  
                  
                  if(!is.null(cosineThreshold) 
                     && (!is.list(V(object$graph)$specList)
                         || !is.list(query$df$specList))){
                      stop("No specList found in query and/or object. Run getSpecList() first or set cosineThreshold to NULL to match only based on rt and mz values.")
                  }
                  
                  for (rem in vertex_attr_names(object$graph)[grepl(paste0("^",queryPrefix), vertex_attr_names(object$graph))]){
                      
                      object$graph <- delete_vertex_attr(object$graph, rem)
                      
                      
                  }
                  
                  
                  inp <- matchReference(type.convert(as_data_frame(object$graph, "vertices"), as.is = T),
                                        query$df,
                                        parent_mztol = parent_mztol,
                                        parent_ppm = parent_ppm,
                                        rttol = rttol,
                                        getCosine = getCosine,
                                        cosineThreshold = cosineThreshold,
                                        singleHits = singleHits,
                                        queryPrefix = queryPrefix,
                                        returnMapping = returnMapping,
                                        ...)
                  
                  for (add in colnames(inp)[grepl(paste0("^",queryPrefix), colnames(inp))]){
                      
                      vertex_attr(object$graph, add) <- inp[[add]]
                      
                      
                  }
                  
                  #vertex_attr(object$graph) <- as.list(inp[,grepl(paste0("^",queryPrefix), colnames(inp)), drop = FALSE])
                  
                  
                  
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$matchReference <<- paste(e)
                  
              },
              finally = 
              {
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- MseekHash(object)
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputHash = beforeHash,
                                                               outputHash = afterHash,
                                                               # fileNames = names(rawdata),
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = paste0("Matched query MseekFT object ", MseekHash(query), " to this reference MseekGraph object"),
                                                               param = FunParam(fun = "Metaboseek::matchReference",
                                                                                args = c(list(...),
                                                                                         list(parent_mztol = parent_mztol,
                                                                                              parent_ppm = parent_ppm,
                                                                                              rttol = rttol,
                                                                                              getCosine = getCosine,
                                                                                              cosineThreshold = cosineThreshold,
                                                                                              singleHits = singleHits,
                                                                                              queryPrefix = queryPrefix,
                                                                                              returnMapping = returnMapping)),
                                                                                longArgs = list(queryHistory = processHistory(query)))
                                              ))
              })
              
              return(object)
              
          })

#' @aliases LabelFinder
#' 
#' @description \code{LabelFinder}: Find labeled features, see \code{\link{findLabels}()}
#' @param newName name for the LabelFinder result object
#' @param object2 Feature Table to compare to (with targets expected to carry a label)
#' 
#' @examples 
#' \dontrun{
#' MseekExamplePreload(data = TRUE, tables = TRUE)
#' LabelFinderResults <- LabelFinder(object = tab2, #remove intensity columns to have them replaced with new ones from rawdata
#'                                 object2 = tab2,
#'                                 newName = "Test",
#'                                 MSData = MSD$data,
#'                                 ref_intensityCols = tab2$intensities[1:3],
#'                                 comp_intensityCols = tab2$intensities[4:7],
#'                                 labelmz = 2*1.00335,
#'                                 ifoldS1 = 10,
#'                                 ifoldS2 = 10000)
#' }
#' 
#' @rdname analyzeFT
#' @export
setMethod("LabelFinder", signature(object = "MseekFamily"),
          function(object, object2, MSData, newName, ...){
              beforeHash <- MseekHash(object)
              p1 <- proc.time()
              err <- list()
              tryCatch({
                  
             
                  object$df <- object$df[,colnames(object$df) %in% c("rt", "mz", "rtmin","rtmax", "comments")]
                  object2$df <- object2$df[,colnames(object2$df) %in% c("rt", "mz", "rtmin","rtmax", "comments")]
                  
                  
                  object <- buildMseekFT(findLabels(reflist = object$df,
                                                    complist = object2$df,
                                                    rawdata = MSData,
                                                    ...),
                                         processHistory = object$.processHistory,
                                         tablename = newName,
                                         editable = FALSE)
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$LabelFinder <<- paste(e)
              },
              finally = {
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- MseekHash(object)
                  object <- addProcessHistory(object, FTProcessHistory(changes = afterHash != beforeHash,
                                                                       inputHash = beforeHash,
                                                                       outputHash = afterHash,
                                                                       error = err,
                                                                       processingTime = p1,
                                                                       sessionInfo = NULL,
                                                                       info = "Tried to find labels",
                                                                       param = FunParam(fun = "Metaboseek::LabelFinder",
                                                                                        args = c(list(...),
                                                                                                 list(
                                                                                                     newName = newName
                                                                                                 )),
                                                                                        longArgs = list(object2 = MseekHash(object2),
                                                                                                        MSData = summary(MSData)))
                                                                       
                  ))
              })
              return(object)
          })

#' @aliases PatternFinder
#' 
#' @description \code{PatternFinder}: Find Pattern in Spectra
#' @param peaks names list of mz values (like output from \code{parsePatterns()}) to look for in spectra
#' @param losses names list of mz values (like output from \code{parsePatterns()}) to look for in spectra (as neutral losses)
#' @param noise remove peaks below this relative intensity when merging spectra (relative to highest peak, not percent)
#' 
#' @examples 
#' \dontrun{
#' MseekExamplePreload(data = TRUE, tables = TRUE)
#' tab1 <- FTMS2scans(tab1, MSD$data)
#' LabelFinderResults <- PatternFinder(object = tab1, #needs to have an MS2
#'                                 MSData = MSD$data,
#'                                 peaks = list(testpeak = 85.02895),
#'                                 losses = list(testloss = 18.010788))
#' LabelFinderResults$df$matched_losses
#' LabelFinderResults$df$matched_patterns
#' }
#' 
#' @rdname analyzeFT
#' @export
setMethod("PatternFinder", signature(object = "MseekFamily"),
          function(object, MSData,
                   peaks, losses,
                   ppm = 5, mzdiff = 0.002,
                   noise = 0.02){
            beforeHash <- MseekHash(object)
            p1 <- proc.time()
            err <- list()
            tryCatch({
              
              
              if(!length(object$df$MS2scans)){
                stop('Run the "Find MS2 scans" process (FTMS2scans method) before searching patterns in these scans!')
                
              }
              
              AllSpecLists <- lapply(makeScanlist2(object$df$MS2scans),
                                     getAllScans, MSData,
                                     removeNoise = NULL)#input$noise*0.01)
              
              

              MergedSpecs <- lapply(AllSpecLists, mergeMS, ppm = ppm, mzdiff = 0, noiselevel = noise)
              
                if(length(peaks)){
                matchedPatterns <- data.frame(matched_patterns = matchedToCharacter(findPatterns(MergedSpecs,
                                                                                                 peaks,
                                                                                                 ppm = ppm,
                                                                                                 mzdiff = mzdiff)), 
                                              stringsAsFactors = FALSE)
                
                object <- updateFeatureTable(object,matchedPatterns)
              }
              
              if(length(losses)){
                MergedSpecs[lengths(MergedSpecs) > 0] <- mapply(function(x,y){
                  x[,1] <- y - x[,1]
                  x <- x[rev(seq_len(nrow(x))),, drop = FALSE] #because input is increasing, this will make output increasing (maybe faster than order()?)
                  return(x[x[,1] > 0,, drop = FALSE]) #remove negative mz values
                }, 
                x = MergedSpecs[lengths(MergedSpecs) > 0],
                y = object$df$mz[lengths(MergedSpecs) > 0],
                SIMPLIFY = FALSE)
                matchedPatterns <- data.frame(matched_losses = matchedToCharacter(findPatterns(MergedSpecs,
                                                                                               losses,
                                                                                               ppm = ppm,
                                                                                               mzdiff = mzdiff)), 
                                              stringsAsFactors = FALSE)
                object <- updateFeatureTable(object,matchedPatterns)
              }
              
            },
            error = function(e){
              #this assigns to object err in function environment,
              #but err has to exist in the environment, otherwise
              #will move through scopes up to global environment..
              err$PatternFinder <<- paste(e)
            },
            finally = {
              p1 <- (proc.time() - p1)["elapsed"]
              afterHash <- MseekHash(object)
              object <- addProcessHistory(object, FTProcessHistory(changes = afterHash != beforeHash,
                                                                   inputHash = beforeHash,
                                                                   outputHash = afterHash,
                                                                   error = err,
                                                                   processingTime = p1,
                                                                   sessionInfo = NULL,
                                                                   info = "Found Patterns in MS2 data",
                                                                   param = FunParam(fun = "Metaboseek::PatternFinder",
                                                                                    args = c(#list(...),
                                                                                             list(
                                                                                               peaks = peaks,
                                                                                               losses = losses,
                                                                                               ppm = ppm,
                                                                                               mzdiff = mzdiff
                                                                                             )),
                                                                                    longArgs = list(MSData = summary(MSData)))
                                                                   
              ))
            })
            return(object)
          })