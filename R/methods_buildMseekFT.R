#' @include Classes.R

#' @title buildMseekFT
#' @aliases buildMseekFT
#' @name buildMseekFT
#' 
#' @description Methods to build an \code{MseekFT} object from an xsAnnotate,
#'  xcmsSet or data.frame object, and to load or save \code{MseekFT} objects
#'
#' @param object an xsAnnotate, xcmsSet or data.frame object. For \code{saveMseekFT},
#'  an \code{MseekFT} object, for \code{loadMseekFT} a charcater vector of file paths
#' @param processHistory a list of \code{\link[xcms]{processHistory}} objects,
#' will override the processHistory list that may be supplied by \code{object}
#' @param ... additional arguments passed to \code{\link{constructFeatureTable}}
#' 
#' @return an object of class \code{MseekFT}, with processHistory extended from
#'  \code{object} if applicable. 
#'  
#' @importClassesFrom CAMERA xsAnnotate
#' @importClassesFrom xcms XCMSnExp xcmsSet
#' @importFrom xcms processHistory
#' @rdname buildMseekFT
#' @export
setMethod("buildMseekFT",  "xsAnnotate",
          function(object, processHistory = list(), ...){
              
              oldHistory <- if(length(processHistory)){processHistory}else{processHistory(object)}
              
              res <- constructFeatureTable(df = CAMERA::getPeaklist(object),
                                           processHistory = c(oldHistory),
                                           ...)
              
              
              
              res$sampleNames <- object@xcmsSet@phenoData$sampleNames
              res$RTcorr <- rtexport(object)
              res$RTcorrected <- !identical(res$RTcorr$corr, res$RTcorr$noncorr)
              
              res <- addProcessHistory(res, FTProcessHistory(info = "Extracted Feature Table from CAMERA::xsAnnotate using CAMERA::getPeaklist and built MseekFT object",
                                                             sessionInfo = sessionInfo(),
                                                             outputHash = MseekHash(res),
                                                             param = FunParam(fun = "Metaboseek::buildMseekFT",
                                                                              args = list(...))))
              
              return(res)
              
          })

#' @rdname buildMseekFT
#' @export
setMethod("buildMseekFT", 
          "XCMSnExp",
          function(object, processHistory = list(), ...){
              
              oldHistory <- if(length(processHistory)){processHistory}else{processHistory(object)}
              
              
              res <- constructFeatureTable(df = xcms::peakTable(as(object,"xcmsSet")),
                                           processHistory = c(oldHistory),
                                           ...)
              
              
              
              res$sampleNames <- as.character(object@phenoData@data$sampleNames)
              res$RTcorr <- rtexport(object)
              res$RTcorrected <- !identical(res$RTcorr$corr, res$RTcorr$noncorr)
              
              res <- addProcessHistory(res, FTProcessHistory(info = "Extracted Feature Table from xcms::XCMSnExp using xcms::peakTable(as(xset,'xcmsSet')) and built MseekFT object",
                                                             sessionInfo = sessionInfo(),
                                                             outputHash = MseekHash(res),
                                                             param = FunParam(fun = "Metaboseek::buildMseekFT",
                                                                              args = list(...))))
              return(res)
          })

#' @rdname buildMseekFT
#' @export
setMethod("buildMseekFT", 
          signature(object = "data.frame"),
          function(object, processHistory = list(), ...){
              
              res <- constructFeatureTable(df = object,
                                           processHistory = processHistory,
                                           ...)
              
              res <- addProcessHistory(res, FTProcessHistory(info = "Built MseekFT object from a data.frame.",
                                                             sessionInfo = sessionInfo(),
                                                             outputHash = MseekHash(res),
                                                             param = FunParam(fun = "Metaboseek::buildMseekFT",
                                                                              args = list(...))))
              
              return(res)
              
          })




#' @aliases loadMseekFT
#' 
#' @description \code{loadMseekFT}: load a \code{MseekFT} object from an .mskFT file
#' @rdname buildMseekFT
#' @export
setMethod("loadMseekFT", 
          signature(object = "character"),
          function(object){
              
              if(length(object)>1){
                  lapply(object, loadMseekFT)
              }else{
                  
                  res <- readRDS(object)
                  
                  res <- addProcessHistory(res, FTProcessHistory(info = paste0("Loaded MseekFT object from file: ", object),
                                                                 sessionInfo = sessionInfo(),
                                                                 outputHash = MseekHash(res),
                                                                 param = FunParam(fun = "Metaboseek::loadMseekFT",
                                                                                  args = list(file = object))))
                  
                  return(res)
              }
          })

#' @title saveMseekFT
#' @aliases saveMseekFT
#' 
#' @description \code{saveMseekFT}: save a \code{MseekFT} object to a file, registering the save event in the processHistory 
#'
#' @param file file path to write to
#' @param writeCSV write object to a .csv file (data frame only)
#' @param writeRDS write object to an .mskFT (RDS) file; saves the entire object
#' 
#' @return the \code{MseekFT} object, with saving event added to processHistory
#'   
#' @rdname buildMseekFT
#' @export
setMethod("saveMseekFT", 
          "MseekFT",
          function(object, file, writeCSV = FALSE, writeRDS = TRUE){
              
              object <- addProcessHistory(object, FTProcessHistory(info = paste0("Saved MseekFT object to file:", file),
                                                                   inputHash = MseekHash(object),
                                                                         param = FunParam(fun = "Metaboseek::saveMseekFT",
                                                                                          args = list(file = file,
                                                                                                      writeCSV = writeCSV,
                                                                                                      writeRDS = writeRDS))))
              #make sure file extension is .mskFT
              if(writeCSV){
                  tableWriter(object$df,
                              fname = paste0(gsub('\\.csv$','',file),".csv"),
                              format = "csv")
              }
              
              
              #make sure file extension is .mskFT
              if(writeRDS){
                  saveRDS(object, file = paste0(gsub('\\.mskFT$','',file),".mskFT"))
              }
              
              
              invisible(object)
          })



