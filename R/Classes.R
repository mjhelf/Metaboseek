## Generics
setGeneric("analyzeFT", function(object, MSData, param) standardGeneric("analyzeFT"))
setGeneric("addProcessHistory", function(object, ...) standardGeneric("addProcessHistory"))
setGeneric("buildMseekFT", function(object, ...) standardGeneric("buildMseekFT"))
setGeneric("error", function(object) standardGeneric("error"))
setGeneric("FTNormalize", function(object, fun, ...) standardGeneric("FTNormalize"))
setGeneric("getMseekIntensities", function(object, rawdata, ...) standardGeneric("getMseekIntensities"))
setGeneric("hasError", function(object) standardGeneric("hasError"))
setGeneric("loadMseekFT", function(object) standardGeneric("loadMseekFT"))
setGeneric("removeNAs", function(object, ...) standardGeneric("removeNAs"))
setGeneric("rename", function(object, ...) standardGeneric("rename"))
setGeneric("saveMseekFT", function(object, file, ...) standardGeneric("saveMseekFT"))
#setGeneric("processHistory", function(object, ...) standardGeneric("processHistory"))
setGeneric("withHistory", function(object, fun, ...) standardGeneric("withHistory"))


## Registered S3 classes
setOldClass("MseekFT")
setOldClass("sessionInfo")

## Class unions
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("sessionInfoOrNULL", c("sessionInfo", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
#' FTAnalysisParam
#' @aliases FTAnalysisParam-class
#' 
#' @title Parameter class for Feature Table analysis
#' 
#' @description Objects of this class hold parameters for feature table analysis,
#' to be used with \code{\link{analyzeFT}()}
#' 
#' @slot intensities the intensity column names, before normalization 
#' (without __norm suffix), will be automatically renamed if useNormalized.
#' @slot groups named list of non-normalized intensity columns listed by group 
#' (as supplied by $anagroupnames of MseekFT objects), will be automatically 
#' renamed if useNormalized.
#' @slot analyze character vector to select the analyses to be run: 
#' "Basic analysis", "clara_cluster", "t-test", "Peak shapes"
#' @slot normalize normalze intensity columns
#' @slot useNormalized use normalized values for analyses; will trigger 
#' normalize if there is no normalized data available for all selected 
#' intensity columns
#' @slot logNormalized if TRUE, applies a log10 to intensity values after normalization
#' @slot .files character() with file paths to the MS data files to be used
#'  in an analysis
#' @slot ppm ppm range for peak shape analysis
#' @slot controlGroup control group for foldChange (part of Basic analysis) 
#' analysis (optional) 
#' @slot numClusters number of clusters for clara_clusters analysis
#' @slot mzMatchParam list of parameters passed to mass
#' @slot workers number of workers to use for multithreaded analyses
#' 
#' @rdname FTAnalysisParam-class
setClass("FTAnalysisParam",
         slots = c(intensities = "character",
                   groups = "list",
                   .files = "character",
                   analyze = "character", 
                   normalize = "logical",
                   useNormalized = "logical",
                   logNormalized = "logical",
                   ppm = "numeric",
                   controlGroup = "characterOrNULL",
                   numClusters = "numeric",
                   mzMatchParam = "list",
                   workers = "numeric"),
         contains = "Param",
         prototype = prototype(
             #df = data.frame(), 
             intensities = character(),
             groups = list(),
             .files = character(),
             analyze = c("Basic analysis", "clara_cluster",
                         "t-test", "Peak shapes",
                         "Fast peak shapes", "PCA features",
                         "PCA samples", "mzMatch"), 
             normalize = T,
             useNormalized = T,
             logNormalized = F,
             #MSData = NULL,
             ppm = 5,
             controlGroup = NULL,
             numClusters = 2,
             mzMatchParam = list(db = "smid-db_pos.csv",
                                 ppm = 5,
                                 mzdiff = 0.001),
             workers = 1
         ),
         validity = function(object) {
             TRUE
             # msg <- character()
             #     if (!length(object@intensities) > 0)
             #         msg <- c(msg, paste0("No intensity columns specified!"))
             #     if (length(msg)) msg
             #     else TRUE
         }
)

#' FunParam
#' @aliases FTAnalysisParam-class
#' 
#' @title Parameter class for Feature Table analysis
#' 
#' @description Objects of this class hold parameters for any function call.
#' 
#' @slot fun function name
#' @slot args arguments to fun, as supplied to fun.
#' 
#' @slot longArgs parameters fed to the function that are excessively large 
#' should be summarized using \code{summary()} and then listed in this slot.
#' 
#' @importClassesFrom xcms GenericParam
#' @rdname FunParam-class
setClass("FunParam",
         slots = c(fun = "character",
             args = "list",
             longArgs = "list"
                   ),
         contains = "GenericParam",
         prototype = prototype(
             fun = character(),
             longArgs = list(),
             args = list()
         ),
         validity = function(object) {
             msg <- character()
              
             if (!length(object@fun) && !length(object@args)){
                 msg <- c(msg, paste0("No function specified!"))}    
             
             if (!length(object@args) && !length(object@args)){
                 msg <- c(msg, paste0("No arguments for function specified!"))}
             
             if (length(object@args) != length(grep("^$",names(object@args), invert = TRUE))){
                 msg <- c(msg, paste0("args is not a named list"))}
             
             if (length(object@longArgs) != length(grep("^$",names(object@longArgs), invert = TRUE))){
                 msg <- c(msg, paste0("longArgs is not a named list"))}    
             
             if (length(msg)){ msg}else{TRUE}
         }
)

#' FTProcessHistory
#' @aliases FTProcessHistory-class
#' 
#' @title Parameter class for Feature Table analysis
#' 
#' @description Objects of this class hold parameters for feature table analysis,
#' to be used with \code{\link{analyzeFT}()}
#' 
#' @slot error a named list of character() vectors, reporting errors that 
#' occured in this analysis step 
#' @slot changes did changes occur on the associated object. If FALSE, this step
#'  did not result in relevant changes and could be dropped for reporting
#' @slot inputDFhash character digest of the data.frame object before this analysis step
#' @slot outputDFhash character digest of the data.frame object after this analysis step
#' @slot sessionInfo a \code{\link[utils]{sessionInfo}} object, should be genereated
#'  at time of the recorded event and at least once in every session (by default,
#'  will be populated by load and constructor methods for MseekFT class).
#' @param fileNames names of files used in this analysis step (a more human-readable 
#' variant of the fileIndex slot)
#' 
#' @rdname FTProcessHistory-class
setClass("FTProcessHistory",
         slots = c(error = "listOrNULL",
                   changes = "logical",
                   fileNames = "characterOrNULL",
                   inputDFhash = "characterOrNULL",
                   outputDFhash = "characterOrNULL",
                   sessionInfo = "sessionInfoOrNULL"),
         contains = "XProcessHistory",
         prototype = prototype(
             error = list(),
             fileNames = character(),
             changes = FALSE,
             inputDFhash = digest::digest(data.frame(stringsAsFactors = FALSE),
                                  algo = "xxhash64"),
             outputDFhash = digest::digest(data.frame(stringsAsFactors = FALSE),
                                           algo = "xxhash64"),
             sessionInfo = NULL
         ),
         validity = function(object) {
             msg <- character()
             if (length(object@param) > 0)
                 if (!is(object@param, "Param"))
                     msg <- c(msg,
                              paste0("Only objects from type 'Param' ",
                                     "allowed in slot '@param'! I got ",
                                     class(object@param)))
                 if (!is.logical(object@changes))
                     msg <- c(msg, "noChange has to be logical")
              if (length(msg)) msg
              else TRUE
         })





setMethod("initialize", "FTAnalysisParam", function(.Object, ...) {
    Biobase::classVersion(.Object)["FTAnalysisParam"] <- "0.0.1"
    callNextMethod(.Object, ...)
})

#' @title analyzeFT
#' @aliases analyzeFT
#' 
#' @description analyze Tables using \code{\link{analyzeTable}()},
#'  recording processing history if \code{object} is of class \code{MseekFT}.
#'  Will use intensity columns and grouping information from the MseekFT object
#'  if available
#'
#' @param object an MseekFT or data.frame object.
#' @param MSData list of xcmsRaw objects
#' @param param a \code{\link{FTAnalysisParam}} object
#' 
#' @return an object of the same class as \code{object}, with analyses performed as
#' defined by \code{param} 
#'   
#' @rdname analyzeFT
setMethod("analyzeFT", 
          signature(object = "data.frame", MSData = "listOrNULL", param = "FTAnalysisParam"),
          function(object, MSData, param){
              params <- xcms:::.param2list(param)
              do.call(Metaboseek:::analyzeTable,
                      c(list(df = object, MSData = MSData),
                        params))
              })
          

#' @aliases analyzeFT
#'
#' @rdname analyzeFT
setMethod("analyzeFT", 
          signature(object = "MseekFT",
                    MSData = "listOrNULL",
                    param = "FTAnalysisParam"),
          function(object, MSData, param){
              
              param@intensities <- object$intensities
              param@groups <- object$anagroupnames
              
              inputHash <- digest::digest(object$df, algo = "xxhash64")
              
              res <- analyzeFT(object = object$df, MSData = MSData,
                               param = param)
              
              #check if this analysis yielded any changes in the data.frame
              changes <- (!identical(res$df,object$df) 
                          || !identical(res$PCA_samples,
                                       object$anagrouptable))
              
              object <- updateFeatureTable(object,res$df)
              object$anagrouptable <- updateDF(res$PCA_samples,
                                               object$anagrouptable)
              
              if(is.null(object$.processHistory)){
                  
                  object$.processHistory <- list()
                      
                  }
              
              object$.processHistory <- c(object$.processHistory,
                                          FTProcessHistory(param = param,
                                                           changes = changes,
                                                           error = res$errmsg,
                                                           info = paste(param@analyze, collapse = "|")[1],
                                                           inputDFhash = inputHash,
                                                           outputDFhash = digest::digest(object$df,
                                                                                         algo = "xxhash64")
                                                           ))

              return(object)
          })

setMethod("initialize", "FTProcessHistory", function(.Object, ...) {
    Biobase::classVersion(.Object)["FTProcessHistory"] <- "0.0.1"
    callNextMethod(.Object, ...)
})

setMethod("initialize", "FunParam", function(.Object, ...) {
    Biobase::classVersion(.Object)["FunParam"] <- "0.0.1"
    callNextMethod(.Object, ...)
})

#' @rdname FTProcessHistory-class
setMethod("show", "FTProcessHistory", function(object) {
    callNextMethod()
    erLabel <- if(length(object@error)){""}else{"-none-"}
    cat(" errors:", erLabel, "\n")
    if (length(object@error) > 0) {
        for (i in 1:length(object@error)) {
            if (!is.null(names(object@error)))
                cat(" ", names(object@error)[i], "= ")
            cat(object@error[[i]], "\n")
        }
    }
    chLabel <- if(object@changes){"yes"}else{"no"}
    
    cat(" changes:", chLabel, "\n")
    cat(" input data.frame hash:", object@inputDFhash, "\n")
    cat(" output data.frame hash:", object@outputDFhash, "\n")
    cat(" contains sessionInfo:", !is.null(object@sessionInfo), "\n")
})

setMethod("show", "XProcessHistory", function(object) {
    callNextMethod()
    pcLabel <- "-none-"
    if (length(object@param)){
        pcLabel <- class(object@param)}
    cat(" Parameter class:", pcLabel, "\n")
    if (length(object@param)){
        show(object@param)}
    if (!is.na(msLevel(object)))
        cat(" MS level(s)", paste(msLevel(object), sep = " "), "\n")
    
})

#' @rdname FunParam
setMethod("show", "FunParam", function(object) {
    callNextMethod()
    cat(" long (summarized) arguments:\n")
    if (length(object@longArgs) > 0) {
        for (i in 1:length(object@longArgs)) {
            cat(" ", names(object@longArgs)[i], "= ")
            if (is.atomic(object@longArgs[[i]]) && !length(names(object@longArgs[[i]]))){
                cat(object@longArgs[[i]], "\n")
                }else{
                cat("\n")    
                print(object@longArgs[[i]]); cat("\n")    
                }
            
            
        }
    }
})

#' @description
#' 
#' Copied description and Method template from \code{xcms}.
#' 
#' \code{addProcessHistory}: adds (appends) a single
#' \code{\link{ProcessHistory}} object to the \code{.processHistory} slot.
#'
#' @return
#'
#' The \code{addProcessHistory} method returns the input object with the
#' provided \code{\link{ProcessHistory}} appended to the process history.
#'
#' @author Johannes Rainer, Maximilian Helf
#'
#' @noRd
setMethod("addProcessHistory", "MseekFT", function(object, ph) {
    if (!inherits(ph, "ProcessHistory"))
        stop("Argument 'ph' has to be of type 'ProcessHistory' or a class ",
             "extending it!")
    object$.processHistory[[(length(object$.processHistory) + 1)]] <- ph
    if (validObject(object))
        return(object)
})

#' @rdname MseekFT-class
#' 
#' @description extract a list of \code{ProcessHistory} objects from an object,
#'  representing changes made to the object.
#' 
#' @export
setMethod("processHistory", "MseekFT", function(object) {
    object$.processHistory})

#' @importClassesFrom CAMERA xsAnnotate
#' @noRd
#' @export
setMethod("processHistory", "xsAnnotate", function(object) {
    object@xcmsSet@.processHistory})

#' @importClassesFrom CAMERA xsAnnotate
#' @noRd
setMethod("addProcessHistory", "xsAnnotate", function(object, ph) {
    if (!inherits(ph, "ProcessHistory"))
        stop("Argument 'ph' has to be of type 'ProcessHistory' or a class ",
             "extending it!")
    object@xcmsSet@.processHistory[[(length(object@xcmsSet@.processHistory) + 1)]] <- ph
    if (validObject(object))
        return(object)
})


#' @rdname FTProcessHistory-class
setMethod("hasError", "FTProcessHistory",
          function(object){
              length(object@error) > 0
          })

#' @rdname FTProcessHistory-class
setMethod("error", "FTProcessHistory",
          function(object){
              object@error
          })

#' @title buildMseekFT
#' @aliases buildMseekFT
#' 
#' @description build an \code{MseekFT} object from an xsAnnotate, xcmsSet or data.frame object
#'
#' @param object an xsAnnotate, xcmsSet or data.frame object.
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
setMethod("buildMseekFT",  "xsAnnotate",
          function(object, processHistory = list(), ...){
              
              oldHistory <- if(length(processHistory)){processHistory}else{processHistory(object)}
              
              res <- constructFeatureTable(df = CAMERA::getPeaklist(object),
                                    processHistory = c(oldHistory,
                                                       FTProcessHistory(info = "Extracted Feature Table from CAMERA::xsAnnotate using CAMERA::getPeaklist and built MseekFT object",
                                                                       param = xcms::GenericParam(fun = "buildMseekFT",
                                                                                                  args = list(...)))),
                                    ...)
              res$sampleNames <- object@xcmsSet@phenoData$sampleNames
              res$RTcorr <- rtexport(object)
              res$RTcorrected <- !identical(res$RTcorr$corr, res$RTcorr$noncorr)
              
              return(res)
              
          })

#' @rdname buildMseekFT
setMethod("buildMseekFT", 
          "XCMSnExp",
          function(object, processHistory = list(), ...){
              
              oldHistory <- if(length(processHistory)){processHistory}else{processHistory(object)}
              
              
              res <- constructFeatureTable(df = xcms::peakTable(as(object,"xcmsSet")),
                                    processHistory = c(oldHistory,
                                                       FTProcessHistory(info = "Extracted Feature Table from xcms::XCMSnExp using xcms::peakTable(as(xset,'xcmsSet')) and built MseekFT object",
                                                                      param = xcms::GenericParam(fun = "buildMseekFT",
                                                                                                 args = list(...)))),
                                    ...)
              
              res$sampleNames <- as.character(object@phenoData@data$sampleNames)
              res$RTcorr <- rtexport(object)
              res$RTcorrected <- !identical(res$RTcorr$corr, res$RTcorr$noncorr)
              
              return(res)
          })

#' @rdname buildMseekFT
setMethod("buildMseekFT", 
          signature(object = "data.frame"),
          function(object, processHistory = list(), ...){
              
              constructFeatureTable(df = object,
                                    processHistory = c(processHistory,
                                                       FTProcessHistory(info = "Built MseekFT object from a data.frame.",
                                                                      param = xcms::GenericParam(fun = "buildMseekFT",
                                                                                                 args = list(...)))),
                                    ...)
          })

#' @title saveMseekFT
#' @aliases saveMseekFT
#' 
#' @description save a \code{MseekFT} object to a file, registering the save event in the processHistory 
#'
#' @param object an \code{MseekFT} object.
#' @param file file path to write to
#' 
#' @return the \code{MseekFT} object, with saving event added to processHistory
#'   
#' @rdname buildMseekFT
setMethod("saveMseekFT", 
          "MseekFT",
          function(object, file, writeCSV = FALSE, writeRDS = TRUE){
              
              object <- addProcessHistory(object, xcms:::XProcessHistory(info = "Saved MseekFT object to a file.",
                                                       param = xcms::GenericParam(fun = "Metaboseek::saveMseekFT",
                                                                                  args = list(file = file,
                                                                                              writeCSV = writeCSV,
                                                                                              writeRDS = writeRDS))))
              #make sure file extension is .mskFT
              if(writeCSV){
                  tableWriter(object$df, fname = paste0(file,".csv"))
              }
              
              
              #make sure file extension is .mskFT
              if(writeRDS){
                  saveRDS(object, file = paste0(file,".mskFT"))
              }
              
              
              invisible(object)
          })

#' @title MseekFT-class
#' 
#' @description save a \code{MseekFT} object to a file, registering the save event in the processHistory 
#'
#' @param object an \code{MseekFT} object.
#' @param name file path to write to
#' 
#'   
#' @rdname MseekFT-class
setMethod("rename", 
          "MseekFT",
          function(object, name){
              
              object <- addProcessHistory(object, xcms:::XProcessHistory(info = "Renamed MseekFT object",
                                                                         param = xcms::GenericParam(fun = "rename",
                                                                                                    args = list(name = name))))
           object$tablename <- name
              
             return(object)
          })

#' @title .withHistory
#' @aliases .withHistory
#' 
#' @description apply a function to an object and add this event to the object's
#' processHistory, or return a list that contains the event history
#'
#' @param fun character(1), name of the function to apply
#' @param args named list of arguments to fun, will be kept in the ProcessHistory as they are
#' @param longArgs arguments to fun that are large. They will be supplied to fun as they are, but 
#' will be summarized using \code{summary()} in the ProcessHistory.
#' @param addHistory if TRUE, adds the processing history to the output object if an 
#' \code{addProcessHistory} method exists for it. If FALSE, this function
#'  returns a list(), see \code{Value}!
#' @param continueWithErrors if TRUE, if an error occurs during the call to fun,
#' it is recorded in the ProcessHistory, and the value specified in returnIfError
#' is returned.
#' @param returnIfError object to return if error occurs. Will be treated like the 
#' object returned by fun (ProcessHistory will be added to the object if possible to keep track of failed analyses). 
#' 
#' @param param a \code{\link{FTAnalysisParam}} object
#' 
#' @return if \code{addHistory = TRUE}: the object returned by fun, with analyses performed as
#' defined by args and longArgs and ProcessHistory added to the object.
#' if \code{addHistory = FALSE}: a list with elements \code{history} and \code{result}
#'   
.withHistory <- function(fun = character(), args = list(),
                         longArgs = list(), addHistory = TRUE,
                         continueWithErrors = FALSE,
                         returnIfError = NULL){
              
              errlist <- NULL
                
              tryCatch({
               res <- do.call(fun,
                      c(args, longArgs)
                      )
                },
              error = function(e){
                  
                  if(continueWithErrors){
                      errlist <- list(fun = e)
                      res <- returnIfError
                      }else{
                      stop(e)
                      }
                  
                  })
              hstry <-  xcms:::XProcessHistory(info = paste0("Applied function ", fun, "and returned a ", class(res))[1],
                                               error = errlist,
                                               param = FunParam(fun = "Metaboseek::saveMseekFT",
                                                                    args = args,
                                                                longArgs = lapply(summary,longArgs)))
              if(addHistory){
                  tryCatch({
                  res <- addProcessHistory(object = res, hstry)
                  },
                  error = function(e){
                      warning("Could not add history to output object. History for this step was discarded.")
                      if(!is.null(errlist)){warning(errlist)}
                      }
                  )
                  
                  return(res)
                  
                  }
              return(list(history = hstry,
                          result = res))
              
          }

#' FTProcessHistory
#' @aliases FTProcessHistory
#' 
#' @title Constructor for FTProcessHistory class
#' 
#' @description Construct a \code{FTProcessHistory} object with parameters for 
#' use with \code{\link{analyzeFT}()}
#' 
#' @param error a named list of character() vectors, reporting errors that 
#' occured in this analysis step 
#' @param changes did changes occur on the associated object. If FALSE, this step
#'  did not result in relevant changes and could be dropped for reporting
#' @param inputDFhash character digest of the data.frame object before this analysis step
#' @param outputDFhash character digest of the data.frame object after this analysis step
#' @param sessionInfo a \code{\link[utils]{sessionInfo}} object, should be generated
#'  at time of the recorded event and at least once in every session (by default,
#'  will be populated by load and constructor methods for MseekFT class).
#' @param fileNames names of files used in this analysis step (a more human-readable 
#' variant of the fileIndex slot)
#' @param msLevel,... additional arguments passed to \code{\link[xcms]{XProcessHistory}()}
#'
#' @return a \code{\link{FTProcessHistory-class}} object
#' 
#' @examples
#' FTProcessHistory(error = list(),
#' changes = TRUE,
#' inputDFhash = digest::digest(data.frame(1,stringsAsFactors = FALSE),
#'                              algo = "xxhash64"),
#' outputDFhash = digest::digest(data.frame(2,stringsAsFactors = FALSE),
#'                               algo = "xxhash64"),
#' sessionInfo = utils::sessionInfo(),
#' info = "Example")
#' 
#' @rdname FTProcessHistory
#' @export
FTProcessHistory <- function(error = list(),
                             changes = TRUE,
                             inputDFhash = NULL,
                             outputDFhash = NULL,
                             sessionInfo = NULL,
                             fileNames = character(),
                             msLevel = NA_integer_,
                             ...) {
    obj <- xcms:::XProcessHistory(msLevel = msLevel, ...)
    obj <- as(obj, "FTProcessHistory")
    obj@error <- error
    obj@changes <- as.logical(changes)
    obj@inputDFhash <- inputDFhash
    obj@outputDFhash <- outputDFhash
    obj@sessionInfo <- sessionInfo
    obj@fileNames <- fileNames
    Biobase::classVersion(obj)["FTProcessHistory"] <- "0.0.1"
    OK <- validObject(obj)
    if (is.character(OK))
        stop(OK)
    return(obj)
}

#' @title FunParam
#' @return returns a \code{FunParam} object.
#'
#' @param fun \code{character} representing the name of the function.
#' @param args named \code{list} with the arguments to the function \code{fun}.
#' @param longArgs named \code{list} of large arguments to the function 
#' \code{fun}, summarized using \code{summary()}.
#'
#' @rdname FunParam
#' @export
FunParam <- function(fun = character(), args = list(), longArgs = list()){
    obj <- new("FunParam", fun = fun, args = args, longArgs = longArgs)
    
    OK <- validObject(obj)
    if (is.character(OK))
        stop(OK)
    return(obj)
}

#' FTAnalysisParam
#' @aliases FTAnalysisParam
#' 
#' @title Constructor for FTAnalysisParam class
#' @description Construct a \code{FTAnalysisParam} object with parameters for 
#' use with \code{\link{analyzeFT}()}
#' 
#' @inheritParams analyzeTable
#' @param .files character() with file paths to the MS data files to be used
#'  in an analysis
#' @return a \code{\link{FTAnalysisParam-class}} object
#'
#' @rdname FTAnalysisParam
#' @export
FTAnalysisParam <- function(intensities = character(),
                            groups = list(),
                            .files = character(),
                            analyze = c("Basic analysis", "clara_cluster",
                                        "t-test", "Peak shapes",
                                        "Fast peak shapes", "PCA features",
                                        "PCA samples", "mzMatch"), 
                            normalize = T,
                            useNormalized = T,
                            logNormalized = F,
                            ppm = 5,
                            controlGroup = NULL,
                            numClusters = 2,
                            mzMatchParam = list(db ="smid-db_pos.csv",
                                                ppm = 5,
                                                mzdiff = 0.001),
                            workers = 1){
    
    return(new("FTAnalysisParam",
               intensities = intensities,
               groups = groups,
               .files = .files,
               analyze = analyze, 
               normalize = normalize,
               useNormalized = useNormalized,
               logNormalized = logNormalized,
               #MSData = NULL,
               ppm = ppm,
               controlGroup = controlGroup,
               numClusters = numClusters,
               mzMatchParam = mzMatchParam,
               workers = workers))}

