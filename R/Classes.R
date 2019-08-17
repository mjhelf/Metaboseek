## Generics
setGeneric("analyzeFT", function(object, MSData, param) standardGeneric("analyzeFT"))
setGeneric("hasError", function(object) standardGeneric("hasError"))
setGeneric("error", function(object) standardGeneric("error"))
setGeneric("buildMseekFT", function(object, ...) standardGeneric("buildMseekFT"))
setGeneric("saveMseekFT", function(object, file) standardGeneric("saveMseekFT"))
setGeneric("loadMseekFT", function(object) standardGeneric("loadMseekFT"))
setGeneric("addProcessHistory", function(object, ...) standardGeneric("addProcessHistory"))


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
#' 
#' @rdname FTProcessHistory-class
setClass("FTProcessHistory",
         slots = c(error = "listOrNULL",
                   changes = "logical",
                   inputDFhash = "characterOrNULL",
                   outputDFhash = "characterOrNULL",
                   sessionInfo = "sessionInfoOrNULL"),
         contains = "XProcessHistory",
         prototype = prototype(
             error = list(),
             changes = FALSE,
             inputDFhash = digest::digest(data.frame(stringsAsFactors = FALSE),
                                  algo = "xxhash64"),
             outputDFhash = digest::digest(data.frame(stringsAsFactors = FALSE),
                                           algo = "xxhash64"),
             sessionInfo = utils::sessionInfo()
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
#'  recording processing history if \code{object} is of class \code{MseekFT}
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
                      c(list(df = object, MSData = MSData[param@.files]),
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
                                          ProcessHistory(param = param,
                                                           changes = changes,
                                                           error = res$errmsg,
                                                           info = param@analyze,
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

#' @rdname FTProcessHistory-class
setMethod("show", "FTProcessHistory", function(object) {
    callNextMethod()
    erLabel <- if(length(object@error)){object@error}else{"-none-"}
    cat(" error:", erLabel, "\n")
    chLabel <- if(object@changes){"yes"}else{"no"}
    cat(" error:", chLabel, "\n")
    cat("input data.frame hash:", object@inputDFhash)
    cat("output data.frame hash:", object@outputDFhash)
    cat(" contains sessionInfo:", !is.null(object@sessionInfo), "\n")
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
#' @noRd
setMethod("addProcessHistory", "MseekFT", function(object, ph) {
    if (!inherits(ph, "ProcessHistory"))
        stop("Argument 'ph' has to be of type 'ProcessHistory' or a class ",
             "extending it!")
    object$.processHistory[[(length(object$.processHistory) + 1)]] <- ph
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
#' @rdname buildMseekFT
setMethod("buildMseekFT", 
          signature(object = "CAMERA::xsAnnotate"),
          function(object, processHistory = list(), ...){
              
              oldHistory <- if(length(processHistory)){processHistory}else{history(object)}
              
              constructFeatureTable(df = CAMERA::getPeaklist(xset),
                                    processHistory = c(oldHistory,
                                                       XProcessHistory(info = "Extracted Feature Table from CAMERA::xsAnnotate using CAMERA::getPeaklist and built MseekFT object",
                                                                       param = xcms::GenericParam(fun = "Metaboseek::buildMseekFT",
                                                                                                  args = list(...)))),
                                    ...)
          })

#' @rdname buildMseekFT
setMethod("buildMseekFT", 
          signature(object = "XCMSnExp"),
          function(object, processHistory = list(), ...){
              
              oldHistory <- if(length(processHistory)){processHistory}else{history(object)}
              
              constructFeatureTable(df = xcms::peakTable(as(xset,"xcmsSet")),
                                    processHistory = c(oldHistory,
                                                       XProcessHistory(info = "Extracted Feature Table from xcms::XCMSnExp using xcms::peakTable(as(xset,'xcmsSet')) and built MseekFT object",
                                                                      param = xcms::GenericParam(fun = "Metaboseek::buildMseekFT",
                                                                                                 args = list(...)))),
                                    ...)
          })

#' @rdname buildMseekFT
setMethod("buildMseekFT", 
          signature(object = "data.frame"),
          function(object, processHistory = list(), ...){
              
              constructFeatureTable(df = xcms::peakTable(as(xset,"xcmsSet")),
                                    processHistory = c(processHistory,
                                                       XProcessHistory(info = "Built MseekFT object from a data.frame.",
                                                                      param = xcms::GenericParam(fun = "Metaboseek::buildMseekFT",
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
          signature(object = "MseekFT"),
          function(object, file){
              
              object <- addProcessHistory(object, XProcessHistory(info = "Built MseekFT object from a data.frame.",
                                                       param = xcms::GenericParam(fun = "Metaboseek::saveMseekFT",
                                                                                  args = list(file = file))))
              #make sure file extension is .mskFT
              if(!grepl("\\.mskFT$",file, ignore.case = TRUE)){
                 file <- paste0(file,".mskFT")
                  }
              
              saveRDS(object, file = file)
              
              invisible(object)
          })


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
#' @param ... additional arguments passed to \code{\link[xcms]{XProcessHistory}()}
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
                             ...) {
    obj <- xcms:::XProcessHistory(...)
    obj <- as(obj, "FTProcessHistory")
    obj@error <- error
    obj@changes <- as.logical(changes)
    obj@inputDFhash <- inputDFhash
    obj@outputDFhash <- outputDFhash
    obj@sessionInfo <- sessionInfo
    Biobase::classVersion(obj)["FTProcessHistory"] <- "0.0.1"
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
