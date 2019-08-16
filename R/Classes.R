setClassUnion("listOrNULL", c("list", "NULL"))

setOldClass("MseekFT")

#'
#'
#' 
#' @rdname FTAnalysisParam
setClass("FTAnalysisParam",
         slots = c(#df = "data.frame", 
                   intensities = "character",
                   groups = "list",
                   .files = "character",
                   analyze = "character", 
                   normalize = "logical",
                   useNormalized = "logical",
                   logNormalized = "logical",
                   #MSData = NULL,
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
             mzMatchParam = list(db = system.file("db",
                                             "smid-db_pos.csv",
                                             package = "Metaboseek"),
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

#'
#'
#' 
#' @rdname FTProcessHistory
setClass("FTProcessHistory",
         slots = c(error = "listOrNULL",
                   changes = "logical"),
         contains = "XProcessHistory",
         prototype = prototype(
             error = list(),
             changes = FALSE
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



setGeneric("analyzeFT", function(object, MSData, param) standardGeneric("analyzeFT"))
setGeneric("hasError", function(object) standardGeneric("hasError"))
setGeneric("error", function(object) standardGeneric("error"))

setMethod("initialize", "FTAnalysisParam", function(.Object, ...) {
    Biobase::classVersion(.Object)["FTAnalysisParam"] <- "0.0.1"
    callNextMethod(.Object, ...)
})

#' @aliases analyzeFT
#'
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
#' @param object an MseekFT or data.frame object.
#' @param MSData list of xcmsRaw objects
#' @param param an \code{FTAnalysisParam}
#' 
#' @rdname analyzeFT
setMethod("analyzeFT", 
          signature(object = "MseekFT",
                    MSData = "listOrNULL",
                    param = "FTAnalysisParam"),
          function(object, MSData, param){
              
              param@intensities <- object$intensities
              param@groups <- object$anagroupnames
              
              
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
                                                           error = res$errmsg))

              return(object)
          })

setMethod("initialize", "FTProcessHistory", function(.Object, ...) {
    Biobase::classVersion(.Object)["FTProcessHistory"] <- "0.0.1"
    callNextMethod(.Object, ...)
})

#' @rdname analyzeFT
setMethod("hasError", "FTProcessHistory",
          function(object){
              length(object@error) > 0
          })

#' @rdname analyzeFT
setMethod("error", "FTProcessHistory",
          function(object){
              object@error
          })

#'
#'
#'
#'
#' @rdname FTAnalysisParam
FTProcessHistory <- function(error = list(), changes = TRUE, ...) {
    obj <- xcms:::XProcessHistory(...)
    obj <- as(obj, "FTProcessHistory")
    obj@error <- error
    obj@changes <- as.logical(changes)
    Biobase::classVersion(obj)["FTProcessHistory"] <- "0.0.1"
    OK <- validObject(obj)
    if (is.character(OK))
        stop(OK)
    return(obj)
}

#'
#' @description Construct a \code{FTAnalysisParam} object with parameters for 
#' use with \code{\link{analyzeFT}}
#' 
#' @inheritParams analyzeTable
#' @param .files character() with file paths to the MS data files to be used
#'  in an analysis
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
                            #MSData = NULL,
                            ppm = 5,
                            controlGroup = NULL,
                            numClusters = 2,
                            mzMatchParam = list(db = system.file("db",
                                                                 "smid-db_pos.csv",
                                                                 package = "Metaboseek"),
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
