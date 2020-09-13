## Generics
setGeneric("analyzeFT", function(object, MSData, param) standardGeneric("analyzeFT"))
setGeneric("addProcessHistory", function(object, ph, ...) standardGeneric("addProcessHistory"))
setGeneric("buildMseekFT", function(object, ...) standardGeneric("buildMseekFT"))
setGeneric("buildMseekGraph", function(object, ...) standardGeneric("buildMseekGraph"))

setGeneric("error", function(object) standardGeneric("error"))
setGeneric("FTAnova", function(object, ...) standardGeneric("FTAnova"))

setGeneric("FTBasicAnalysis", function(object, ...) standardGeneric("FTBasicAnalysis"))
setGeneric("FTCluster", function(object, ...) standardGeneric("FTCluster"))
setGeneric("FTedges", function(object, ...) standardGeneric("FTedges"))

#setGeneric("FTHash", function(object, ...) standardGeneric("FTHash"))
setGeneric("FTFilter", function(object, ...) standardGeneric("FTFilter"))
setGeneric("FTMS2scans", function(object, rawdata, ...) standardGeneric("FTMS2scans"))

setGeneric("FTMzMatch", function(object, ...) standardGeneric("FTMzMatch"))

setGeneric("FTNormalize", function(object, fun, ...) standardGeneric("FTNormalize"))
setGeneric("FTOldPeakShapes", function(object, rawdata, ...) standardGeneric("FTOldPeakShapes"))
setGeneric("FTPCA", function(object, ...) standardGeneric("FTPCA"))

setGeneric("FTPeakShapes", function(object, rawdata, ...) standardGeneric("FTPeakShapes"))
setGeneric("FTT.test", function(object, ...) standardGeneric("FTT.test"))

setGeneric("getMseekIntensities", function(object, rawdata, importFrom, ...) standardGeneric("getMseekIntensities"))
setGeneric("getSpecList", function(object, rawdata, ...) standardGeneric("getSpecList"))

setGeneric("groupingTable", function(object) standardGeneric("groupingTable"))
setGeneric("groupingTable<-", function(object, value) standardGeneric("groupingTable<-"))
setGeneric("hasAdjustedRtime", function(object) standardGeneric("hasAdjustedRtime"))

setGeneric("hasError", function(object) standardGeneric("hasError"))
setGeneric("intensityCols", function(object) standardGeneric("intensityCols"))
setGeneric("intensityCols<-", function(object, value) standardGeneric("intensityCols<-"))

setGeneric("loadMseekFT", function(object) standardGeneric("loadMseekFT"))
setGeneric("loadMseekGraph", function(object, ...) standardGeneric("loadMseekGraph"))

setGeneric("matchReference", function(object, query, ...) standardGeneric("matchReference"))
setGeneric("MseekHash", function(object) standardGeneric("MseekHash"))


setGeneric("PatternFinder", function(object, ...) standardGeneric("PatternFinder"))

setGeneric("previousStep", function(object, ...) standardGeneric("previousStep"))
setGeneric("removeNAs", function(object, ...) standardGeneric("removeNAs"))
setGeneric("rename", function(object, ...) standardGeneric("rename"))
setGeneric("saveMseekFT", function(object, file, ...) standardGeneric("saveMseekFT"))
setGeneric("saveMseekGraph", function(object, file, ...) standardGeneric("saveMseekGraph"))

setGeneric("searchFunParam", function(object, fun, ...) standardGeneric("searchFunParam"))
setGeneric("setLayout", function(object, layout, ...) standardGeneric("setLayout"))


setGeneric("shortPrint", function(object) standardGeneric("shortPrint"))
setGeneric("simplify", function(object, ...) standardGeneric("simplify"))


setGeneric("importMseekIntensities", function(object, rawdata, importFrom, ...) standardGeneric("importMseekIntensities")) #transfer Mseek intensities and history entry about making them


#setGeneric("processHistory", function(object, ...) standardGeneric("processHistory"))
setGeneric("withHistory", function(object, fun, ...) standardGeneric("withHistory"))

setGeneric("LabelFinder", function(object, ...) standardGeneric("LabelFinder"))


## Registered S3 classes
setOldClass("MseekFT")
setOldClass("sessionInfo")
setOldClass("MseekGraph")

## Class unions
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("sessionInfoOrNULL", c("sessionInfo", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("MseekFTOrNULL", c("MseekFT", "NULL"))
setClassUnion("MseekFamily", c("MseekFT", "MseekGraph"))

#' @noRd
#' @author Johannes Rainer
setClass("Param",
         representation = representation("VIRTUAL"),
         contains = c("Versioned"))
setClassUnion("ParamOrNULL", c("Param", "NULL"))

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
         slots = c(intensities = "characterOrNULL",
                   groups = "listOrNULL",
                   .files = "character",
                   analyze = "characterOrNULL", 
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
#' @aliases FunParam
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
#' 
#' @rdname FunParam-class
setClass("FunParam",
         slots = c(fun = "character",
             args = "list",
             longArgs = "list"
                   ),
         contains = "Param",
         prototype = prototype(
             fun = character(),
             longArgs = list(),
             args = list()
         ),
         validity = function(object) {
             msg <- character()
              
             if (!length(object@fun) && !length(object@args)){
                 msg <- c(msg, paste0("No function specified!"))}    
             
             # if (!length(object@args) && !length(object@args)){
             #     msg <- c(msg, paste0("No arguments for function specified!"))}
             
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
#' @slot inputHash character \code{\link{MseekHash}} of the object before this analysis step
#' @slot outputHash character \code{\link{MseekHash}} of the object after this analysis step
#' @slot sessionInfo a \code{\link[utils]{sessionInfo}} object, should be genereated
#'  at time of the recorded event and at least once in every session (by default,
#'  will be populated by load and constructor methods for MseekFT class).
#' @slot fileNames names of files used in this analysis step (a more human-readable 
#' variant of the fileIndex slot)
#' @slot processingTime time spent on processing this step, in seconds
#' 
#' @rdname FTProcessHistory-class
setClass("FTProcessHistory",
         slots = c(error = "listOrNULL",
                   changes = "logical",
                   fileNames = "characterOrNULL",
                   inputHash = "characterOrNULL",
                   outputHash = "characterOrNULL",
                   sessionInfo = "sessionInfoOrNULL",
                   processingTime = "numeric"),
         contains = "XProcessHistory",
         prototype = prototype(
             error = list(),
             fileNames = character(),
             changes = FALSE,
             inputHash = NULL,
             outputHash = NULL,
             sessionInfo = NULL,
             processingTime = NA_real_
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
    
    # chLabel <- if(object@changes){"yes"}else{"no"}
    # cat(" changes:", chLabel, "\n")
    
    cat(object@inputHash, "->", object@outputHash, "\n")
    if(!is.null(object@sessionInfo)){
    cat("contains sessionInfo:", "\n")
}

    if(length(object@fileNames)){
    cat(" fileNames:", object@fileNames, "\n")
    }
        
    if(!is.na(object@processingTime)){
        if(object@processingTime > 60){
            timeform <- paste0(format(object@processingTime/60, nsmall = 2, scientific = FALSE), " min.")
        }else{
            timeform <- paste0(format(object@processingTime, nsmall = 3, scientific = NA), " sec.")
            }
        cat(" processing time:", timeform, "\n")
    }  
    
    erLabel <- if(length(object@error)){""}else{"-none-"}
    cat(" errors:", erLabel, "\n")
    if (length(object@error) > 0) {
        for (i in seq_len(length(object@error))) {
            if (!is.null(names(object@error)))
                cat(" ", names(object@error)[i], "= ")
            cat(object@error[[i]], "\n")
        }
    }
    

    
   
})

#' @noRd
setMethod("show", "XProcessHistory", function(object) {
    callNextMethod()
    pcLabel <- "-none-"
    if (length(object@param)){
        pcLabel <- class(object@param)}
    cat(" Parameter class:", pcLabel, "\n")
    if (length(object@param)){
        show(object@param)}
    if (!is.na(object@msLevel))
        cat(" MS level(s)", paste(object@msLevel, sep = " "), "\n")
    
})

setMethod("shortPrint", "ANY", function(object){
    
    if(class(object) == "FTProcessHistory"){
        cat(object@info, "\n")
        cat(paste0("...",.characterTail(object@inputHash), " -> ",
                   "...",.characterTail(object@outputHash),
                   "\n"))
        if(length(object@error)){
            print(object@error)
        }
        }
    else{
    print(object)
    }
    
    })


setMethod("shortPrint", "FTProcessHistory", function(object){
    
    cat(object@info, "\n")
    cat(paste0("...",.characterTail(object@inputHash), " -> ",
               "...",.characterTail(object@outputHash),
               "\n"))
    if(length(object@error)){
    print(object@error)
    }

    })

.characterTail <- function(x, n = 6){
    sapply(x, function(x){
        if(!length(nchar(x)) 
           || is.na(x)){return("")} 
    substr(x, nchar(x)-n+1, nchar(x))
    })
}

#' @noRd
setMethod("show", "FunParam", function(object) {
    cat(" Object of class:", class(object), "\n")
    cat("  function:", object@fun, "\n")
    
    cat("  arguments:\n")
    if (length(object@args) > 0) {
        for (i in 1:length(object@args)) {
            cat("   ", names(object@args)[i], "= ")
            if (is.atomic(object@args[[i]]) 
                && !length(names(object@args[[i]]))
                && ! "summaryDefault" %in% class(object@args[[i]])){
                cat(object@args[[i]], "\n")
            }else{
                cat("\n")    
                print(object@args[[i]]); cat("\n")    
            }
            
            
        }
    }    
    cat("  long (summarized) arguments:\n")
    if (length(object@longArgs) > 0) {
        for (i in 1:length(object@longArgs)) {
            cat("   ", names(object@longArgs)[i], "= ")
            if (is.atomic(object@longArgs[[i]]) 
                && !length(names(object@longArgs[[i]]))
                && ! "summaryDefault" %in% class(object@longArgs[[i]])){
                cat(object@longArgs[[i]], "\n")
                }else{
                cat("\n")    
                print(object@longArgs[[i]]); cat("\n")    
                }
            
            
        }
    }
})




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
#' @param object an object of class "ProcessHistory", or one of its descendants
#' @export
setMethod("hasError", "ProcessHistory",
          function(object){
              length(object@error) > 0
          })



#' @rdname FTProcessHistory-class
setMethod("error", "ProcessHistory",
          function(object){
              object@error
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
#' @param inputHash character \code{\link{MseekHash}} of the object before this analysis step
#' @param outputHash character \code{\link{MseekHash}} of the object after this analysis step
#' @param sessionInfo a \code{\link[utils]{sessionInfo}} object, should be generated
#'  at time of the recorded event and at least once in every session (by default,
#'  will be populated by load and constructor methods for MseekFT class).
#' @param fileNames names of files used in this analysis step (a more human-readable 
#' variant of the fileIndex slot)
#' @param processingTime time spent on processing this step, in seconds
#' @param msLevel,... additional arguments passed to \code{\link[xcms]{XProcessHistory}()}
#'
#' @return a \code{\link{FTProcessHistory-class}} object
#' 
#' @examples
#' FTProcessHistory(error = list(),
#' changes = TRUE,
#' inputHash = NULL,
#' outputHash = NULL,
#' sessionInfo = utils::sessionInfo(),
#' info = "Example")
#' 
#' @rdname FTProcessHistory
#' @export
FTProcessHistory <- function(error = list(),
                             changes = TRUE,
                             inputHash = NULL,
                             outputHash = NULL,
                             sessionInfo = NULL,
                             fileNames = character(),
                             processingTime = NA_real_,
                             msLevel = NA_integer_,
                             ...) {
    obj <- xcms:::XProcessHistory(msLevel = msLevel, ...)
    obj <- as(obj, "FTProcessHistory")
    obj@error <- error
    obj@changes <- as.logical(changes)
    obj@inputHash <- inputHash
    obj@outputHash <- outputHash
    obj@sessionInfo <- sessionInfo
    obj@fileNames <- fileNames
    obj@processingTime <- processingTime
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
    
    # OK <- validObject(obj)
    # if (is.character(OK))
    #     stop(OK)
    return(obj)
}

#' FTAnalysisParam
#' @aliases FTAnalysisParam
#' 
#' @title Constructor for FTAnalysisParam class
#' @description Construct a \code{FTAnalysisParam} object with parameters for 
#' use with \code{\link{analyzeFT}()}
#' 
#' @param intensities the intensity column names, before normalization 
#' (without __norm suffix), will be automatically renamed if useNormalized.
#' @param groups named list of non-normalized intensity columns listed by group 
#' (as supplied by $anagroupnames of MseekFT objects), will be automatically 
#' renamed if useNormalized.
#' @param analyze character vector to select the analyses to be run: 
#' "Basic analysis", "clara_cluster", "t-test", "Peak shapes"
#' @param normalize normalze intensity columns
#' @param useNormalized use normalized values for analyses; will trigger 
#' normalize if there is no normalized data available for all selected 
#' intensity columns
#' @param logNormalized if TRUE, applies a log10 to intensity values after normalization
#' @param .files character() with file paths to the MS data files to be used
#'  in an analysis
#' @param ppm ppm range for peak shape analysis
#' @param controlGroup control group for foldChange (part of Basic analysis) 
#' analysis (optional) 
#' @param numClusters number of clusters for clara_clusters analysis
#' @param mzMatchParam list of parameters passed to mass
#' @param workers number of workers to use for multithreaded analyses
#' 
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

