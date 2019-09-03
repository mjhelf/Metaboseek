#' @include methods_buildMseekFT.R

#' @aliases addProcessHistory
#' @name MseekFT-class
#' 
#' @description 
#' 
#' \code{addProcessHistory}: adds (appends) a single \code{\link{ProcessHistory}}
#'  object to the \code{.processHistory} slot. Copied description and Method 
#'  template for \code{addProcessHistory} from \code{xcms}.
#' 
#' @param ph a \code{ProcessHistory} object
#' @param fun character(1), function name to look for
#' @param value for \code{groupingTable<-}: a data.frame with columns 
#' \code{Column} and \code{Group}. For \code{intensityCols<-}: a character vector of column names
#' 
#' @return
#' The \code{addProcessHistory} method returns the input object with the
#' provided \code{\link{ProcessHistory}} appended to the process history.
#'
#' @rdname MseekFT-class
#' @export
setMethod("addProcessHistory", c("MseekFT", "ProcessHistory"), function(object, ph) {
    if (!inherits(ph, "ProcessHistory"))
        stop("Argument 'ph' has to be of type 'ProcessHistory' or a class ",
             "extending it!")
    object$.processHistory[[(length(object$.processHistory) + 1)]] <- ph
    if (validObject(object))
        return(object)
})

#' @rdname MseekFT-class
#' @aliases processHistory
#' @description \code{processHistory}: extract a list of \code{ProcessHistory} objects from an object,
#'  representing changes made to the object.
#' 
#' @export
setMethod("processHistory", "MseekFT", function(object) {
    if(!length(object$.processHistory)){
        return(list())
        }else{
    return(object$.processHistory)
            }
        })

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
                     
                     if(is.data.frame(groupingTable(object)) 
                        && nrow(groupingTable) == length(value)){
                         
                         groupingTable(object)$Column <- value
                         
                     }else{
                         groupingTable(object) <- data.frame(Column = value,
                                                             Group = "G1",
                                                             stringsAsFactors = FALSE)
                     }
                     object
                 })

#' @aliases rename
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
              
              object <- addProcessHistory(object, FTProcessHistory(info = "Renamed MseekFT object",
                                                                   param = FunParam(fun = "rename",
                                                                                    args = list(name = name))))
              object$tablename <- name
              
              return(object)
          })

#' @aliases hasAdjustedRtime
#' @description \code{hasAdjustedRtime}: check if the object contains retention time
#' correction information.
#' @return For \code{hasAdjustedRtime}: A logical, indicating if the object 
#' contains retention time correction information.
#' 
#' @rdname MseekFT-class
#' @export
setMethod("hasAdjustedRtime", "MseekFT",
          function(object){
              return(!is.null(object$RTcorrected) && object$RTcorrected)
          })





#' @noRd
#' @description \code{FTFilter}: apply a list of filters to a MseekFT object
#' @param filters a list of filters
#' @export
setMethod("FTFilter", c("data.frame"),
          function(object,
                   filters = list(),
                   sortBy = character(),
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

#' @aliases FTFilter
#' @rdname MseekFT-class
#' @description \code{FTFilter}: apply a list of filters to a MseekFT object
#' @param filters a list of filters
#' @param sortBy sort by this column
#' @param decreasing logical to specify if sort should be decreasing.
#' @export
setMethod("FTFilter", c("MseekFT"),
          function(object,
                   filters = list(),
                   sortBy = character(),
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
                  
                  beforeRows <- nrow(object$df)
                  
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
                  
                  if(!length(err)){
                      msg <- paste("Filtered Feature Table; before:", beforeRows,
                                   "Features, after:", nrow(object$df), "Features" )
                  }else{
                      msg <- "Failed to filter MseekFT object"
                  }
                  
                  object <- addProcessHistory(object,
                                              FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputDFhash = beforeHash,
                                                               outputDFhash = afterHash,
                                                               fileNames = character(),
                                                               error = err,
                                                               sessionInfo = NULL,
                                                               processingTime = p1,
                                                               info = msg,
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
