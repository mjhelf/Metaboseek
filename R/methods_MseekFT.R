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

#' @param selCols selected columns (with intensity values)
#' @param logNormalized if TRUE, applies a log10 to intensity values after normalization
#' @rdname featureTableNormalize
#' @export
setMethod("FTNormalize", "data.frame",
          function(object, intensityCols, logNormalized = FALSE){
              
              #normalize data and save it in matrix
              mx <- as.matrix(object[,intensityCols])
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



#' @noRd
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
