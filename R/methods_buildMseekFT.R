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
                                                             outputDFhash = MseekHash(res),
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
                                                             outputDFhash = MseekHash(res),
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
                                                             outputDFhash = MseekHash(res),
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
                                                                 outputDFhash = MseekHash(res),
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
              
              object <- addProcessHistory(object, xcms:::XProcessHistory(info = "Saved MseekFT object to a file.",
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


#' @title MseekGraphs
#'
#' @rdname MseekGraphs
#' @name MseekGraphs
#' @aliases buildMseekGraph
#' 
#' @description \code{MseekGraph} objects are \code{\link[igraph]{igraph}} objects
#' which have layout information and slots that are the same as MseekFT objects,
#' except that the bulk of data is kept in an \code{igraph} object instead of 
#' a \code{data.frame}. These objects are read and displayed by the 
#' \code{\link{NetworkModule}}.
#' 
#' @export
setMethod("buildMseekGraph", c("MseekFT"),
          function(object, cosineThreshold = 0.6){
              beforeHash <- MseekHash(object)
              
              p1 <- proc.time()
              err <- list()
              res <- object[names(object) != "df"]
              #only difference is how the dataframe is saved: now in the $graph slot as igraph...
              
              #list(graph = NULL,
              #           .processHistory = processHistory(object)
              #)
              class(res) <- "MseekGraph"
              
              tryCatch({
                  
                  
                  if(is.null(object$edges)){
                      stop("No edgelist found. Run FTedges() first")
                  }
                  
                  tempnodes <- object$df[ object$df$MS2scans != "", colnames(object$df)!= "id" ]
                  
                  tempnodes <- data.frame(id = seq(nrow(tempnodes)), tempnodes)
                  
                  tempedges <- object$edges[object$edges$cosine >= cosineThreshold,]
                  
                  tempedges$from <- sapply(tempedges$from, match, tempnodes$fixed__id)
                  tempedges$to <- sapply(tempedges$to, match, tempnodes$fixed__id)
                  
                  
                  tempedges <- na.omit(tempedges)
                  
                  
                  
                  g1 <- graph_from_data_frame(d=tempedges,
                                              vertices=tempnodes,
                                              directed=F) 
                  
                  V(g1)$id <- tempnodes$id
                  
                  # Removing loops from the graph:
                  g1 <- simplify(g1, remove.multiple = F, remove.loops = T) 
                  
                  #important for overview mode!
                  V(g1)$subcl <-  clusters(g1)$membership
                  
                  #make a fixed layout and add it to the graph in a way the NetworkModule will understand
                  if(is.null(V(g1)$x__coord) || is.null(V(g1)$y__coord)){
                      layo <- layout_components_qgraph(g1, qgraph::qgraph.layout.fruchtermanreingold)
                      
                      V(g1)$x__coord <- layo$layout[,1]
                      V(g1)$y__coord <- layo$layout[,2]
                  }
                  
                  res$graph <- g1
                  
                  
              },
              error = function(e){
                  #this assigns to object err in function environment,
                  #but err has to exist in the environment, otherwise
                  #will move through scopes up to global environment..
                  err$buildMseekGraph <<- paste(e)
                  
              },
              finally = {
                  p1 <- (proc.time() - p1)["elapsed"]
                  afterHash <- MseekHash(res)
                  
                  res <- addProcessHistory(res,
                                           FTProcessHistory(changes = afterHash != beforeHash,
                                                            inputDFhash = beforeHash,
                                                            outputDFhash = afterHash,
                                                            error = err,
                                                            sessionInfo = NULL,
                                                            processingTime = p1,
                                                            info = paste0("Generated MseekGraph from MseekFT object."),
                                                            param = FunParam(fun = "Metaboseek::buildMseekGraph",
                                                                             args = list(cosineThreshold = cosineThreshold),
                                                                             longArgs = list())
                                           ))
              })
              
              return(res)
              
          })

#' @aliases saveMseekGraph
#' 
#' @description \code{saveMseekGraph}: save a \code{MseekGraph} object to a
#'  file, registering the save event in the processHistory if the format allows
#'
#' @param file file path to write to
#' @param writeGraphML write object to a .graphML file (will drop metadata and some columns)
#' @param writeRDS write object to an .mskg (RDS) file; saves the entire object
#' 
#' @return the \code{MseekFT} object, with saving event added to processHistory
#' 
#' @importFrom igraph write_graph
#'   
#' @rdname MseekGraphs
#' @export
setMethod("saveMseekGraph", 
          "MseekGraph",
          function(object, file, writeGraphML = FALSE, writeRDS = TRUE){
              
              object <- addProcessHistory(object, FTProcessHistory(info = "Saved MseekFT object to a file.",
                                                                   param = FunParam(fun = "Metaboseek::saveMseekFT",
                                                                                    args = list(file = file,
                                                                                                writeGraphML = writeGraphML,
                                                                                                writeRDS = writeRDS))))
              #make sure file extension is .mskFT
              if(writeGraphML){
                  write_graph(object$graph,
                              paste0(gsub('\\.graphml$','',
                                          file, ignore.case = TRUE),
                                     ".graphML"),
                              format = "graphml")
                  
              }
              
              
              #make sure file extension is .mskFT
              if(writeRDS){
                  saveRDS(object,
                          file = paste0(gsub('\\.mskg$','',
                                             file, ignore.case = TRUE),
                                        ".mskg"))
              }
              
              
              invisible(object)
          })


#' @aliases loadMseekGraph
#' 
#' @description \code{loadMseekGraph}: load a \code{MseekGraph} object from an 
#' .mskg or .graphml file (file extensions inform how file is loaded in)
#' @rdname buildMseekFT
#' @export
setMethod("loadMseekGraph", 
          signature(object = "character"),
          function(object){
              
              if(length(object)>1){
                  lapply(object, loadMseekFT)
              }else{
                  
                  if(grepl("\\.graphml$", object, ignore.case = TRUE)){
                      
                      res <- constructFeatureTable(tablename = basename(gsub("\\.graphml$","", ignore.case = TRUE, object)))
                      res <- res[names(res) != "df"]
                      res$graph <- read_graph(object, "graphml")
                      
                      if(is.null(V(res$graph)$fixed__id)){
                          V(res$graph)$fixed__id <- seq(vcount(res$graph))
                      }
                      
                      #make a fixed layout and add it to the graph in a way the NetworkModule will understand
                      if(is.null(V(res$graph)$x__coord) 
                         || is.null(V(res$graph)$y__coord)){
                          layo <- layout_components_qgraph(res$graph,
                                                           qgraph::qgraph.layout.fruchtermanreingold)
                          
                          V(res$graph)$x__coord <- layo$layout[,1]
                          V(res$graph)$y__coord <- layo$layout[,2]
                          
                      }
                      
                      class(res) <- "MseekGraph"
                      
                  }
                      
                  if(grepl("\\.mskg$", object, ignore.case = TRUE)){
                      
                  
                  res <- readRDS(object)
                  
                  }
                  
                  res <- addProcessHistory(res, FTProcessHistory(info = paste0("Loaded MseekGraph object from file: ", object),
                                                                 sessionInfo = sessionInfo(),
                                                                 outputDFhash = MseekHash(res),
                                                                 param = FunParam(fun = "Metaboseek::loadMseekGraph",
                                                                                  args = list(file = object))))
                  
                  return(res)
              }
          })