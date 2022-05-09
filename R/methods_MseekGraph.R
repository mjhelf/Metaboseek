#' @include methods_MseekFamily.R


#' @rdname MseekGraphs
#' @aliases setLayout
#' 
#' @description \code{setLayout():} change the graph layout of an existing MseekGraph object.
#' 
#' @param object an \code{MseekGraph} object (\code{MseekFT} for \code{buildMseekGraph()})
#' @param layoutFunction function to use for layout of the resulting graph
#' 
#' @export
setMethod("setLayout", c("MseekGraph"),
          function(object,
                   layoutFunction = "qgraph::qgraph.layout.fruchtermanreingold"
          ){
            beforeHash <- MseekHash(object)
            
            p1 <- proc.time()
            err <- list()
            
            tryCatch({
              
              if(!is.MseekGraph(object)){
              stop("Input to setLayout is not an MseekGraph object")
              }
              
              #make a fixed layout and add it to the graph in a way the NetworkModule will understand
              object$graphLayouts <- layout_components_qgraph(object$graph, layoutFunction) # qgraph::qgraph.layout.fruchtermanreingold)
              
              V(object$graph)$x__coord <- object$graphLayouts$layout[,1]
              V(object$graph)$y__coord <- object$graphLayouts$layout[,2]
              
              
            },
            error = function(e){
              #this assigns to object err in function environment,
              #but err has to exist in the environment, otherwise
              #will move through scopes up to global environment..
              err$setLayout <<- paste(e)
              
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
                                                           info = paste0("Updated graph layout"),
                                                           param = FunParam(fun = "Metaboseek::setLayout",
                                                                            args = list(layoutFunction = layoutFunction),
                                                                            longArgs = list())
                                          ))
            })
            
            return(object)
            
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
          function(object, cosineThreshold = 0.6,
                   layoutFunction = "qgraph::qgraph.layout.fruchtermanreingold"
          ){
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
              g1 <- igraph::simplify(g1, remove.multiple = F, remove.loops = T) 
              
              #important for overview mode!
              V(g1)$subcl <-  clusters(g1)$membership
              
              #make a fixed layout and add it to the graph in a way the NetworkModule will understand
              # if(is.null(V(g1)$x__coord) || is.null(V(g1)$y__coord)){
              #   layo <- layout_components_qgraph(g1, eval(parse(text =  layoutFunction)))# qgraph::qgraph.layout.fruchtermanreingold)
              #   
              #   V(g1)$x__coord <- layo$layout[,1]
              #   V(g1)$y__coord <- layo$layout[,2]
              # }
              
              res$graph <- g1
              
              res <- setLayout(res, layoutFunction)
              
              
              
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
                                                        inputHash = beforeHash,
                                                        outputHash = afterHash,
                                                        error = err,
                                                        sessionInfo = NULL,
                                                        processingTime = p1,
                                                        info = paste0("Generated MseekGraph from MseekFT object."),
                                                        param = FunParam(fun = "Metaboseek::buildMseekGraph",
                                                                         args = list(cosineThreshold = cosineThreshold,
                                                                                     layoutFunction = layoutFunction),
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
            
            object <- addProcessHistory(object, FTProcessHistory(info = paste0("Saved MseekGraph object to file:", file),
                                                                 param = FunParam(fun = "Metaboseek::saveMseekFT",
                                                                                  args = list(file = file,
                                                                                              inputHash = MseekHash(object),
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
#' @param layoutFunction function to use for layout of the resulting graph
#' 
#' @description \code{loadMseekGraph}: load a \code{MseekGraph} object from an 
#' .mskg or .graphml file (file extensions inform how file is loaded in)
#' @rdname MseekGraphs
#' @export
setMethod("loadMseekGraph", 
          signature(object = "character"),
          function(object,
                   layoutFunction = "qgraph::qgraph.layout.fruchtermanreingold"
          ){
            
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
                
                # #make a fixed layout and add it to the graph in a way the NetworkModule will understand
                # if(is.null(V(res$graph)$x__coord) 
                #    || is.null(V(res$graph)$y__coord)){
                #   layo <- layout_components_qgraph(res$graph,
                #                                    eval(parse(text =  layoutFunction)))
                #   # qgraph::qgraph.layout.fruchtermanreingold)
                #   
                #   V(res$graph)$x__coord <- layo$layout[,1]
                #   V(res$graph)$y__coord <- layo$layout[,2]
                #   
                # }
                
                class(res) <- "MseekGraph"
                
                res <- setLayout(res, layoutFunction)
                
              }
              
              if(grepl("\\.mskg$", object, ignore.case = TRUE)){
                
                
                res <- readRDS(object)
                
                if(!length(res$graphLayouts)){
                res <- setLayout(res, layoutFunction)
                }
                
              }
              
              res <- addProcessHistory(res, FTProcessHistory(info = paste0("Loaded MseekGraph object from file: ", object),
                                                             sessionInfo = sessionInfo(),
                                                             outputHash = MseekHash(res),
                                                             param = FunParam(fun = "Metaboseek::loadMseekGraph",
                                                                              args = list(file = object,
                                                                                          layoutFunction = if(grepl("\\.mskg$", object, ignore.case = TRUE)){NULL}else{layoutFunction}))))
              
              return(res)
            }
          })



#' @rdname MseekGraphs
#' @description \code{simplify}: simplify an MseekGraph object, reducing the number of edges 
#' using additional filters.
#' 
#' @param maxK if length > 0, will only allow at most this many edges from each node
#' @param rankBy will use this edge attribute to rank keep only the top \code{maxK} edges; if length == 0, will use edges with lowest node in from column
#' @param cosineThreshold if length > 0, will remove all edges with a cosine value below this
#' @param layoutFunction function to use for layout of the resulting graph
#' @export
setMethod("simplify", c("MseekGraph"),
          function(object, rankBy = NULL,
                   maxK = 10,
                   cosineThreshold = NULL,
                   layoutFunction = "qgraph::qgraph.layout.fruchtermanreingold"
          ) {
            beforeHash <- MseekHash(object)
            
            p1 <- proc.time()
            err <- list()
            
            tryCatch({
              
              
              tables <- list(nodes = type.convert(as_data_frame(object$graph, "vertices"), as.is = T),
                             edges =  type.convert(as_data_frame(object$graph, "edges"), as.is = T))
              
              nedgesBefore <- nrow(tables$edges)
              
              
              if(length(cosineThreshold)){
                tables$edges <- tables$edges[tables$edges$cosine >= cosineThreshold,]
              }
              
              if(! "fixed__id" %in% colnames(tables$nodes)){
                tables$nodes$fixed__id <- tables$nodes[,1]
              }
              
              ####TEMPORARILY DISABLED
              # if(FALSE
              #    #input$mergeCheck
              # ){
              #     if(input$mergeCheck2){
              #         res <- list(tables = simplifyGraph(res$tables$nodes,
              #                                            res$tables$edges,
              #                                            which(res$tables$edges[[internalValues$colSelected1]] >= input$col1min
              #                                                  & res$tables$edges[[internalValues$colSelected1]] <= input$col1max
              #                                                  & res$tables$edges[[internalValues$colSelected2]] >= input$col2min
              #                                                  & res$tables$edges[[internalValues$colSelected2]] <= input$col2max
              #                                            )),
              #                     graph = NULL)
              #     }else{
              #         res <- list(tables = simplifyGraph(res$tables$nodes,
              #                                            res$tables$edges,
              #                                            which(res$tables$edges[[internalValues$colSelected1]] >= input$col1min
              #                                                  & res$tables$edges[[internalValues$colSelected1]] <= input$col1max)),
              #                     graph = NULL)
              #         
              #     }
              #     
              # }
              
              
              
              if(length(maxK)){
                #select edges to remove
                #  print("beforeremoval")
                if(!length(rankBy)){
                  rankBy <- 1
                }
                
                removeindices <- integer(0)
                for(i in unique(c(tables$edges[,1], tables$edges[,2]))){
                  
                  selall <- which(tables$edges[,1] == i | tables$edges[,2] ==i)
                  if(length(selall) > maxK){
                    
                    removeindices <- c(removeindices, selall[order(tables$edges[selall,rankBy])][1:max(c(1,(length(selall)-maxK)))])
                    
                  }
                  
                }
                if(length(removeindices) > 0){
                  tables$edges <- tables$edges[-unique(removeindices),]
                }
                
              }
              # print(res$tables$edges)
              #  print(res$tables$nodes)
              
              if(!length(object$edges)){
                object$edges <- tables$edges
              }
              
              
              g1 <- graph_from_data_frame(d=tables$edges, vertices=tables$nodes, directed=F) 
              
              
              #V(g1)$label <- V(g1)$parent.mass
              V(g1)$id <- seq(vcount(g1))
              
              # Removing loops from the graph:
              g1 <- igraph::simplify(g1, remove.multiple = F, remove.loops = T) 
              
              #important for overview mode!
              V(g1)$subcl <-  clusters(g1)$membership
              
              object$graph <- g1
 
              if(nedgesBefore != nrow(tables$edges)){
                
                object <- setLayout(object, layoutFunction)
                # 
                # layo <- layout_components_qgraph(g1, eval(parse(text =  layoutFunction))) #qgraph::qgraph.layout.fruchtermanreingold)
                # 
                # V(g1)$x__coord <- layo$layout[,1]
                # V(g1)$y__coord <- layo$layout[,2]
                
              }
              
              
              
              
            },
            error = function(e){
              #this assigns to object err in function environment,
              #but err has to exist in the environment, otherwise
              #will move through scopes up to global environment..
              err$simplify <<- paste(e)
              
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
                                                             info = paste0("Simplified network"),
                                                             param = FunParam(fun = "Metaboseek::simplify",
                                                                              args = list(rankBy = rankBy,
                                                                                          maxK = maxK,
                                                                                          cosineThreshold = cosineThreshold,
                                                                                          layoutFunction = layoutFunction),
                                                                              longArgs = list())
                                            ))
              })
            
            return(object)
            
          })

#' @rdname MseekGraphs
#' @description \code{limitComponents}: simplify an MseekGraph object, reducing the number of edges 
#' for any subgraph with more than n components.
#' 
#' @param n maximum number of nodes connected in a cluster
#' @param rankBy if length > 0, will use this edge attribute to iteratively remove the lowest scoring edges from the large clusters until
#' 
#' @param layoutFunction function to use for layout of the resulting graph
#' @param percentile remove edges in percentile steps
#' @export
setMethod("limitComponents", c("MseekGraph"),
          function(object, rankBy = "cosine",
                   n = Inf,
                   layoutFunction = "qgraph::qgraph.layout.fruchtermanreingold",
                   percentile = TRUE
          ) {
            beforeHash <- MseekHash(object)
            
            p1 <- proc.time()
            err <- list()
            
            tryCatch({
              g <- object$graph
              allClusters <- clusters(g, mode="weak")
              maxClusSize <- max(allClusters$csize)
              print(paste0("Largest cluster size: ", max(allClusters$csize), ". Increasing cosine threshold for large clusters..."))
              
              
              while(any(allClusters$csize > n)){
                if(maxClusSize > max(allClusters$csize)){
              print(paste0("Largest cluster size: ", max(allClusters$csize), ". Increasing cosine threshold for large clusters..."))
                  maxClusSize <- max(allClusters$csize)
                }
                  
              largeClusterNodes <- which(allClusters$membership %in% which(allClusters$csize > n))
              
              tables <- list(nodes = type.convert(as_data_frame(g, "vertices"), as.is = T),
                             edges =  type.convert(as_data_frame(g, "edges"), as.is = T))
              
              if(percentile){
              tables$edges <- tables$edges[-which(tables$edges$from %in% largeClusterNodes
                                           & tables$edges[[rankBy]] <= quantile(tables$edges[[rankBy]][tables$edges$from %in% largeClusterNodes], 0.01)#1.01*min(tables$edges[[rankBy]][tables$edges$from %in% largeClusterNodes])
                                           ),]
              }else{
              tables$edges <- tables$edges[-which(tables$edges$from %in% largeClusterNodes
                                                  & tables$edges[[rankBy]] < 1.01*min(tables$edges[[rankBy]][tables$edges$from %in% largeClusterNodes])
              ),]
              }
             # ie <- incident_edges(g, v = largeClusterNodes, mode = c("all"))
              
              g <- graph_from_data_frame(d=tables$edges, vertices=tables$nodes, directed=F) 
              
              allClusters <- clusters(g, mode="weak")
              }
              print(paste0("Success: Cluster sizes reduced. Largest cluster size: ", max(allClusters$csize)))
              
              
              nedgesBefore <- nrow(object$edges)
              
              object$edges <- type.convert(as_data_frame(g, "edges"), as.is = T)
              
              
              #V(g)$label <- V(g)$parent.mass
              V(g)$id <- seq(vcount(g))
              
              # Removing loops from the graph:
              g <- igraph::simplify(g, remove.multiple = F, remove.loops = T) 
              
              #important for overview mode!
              V(g)$subcl <-  clusters(g)$membership
              
              object$graph <- g
              
              if(nedgesBefore != nrow(object$edges)){
                
                object <- setLayout(object, layoutFunction)
                # 
                # layo <- layout_components_qgraph(g1, eval(parse(text =  layoutFunction))) #qgraph::qgraph.layout.fruchtermanreingold)
                # 
                # V(g1)$x__coord <- layo$layout[,1]
                # V(g1)$y__coord <- layo$layout[,2]
                
              }
              
              
              
              
            },
            error = function(e){
              #this assigns to object err in function environment,
              #but err has to exist in the environment, otherwise
              #will move through scopes up to global environment..
              err$simplify <<- paste(e)
              
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
                                                             info = paste0("Simplified network by limiting number of components"),
                                                             param = FunParam(fun = "Metaboseek::limitComponents",
                                                                              args = list(rankBy = rankBy,
                                                                                          n = n,
                                                                                          layoutFunction = layoutFunction),
                                                                              longArgs = list())
                                            ))
              })
            
            return(object)
            
          })
