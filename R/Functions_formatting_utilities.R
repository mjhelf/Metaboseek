#' ListToReactiveValues
#' 
#' recursively converts lists to reactiveValues, essentially the reverse of 
#' \code{\link[shiny]{reactiveValuesToList}()}. Has to be called in a shiny
#'  session.
#' 
#' @param ls a list object
#' 
#' @importFrom shiny reactiveValues is.reactivevalues
#' 
#' @return a \code{\link[shiny]{reactiveValues}} object
#' 
#' @export
ListToReactiveValues <- function(ls){
  
  #note: 
  if(!is.list(ls) 
     || is.data.frame(ls)
     || (is.list(ls) & length(names(ls)[names(ls)!=""] ) != length(ls) & !is.reactivevalues(ls)) ){
    return(ls)
  }
  
  re <- reactiveValues()
  for (i in names(ls)){
    re[[i]] <- ListToReactiveValues(ls[[i]])
  }
  return(re)
  
}

#' reactiveValuesToListRec
#' 
#' recursively converts reactiveValues to lists, essentially a recursive version of 
#' \code{\link[shiny]{reactiveValuesToList}()}. Has to be called in a shiny
#'  session.
#' 
#' @param x a \code{\link[shiny]{reactiveValues}} object
#' 
#' @importFrom shiny reactiveValues is.reactivevalues
#' 
#' @return a list object
#' 
#' @export
reactiveValuesToListRec <- function(x){
    
    #note: 
    if(!is.reactivevalues(x)){
        return(x)
    }
    
    re <- reactiveValuesToList(x)
    class(re) <- "reactiveValuesInList"
    for (i in names(re)){
        re[[i]] <- reactiveValuesToListRec(re[[i]])
    }
    return(re)
    
}

#' saveMseekSession
#' 
#' save the current Metaboseek session
#' 
#' @param values a \code{\link[shiny]{reactiveValues}} object that in effect 
#' gives read and write access to external objects
#' @param path file path to save to. if NULL, the MseekSession object is returned
#' @param MSData if TRUE, MSData will be included in saved file. If false, only 
#' MS data file paths will be saved
#' 
#' @return an MseekSession object
#' 
#' @export
saveMseekSession <- function(values, path = NULL, MSData = T){
    
    isolate({
    savedValues <- reactiveValuesToListRec(values)
    
    class(savedValues) <- "MseekSession"
    
    if(!MSData){
        savedValues$MSData$MSnExp <- NULL
        savedValues$MSData$data <- names(values$MSData$data)
    }
    
    if(!is.null(path)){
    saveRDS(savedValues, path)
    }else{
    return(savedValues)
    }
    })
}


#' reconstructValues
#' 
#' reconstructs the MseekTree (values object) from a saved values object (that 
#' contains all data to be loaded as lists rather than reactiveValues).
#' 
#' @param values an MseekTree (reactivevalues) object
#' @param savedValues a saved values object (created by applying
#'  \code{reactiveValuesToListRec()} to an MseekTree object)
#' 
#' @return nothing, but modifies the global values object
#' 
#' @export
reconstructValues <- function(values, savedValues){
    
    for(n in names(savedValues)){
        
        if(n %in% names(values) && is.reactivevalues(values[[n]])){
            
            reconstructValues(values[[n]], savedValues[[n]])
            
        }else{
            values[[n]] <- savedValues[[n]]
        }
        
    }
}

#' loadMseekSession
#' 
#' load a Metaboseek session
#' 
#' @param values a \code{\link[shiny]{reactiveValues}} object that in effect 
#' gives read and write access to external objects 
#' @param savedValues an MseekSession object or a file path to an MseekSession
#' saved as RDS file.
#' 
#' @return nothing, but modifies the global values object
#' 
#' @export
loadMseekSession <- function(values, savedValues){
    
    isolate({
        if(is.character(savedValues)){
        savedValues <- readRDS(savedValues)
        }
        
        if(is.character(savedValues$MSData$data)){
            savedValues$MSData$MSnExp <- MSnbase::readMSData(savedValues$MSData$data, pdata = NULL, verbose = F,
                                                        centroided. = T,
                                                        smoothed. = NA, mode = "onDisk")
            savedValues$MSData$data <- loadRawM(savedValues$MSData$data,
                                                workers = values$GlobalOpts$enabledCores)
            
            
            }
        
        reconstructValues(values, savedValues)
    })
}


#' checkFolders
#'
#' Looks for folders as specified in \code{query}
#' 
#' @return a named character vector of the folders in \code{query} which exist.
#' 
#' @param query character vector with folders to search for,
#'  by default looks for drives in Windows file system
#'
checkFolders <- function(query = paste0(LETTERS,":/")){
  
  out <- character(0)
  
  for(i in query){
    if(file.exists(i)){
      out[[gsub(":/","",i)]] = i
    }
  }
  
  return(out)
}

#' Make filenames for exported .csv or .pdf files
#' 
#' TODO: reimplement this
#' Generate a filename from project name and filter criteria
#' 
#' @param projectName ProjectName used as prefix
#' @param FT Mseek's featureTable reactiveValues, as returned by 
#' \code{\link{FeatureTable}(values)} (or a list with same structure)
#'
#' @return a character string with an informative filename that includes 
#' filter criteria
#'
filenamemaker <- function(projectName,
                          FT){
  
  titleout <- paste(projectName, 
                    names(FT$index[which(FT$index == FT$active)]),
                    sep = "_")
  
  
  for(f in FT$tables[[FT$active]]$filters$filters){
    
    if(!is.atomic(f) && f$active){
      titleout <- paste(titleout,f$column,f$minSel, f$maxSel, f$txtSel, sep = "_")
    }
    
    
  }
  return(gsub(".csv$","",gsub("_$","",titleout)))
  
}


#' Get common root folder of file paths
#' 
#' From: https://rosettacode.org/wiki/Find_common_directory_path#R
#' 
#' @param paths vector of paths
#' @param delim folder delimiter
#'
#' @return sting with the common root folder of all supplied \code{paths}
#'
#' @export
get_common_dir <- function(paths, delim = "/")
{
  if(!length(paths)){
    simpleError("cannot find common dir on an object of length 0.")
    }
  if(length(unique(dirname(paths))) == 1){
    return(dirname(paths)[1])
  }else{
    
    path_chunks <- strsplit(paths, delim)
    
    i <- 1
    repeat({
      current_chunk <- sapply(path_chunks, function(x) x[i])
      if(any(current_chunk != current_chunk[1])) break
      i <- i + 1
    })
    return(paste(path_chunks[[1]][seq_len(i - 1)], collapse = delim))
  }
}


#' Mseek.colors
#' 
#' custom color spectrum using color brewer Set1 colors plus topo.colors; 
#' good color discrimination up to n = 13
#' 
#' @param n number of colors
#' @param alpha transparency
#' 
#' @return a character vector representing \code{n} colors.
#' 
#' @export
Mseek.colors<- function (n, alpha){
  
  if(is.null(alpha)){
    alphahex <- ""
  }else{
  alphahex <- as.hexmode(as.integer(alpha*255))
  if(nchar(alphahex) == 1){alphahex <- paste0("0",alphahex)}
  alphahex <- toupper(alphahex)
  }
  
  base <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
            "#FFFF33","#A65628","#F781BF","#999999","#1FFFB4","#000000")
  
  
  
  if(n<=11){
    return(paste0(base[1:n],alphahex))
  }else{
      extended.colrange <- topo.colors(n = n-11, alpha = alpha)
    return(c(paste0(base[1:11],alphahex),extended.colrange))
  }
  
  
}