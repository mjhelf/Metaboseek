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