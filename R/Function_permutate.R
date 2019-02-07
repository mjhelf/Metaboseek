#' permutate
#'
#' Use a data.frame with molecular formulas and combine them
#' 
#' @param modifiers data.frame with columns name, formula, min, max. 
#'
#' @details
#' columns min and max should be the minimum and maximum times a formula is allowed in the combinations, can be positive, negative, or 0.
#' formula: molecular formulas can contain positive or negative integers, but no parantheses or brackets.
#' The mass in the resulting data.frame is monoisotopic.
#'
permutate <- function(modifiers){
  
  recursiveC <- function(l){
    
    if (length(l) == 1){return(unlist(l, recursive = F))}
    
    l[[1]] <- sapply(l[[1]], function(l1,l2){
      
      return(lapply(l2, function(l2,l1){
        
        return(c(l1,l2))
        
      }, l1))
      
    }, l2 = l[[2]])
    
    
    return(recursiveC(l[-2]))
    
  }
  
  
  m1 <- lapply(modifiers$formula, makeSF)
  
  
  
  m2 <- mapply(function(m1l, min, max, name){
    
  res <-  lapply(seq(min,max), function(n, f){
      
      return(n*f)
      
    }, f = m1l)
    
  names(res) <- paste0(seq(min,max), "[", name,"]" )
  return(res)
    
  }, m1, modifiers$min, modifiers$max, modifiers$name)

  
  m3 <- lapply(recursiveC(m2), consolidateSF)
  
 
  
  modnames <- recursiveC(lapply(m2, function(l){as.list(names(l))})  ) 

 res <- data.frame(name = sapply(modnames, function(s){ paste(s, collapse = "_") }),
                   formula = sapply(m3, remakeSF),
                   mass = getMono(m3),
                   stringsAsFactors = F)
 
 return(res)
  
}
