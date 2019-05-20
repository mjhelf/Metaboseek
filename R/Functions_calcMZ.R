#''reformatSF
#'
#' Reformats a molecula formula string so that it has a count for each element, even if the count is 1.
#' 
#' @param s Molecula formula string
#'
reformatSF <- function(s){
  
  foundLL <- regexpr("[A-z][A-Z]|[A-z]$",s)
  
  if(foundLL == -1){
    return(s)
  }
  else{
    
    return( reformatSF(paste0(substr(s,1,foundLL),"1", substr(s,foundLL+1,nchar(s)))))
  }
  
}


#''consolidateSF
#'
#' Merges duplicates in an element count vector
#' 
#' @param x element count vector
#'
consolidateSF <- function(x){
  
  dups <- duplicated(names(x))
  
  if(!any(dups)){
    return(x)
  }
  
  eles <- unique(names(x))
  counts <- integer(length(eles))
  names(counts) <- eles
  
  for(i in names(counts)){
    
    counts[i] <- sum(x[names(x) == i])
    
  }
  
  return(counts)
  
}


#''makeSF
#'
#' Reformat a molecula formula string into an element count vector
#' 
#' @param s Molecula formula string
#'
makeSF <- function(s){
  
  s <- reformatSF(s)
  elnums <- strsplit(s, "[A-Z]|[A-Z][a-z]")[[1]]
  elnums <- as.integer(elnums[-1])
  names(elnums) <- strsplit(s, "-[0-9]+|[0-9]+")[[1]]
  return(consolidateSF(elnums))  
}

#''remakeSF
#'
#' Converts an element count vector back into a molecula formula string
#' 
#' @param s Molecula formula string
#'
remakeSF <- function(x){
  
  return(paste(names(x[x!=0]), x[x!=0], collapse = "", sep = ""))
  
}

#' getMono
#'
#' Get monoisotopic masses for a list of element count vectors
#' 
#' @param mf list of element count vectors
#' 
#' @importFrom enviPat check_chemform
#'
getMono <- function(mf){
  if(!is.list(mf)){
    
    mf <- list(mf)
    
  }
  
  plus <- check_chemform(isotopes,sapply(mf,function(mf1){remakeSF(mf1[mf1>0])} ))
  
  minus <- check_chemform(isotopes,sapply(mf,function(mf1){remakeSF(-mf1[mf1<0])} ))
  
  plus$monoisotopic_mass[plus$warning] <- 0
  minus$monoisotopic_mass[minus$warning] <- 0
  
  return(plus$monoisotopic_mass - minus$monoisotopic_mass)
  
}

#' calcMZs
#'
#' Calculate charge-to-mass ratios for molecula formulas in a data.frame, retaining the dataframes columns, but overriding columns mz, charge and ion
#' 
#' @param df data.frame that contains a column with molecular formulas (MFs)
#' @param charges vector of positive or negative integers indicating the charge states to calculate
#' @param carrier charge carrier - Molecular formula will be added to or removed from MFs for each charge
#' @param monoisotopic if true, will calculate the monoisotopic mass m/z values, will calculate m/z of the most abundant isotope peak otherwise (will be slower but preferrable e.g. for large organic molecules)
#' @param mf_column name of column in df that contains MFs
#' 
#' @importFrom enviPat check_chemform isopattern
#'
calcMZs <- function(df, charges = c(1), carrier = "H", monoisotopic = T, mf_column = "formula", adduct = NULL){
  
  
  if(charges[1] >= 0 ){
    
  inpforms <- paste0(df[[mf_column]], paste(rep(carrier, abs(charges[1])-length(adduct)), sep = "", collapse = ""), adduct)
  
    
  }else{
    
    inpforms <- sapply(paste0(df[[mf_column]], carrier, charges[1]+length(adduct), adduct), function(x){remakeSF(makeSF(x) )})
    
    
  }
  
  formulas <- check_chemform(isotopes,inpforms,get_sorted=FALSE,get_list=FALSE)
  
  if(monoisotopic){
    
    mzs <- (formulas$monoisotopic_mass[!formulas$warning] - 5.48579909070e-4*charges[1])/max(c(abs(charges[1]),1))
    
  }else{
    
      envires <- isopattern(isotopes, formulas$new_formula[!formulas$warning], threshold = 0, charge = charges[1], 
                        emass = 0.00054858, plotit = FALSE, algo=1, rel_to = 0, verbose = FALSE,
                        return_iso_calc_amount = FALSE)
  
  
  mzs <- sapply(envires, function(x){x[,"m/z"][which.max(x[,"abundance"])]})

    
  }
  


  newdf <- df[!formulas$warning,, drop = F]
  
  wmo <<- mzs
  wndf <<- newdf
  
  newdf$mz <- mzs
  newdf$charge <- charges[1]
  
  hnumber <- if(charges[1] > 0){charges[1] - length(adduct)}else{charges[1] + length(adduct)}
  newdf$ion <- paste0("[M",
               if(length(adduct)>0){paste0("+", adduct)}else{""},
               if(hnumber > 0){paste0("+",if(hnumber!=1){hnumber}else{""}, carrier,"]")}else if (hnumber < 0){paste0(if(hnumber!=-1){hnumber}else{"-"}, carrier,"]")}else{"]"},
               if(charges[1] > 0 ){paste0(charges[1],"+")}else if(charges[1] < 0){paste0(abs(charges[1]),"-")}else{""}  )
  
  #newdf$ion <- paste0("[M",  if(charges[1] > 0 ){"+"}else{"-"}, if(abs(charges[1]) > 1){abs(charges[1])}else{""}, if(abs(charges[1]) > 0){paste0(carrier, "]", abs(charges[1]), )}else{"]"})
  
  
  if(length(charges) > 1){
    
    return(do.call(rbind, list(a= newdf, b = calcMZs(df, charges = charges[-1], carrier, monoisotopic, mf_column)   )))
    
  }
  return(newdf)
  
}

