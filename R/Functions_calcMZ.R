#' calcMZs
#'
#' Calculate charge-to-mass ratios for molecula formulas in a data.frame, 
#' retaining the dataframes columns, but overriding columns mz, charge and ion
#' 
#' @return input data.frame with additional columns \code{ion} and \code{charge},
#'  and updated column \code{mz}
#' 
#' @param df data.frame that contains a column with molecular formulas (MFs)
#' @param mf_column name of column in df that contains MFs
#' @param ... additional arguments passed to \code{\link[MassTools]{calcIons}()}
#' 
#' @importFrom MassTools calcIons
#'
calcMZs <- function(df, 
                    mf_column = "formula",
                    ...){
  
    ions <- MassTools::calcIons(df[[mf_column]],
                                mf_column = mf_column, ...)
    
  return(updateDF(ions,
           df[rep(seq(nrow(df)), nrow(ions)/nrow(df)),, drop = F]))
  
}

