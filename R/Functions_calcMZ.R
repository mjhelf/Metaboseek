#' calcMZs
#'
#' Calculate charge-to-mass ratios for molecula formulas in a data.frame, 
#' retaining the dataframes columns, but overriding columns mz, charge and ion
#' 
#' @return input data.frame with additional columns \code{ion} and \code{charge},
#'  and updated column \code{mz}
#' 
#' @param df data.frame that contains a column with molecular formulas (MFs)
#' @param charges vector of positive or negative integers indicating the charge 
#' states to calculate
#' @param carrier charge carrier - Molecular formula will be added to or removed 
#' from MFs for each charge
#' @param monoisotopic if true, will calculate the monoisotopic mass m/z values,
#' will calculate m/z of the most abundant isotope peak otherwise (will be 
#' preferrable e.g. for large organic molecules)
#' @param mf_column name of column in df that contains MFs
#' 
#' @importFrom MassTools calcIons
#'
calcMZs <- function(df, charges = c(1), carrier = "H",
                    monoisotopic = T,
                    mf_column = "formula", adduct = NULL){
  
  return(updateDF(MassTools::calcIons(df[[mf_column]],
                                      charges = charges, carrier = carrier,
                    monoisotopic = monoisotopic,
                    mf_column = mf_column,adduct = adduct),
           df[rep(seq(nrow(df)), length(charges)),, drop = F]))
  
}

