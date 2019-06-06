#' massquery
#' 
#' 
#' get molecular formula predictions from \url{http://chemcalc.org}
#' 
#' @param mz m/z value
#' @param range maximum absolute error
#' @param ppm maximum relative error in ppm
#' @param elem String containing information on which elements are allowed for prediction
#' @param charge charge state (positive or negative integer)
#' @param IntegerSaturation if true, only allows full integers for unsaturation
#' @param minUnsat minimum unsaturation
#' @param maxUnsat maximum unsaturation
#' 
#' @references 
#' \enumerate{
#' \item ChemCalc: a building block for tomorrow’s chemical infrastructure. Patiny, Luc; Borel, Alain Journal of Chemical Information and Modeling 2013.
#'}
#' 
#' 
#' @export
massquery <- function(mz, range=0.01, ppm=5,
                      elem= "C0-100H0-202N0-10O0-10F0-3Cl0-3Br0-1",
                      charge = 1,
                      IntegerSaturation = FALSE,
                      minUnsat = 0,
                      maxUnsat = 40){
  
  charge <- as.integer(charge)
  if (charge > 0 ){charge2 <- paste0("%2B",abs(charge))}
  if (charge < 0 ){charge2 <- paste0("-",abs(charge))}
  if (charge == 0 ){charge2 <- paste0("")}
  
  IntegerSaturation <-  switch(EXPR = {as.character(IntegerSaturation)},
                               "FALSE" = "false", "TRUE" = "true")
  
  if (!is.null(ppm)){range <- as.numeric(mz)*ppm*1e-6}             
  
  mzq <-  paste0("http://www.chemcalc.org/service?action=em2mf&monoisotopicMass=",
                 mz,"&massRange=",range,"&mfRange=",
                 elem,
                 "(",charge2,")",
                 "&integerUnsaturation=",
                 IntegerSaturation,
                 "&minUnsaturation=",
                 minUnsat,
                 "&maxUnsaturation=",
                 maxUnsat)
  
  res <- jsonlite::fromJSON(mzq)
  
  if(length(res$results) == 0){
    return(data.frame(em = "", mf= "", unsat = "",
                      error = "", ppm = "", stringsAsFactors = F))
  }
  
  return(res$results)}
