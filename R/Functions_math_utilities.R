#' gm_mean
#' 
#' Adapted from: https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
#' Ignores 0 and NA values!
#' 
#' @param x numeric vector of positive values
#' 
#'
#'
gm_mean <- function(x){
  #avoiding multiplication of all vector values here to avoid overflow (too large numbers)
  exp(sum(log(x[x > 0]), na.rm=TRUE) / sum(x > 0 & !is.na(x)))
}
