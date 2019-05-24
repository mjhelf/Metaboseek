
plotSpectrum <- function(x, ...){
    
    specplot2(x, ...)
    
}

setMethod("plotSpectrum", 
          signature = c("Spectrum"),
          function(x, ...){
              plotSpectrum(matrix(c(x@mz,x@intensity), ncol = 2), ...)
          })

setMethod("plotSpectrum", 
          signature = c("matrix"),
          function(x, ...){
              plotSpectrum(x, ...)
          })

setMethod("plotSpectrum", 
          signature = c("data.frame"),
          function(x, ...){
              plotSpectrum(x, ...)
          })