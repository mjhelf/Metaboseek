setGeneric("plotSpectrum", function(x, ...) standardGeneric("plotSpectrum"))


setClassUnion("matrixOrDF", c("matrix", "data.frame"))


setMethod("plotSpectrum", 
          signature = c("matrixOrDF"),
          function(x, ...){
            specplot2(x, ...)
          })

setMethod("plotSpectrum", 
          signature = c("Spectrum"),
          function(x, ...){
              plotSpectrum(matrix(c(x@mz,x@intensity), ncol = 2), ...)
          })

