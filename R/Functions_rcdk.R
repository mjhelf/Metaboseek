#' plotSMILE
#' 
#' draw single molecule from a SMILE
#' 
#' @param SMILE a character(1) SMILE term
#' @param ... additional arguments passed to \link[rcdk]{view.image.2d}()
#' 
#' @return plots a molecule in the current plotting device
#' 
#' @export
plotSMILE <- function(SMILE, ...){
  tryCatch({
  mols <- rcdk::parse.smiles(SMILE)    
  par(mar=c(0,0,0,0)) # set margins to zero since this isn't a real plot
  plot(NA,NA,xlim=c(1,10),ylim=c(1,10),xaxt='n',yaxt='n',xlab='',ylab='', type = "n",  bty = "n") 
  temp1 = rcdk::view.image.2d(mols[[1]],...)    
  rasterImage(temp1,1,1,10,10, col = "black", bty = "n")  
}, error = function(e){
    plot(numeric(0),
         numeric(0),
         ylim = c(0,1),
         xlim = c(0,1),
         type = "n", ann = FALSE, bty = "n", axes = F, asp = 1)
    
    text(0.5,0.5, labels = "Please install package rcdk to plot molecular structures.", adj = 0.5)  
    text(0.5,0.3, labels = "Make sure Java is installed (must be 64-bit if you are running 64-bit R).", adj = 0.5)  
    
    })
  
  }
