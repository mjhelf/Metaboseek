#' plotSMILE
#' 
#' draw single molecule from a SMILE
#' 
#' @param SMILE a character(1) SMILE term
#' @param ... additional arguments passed to \link[adsf]{view.image.2d}()
#' 
#' @export
plotSMILE <- function(SMILE, ...){
  
  mols <- rcdk::parse.smiles(SMILE)    
  par(mar=c(0,0,0,0)) # set margins to zero since this isn't a real plot
  plot(NA,NA,xlim=c(1,10),ylim=c(1,10),xaxt='n',yaxt='n',xlab='',ylab='', type = "n",  bty = "n") 
  temp1 = rcdk::view.image.2d(mols[[1]],...)    
  rasterImage(temp1,1,1,10,10, col = "black", bty = "n")  
}
