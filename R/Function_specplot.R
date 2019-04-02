#' specplot
#' 
#' Plot an MS spectrum
#' 
#' @param x mz coordinates
#' @param y intensity coordinates
#' @param norm normalize by
#' @param cx font size
#' @param k top k intensity peaks will be labeled
#' @param fileName plot title
#' @param yrange y axis range
#' @param xrange x axis range
#' @param maxi max intensity to be plotted on side
#' @param labels data.frame containing at least x and y coordinates, plus optional columns: label and color
#' @param mar margins passed to par()
#' @param ylab y axis label
#' @param ylabshift shift horizontal position of y axis label 
#'
#' @importFrom TeachingDemos spread.labs
#' @importFrom Hmisc minor.tick
#' @export
specplot <- function (x=sc[,1],
                       y=sc[,2],
                       norm=max(y)/100,
                       cx=1.5,
                       k = 10,
                       fileName = "title",
                       yrange = c(0,100),
                       xrange = range(x),
                       maxi = max(y),
                       labels = NULL,
                       mar = c(4,6,6,2),
                       ylab = "Relative Intensity (%)",
                       ylabshift = 0
){
  
  pd <- data.frame(x=x,
                   y=y/norm)
  
  if(nrow(pd)>0){
    pd$label <- format(round(pd$x,5), nsmall = 5, scientific = F)
    pd$color <- "blue"
    
  }
  
  par(#oma=c(0,2,0,0), 
    mar = mar,#changed mar[2] to 6 because oma was removed because of issues with interactive view
    xpd = FALSE, xaxs = "i", yaxs = "i")
  PlotWindow(cx, 
             ylim = yrange, 
             xlim = xrange,
             heading = fileName,
             single = T,
             par = F,
             relto = norm,
             ylab,
             xlab = "m/z",
             textadj = 1,
             ylabshift = ylabshift
  )
  
  points(pd$x,pd$y,type="h", bty="n")
  #, axes=F,lwd=0.8,
  #     main=fileName, cex.main=0.5*cx, ann=FALSE, ylab="Relative intensity", 
  #    xlab= expression(italic(m/z)), 
  #   xaxs="i",yaxs="i",
  #  xlim=xrange,
  # ylim=yrange,
  # ...)
  
  
  
  currview <- pd[which(pd$y <= max(yrange)
                       & pd$y >= min(yrange)
                       & pd$x <= max(xrange) 
                       & pd$x >= min(xrange)),]
  
  
  
  if (length(currview$y) >= k){
    kn <-  sort(currview$y, decreasing = T)[k]
    labs <- currview[which(currview$y>=kn),]
  }else{
    labs <- currview}
  
  
  #merge the automatically annotated peaks with labels 
  if(!is.null(labels)){
    
    if(is.null(labels$color)){
      labels$color <- "red" 
    }
    
    
    if(is.null(labels$label)){
      labels$label <- format(round(labels$x,5), nsmall = 5, scientific = F)
    }
    
    
    labs <- rbind(labels, labs)
    
    labs <- labs[!duplicated(round(labs$x,5)),]
  }
  
  
  if(nrow(labs) > 0 ){
    par(xpd=NA)
    labs$xcorr <- suppressWarnings({spread.labs(labs[,1],1.05*strwidth("A"), maxiter=1000, min=min(labs[,1]), max=max(labs[,1]))})
    
    segments(labs[,1],labs[,2]+0.01*max(yrange),labs$xcorr,labs[,2]+0.05*max(yrange), col="olivedrab4", lwd=0.8)
    
    text(labs$xcorr,labs[,2]+0.055*max(yrange),labels=labs$label, col=labs$color, srt=90,adj=c(0,0.3), cex=1*cx)
    text(min(xrange), max(yrange)+1.5*strheight("M"),
         labels = format(maxi*(max(labs$y)/100), scientific = T, digits =4), bty="n",
         font = 2, cex=cx*1)
    
    # title(main=format(maxi*(max(labs$y)/100), scientific = T, digits =4),
    #       line=2, cex.main = cx,
    #       adj = 0,
    #       outer = F)
    
  }
  
  # mtext(side=1, text= expression(italic(m/z)), line=0.7, cex=0.5*cx)
  #  mtext(side=2, text="Relative intensity (%)", line=1.1, cex=0.5*cx)
  # mtext(side=1, text=fileName, line=1.2, cex=0.5*cx)
  #mtext(side=3, text=Ptext, line=0.6, cex=0.5, adj=1)
  #par(cex.axis=0.5*cx, tcl=-0.3)            
  #axis(side=1, lwd=1, minor.tick(nx=10,ny=5, tick.ratio=0.5), mgp=c(0.5,0,0)) #x-axis mgp[2] controls distance of tick labels to axis
  #axis(side=2, lwd=1, las=2, mgp=c(0.5,0.4,0)) #y-axis
}