#' PlotWindow
#'
#' Initiate a plot and draw axes. Used by plotting functions in Metaboseek.
#' 
#' @return plots an empty coordinate system in the current plotting device
#'
#' @param cx character expansion factor (font size)
#' @param ylim numeric(2) of y-axis range
#' @param xlim numeric(2) of x-axis range
#' @param heading heading of the plot
#' @param single if TRUE, this plot is expected to be the only 
#' plot in a composite plot (different margin settings)
#' @param par if FALSE, par margin settings are not set inside 
#' the function and should be set outside
#' @param xlab x axis label
#' @param ylab y axis label
#' @param relto show y axis values relative to relto if not NULL.
#' @param ysci if TRUE, y axis label numbers are shown in scientific format
#' @param liwi line width for axes
#' @param textadj passed on to mtext adj for orientation 
#' of plot description/title text line
#' @param ylabshift integer to adjust the position of the y axis label
#'
PlotWindow <- function(cx = 1, 
                       ylim = c(0,1), 
                       xlim = c(0,1),
                       heading = "test",
                       single = F,
                       par = T,
                       relto = NULL,
                       ylab = "Intensity",
                       xlab = "RT (min)",
                       ysci = T,
                       liwi = 1,
                       textadj = 0.5,
                       ylabshift = 0
                       
){
  
  if(max(ylim)==0){ylim = c(0,1)}
  
  if(par){
    if(single){
      par(#mfrow=c(1,2),
        #oma=c(0,2,0,0),
        mar = c(5.1,6,4.1,0),#oma causes issues in interactive mode
        # mai=c(0,0.5,0,0),
        xpd=FALSE,
        bg=NA,
        xaxs = "i", yaxs = "i"
      )  
    }else{
      par(#mfrow=c(1,2),
        # oma=c(0,2,0,0),
        # mai=c(0,0.5,0,0),
        xpd=FALSE,
        bg=NA,
        xaxs = "i", yaxs = "i"
      )
    }
  }
  
  plot(numeric(),numeric(), type= "n", 
       ylim = ylim,
       xlim = xlim,
       axes=F, ylab="",xlab="")
  
  
  pn <- if(max(ylim)==0){1}else{5}
  #x axis
  axis(side=1, lwd=liwi, lwd.ticks = liwi, at = pretty(xlim),
       labels = format(pretty(xlim), scientific = F),
       mgp=c(0,0.4*cx,0), cex.axis=1*cx, xaxs = "i")
  #x-axis mgp[2] controls distance of tick labels to axis
  
  mtext(side=1, text= xlab, line=1.5*(1+(cx-1)/2), cex=par("cex")*cx*1)
  
  if(!is.null(relto) && relto != 1 ){
    axis(side=2,  las=2, at = pretty(ylim, n =pn),
         labels = format(pretty(ylim, n =pn), scientific = F),
         mgp=c(0,0.6,0), cex.axis=1*cx, lwd = liwi, lwd.ticks = liwi)
    #axis labels
    mtext(side=2, text= ylab,
          line=4*(1+(cx-1)/1.7)-ylabshift,
          cex=par("cex")*1*cx)
  }
  else{
    #y axis
    axis(side=2,  las=2, at = pretty(ylim, n =pn),
         labels = format(pretty(ylim, n =pn), scientific = ysci,digits = 3),
         mgp=c(0,0.6,0), cex.axis=1*cx, lwd = liwi, lwd.ticks = liwi)
    #axis labels
    mtext(side=2, text= ylab,
          line=4*(1+(cx-1)/1.7)-ylabshift,
          cex=par("cex")*1*cx)
  }
  
  #fix axis to not have gaps at edges
  abline(v=min(xlim), h=min(ylim), lwd = liwi)
  
  Hmisc::minor.tick(nx=2, ny=2,
                    tick.ratio=0.5, x.args = list(),
                    y.args = list())
  
  # text(max(xlim), max(ylim)+1.5*strheight("M"),
  #      labels = heading, bty="n",
  #      font = 2, cex=cx*1, adj = textadj)
  # 
 title(main=heading, line=2, cex.main = cx, adj = textadj)
}