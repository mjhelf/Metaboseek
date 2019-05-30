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
  if(!is.null(labels) && nrow(labels)>0){
    labels$y <- labels$y/norm
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

#' parseTitle
#'
#' parse a scanInfo table into a title for a mass spectrum plot
#'
#' @param scanInfo a data.frame as returned by \link{getScanInfo}()
#'
#' @export
parseTitle <- function(scanInfo){
    
    
    if(is.null(scanInfo) ||nrow(scanInfo) == 0){
        
        return("") 
    }
    
    if(nrow(scanInfo) ==1){
        
        return(
            paste0(basename(scanInfo$filename), "#",
                   scanInfo$acquisitionNum,
                   " (", round(scanInfo$retentionTime/60,3), " min)",
                   if(scanInfo$precursorMZ != 0){
                       paste0("\nParent m/z: ", round(scanInfo$precursorMZ,5))}else{""},
                   collapse = " ")
        )
        
        
        
    }
    
    if(all(scanInfo$msLevel == scanInfo$msLevel[1])){
        return(
            paste0(
                if(length(unique(scanInfo$filename)) == 1){
                    paste0(scanInfo$filename[1],"#", min(scanInfo$acquisitionNum),"-",
                           max(scanInfo$acquisitionNum),"[",nrow(scanInfo)," scans]")
                }else{
                    paste0("[",length(unique(scanInfo$filename))," files][",nrow(scanInfo)," scans]")
                },
                "(",
                round(min(scanInfo$retentionTime)/60,3),
                " - ",
                round(max(scanInfo$retentionTime)/60,3),
                " min)",
                if(scanInfo$msLevel[1] > 1){
                    paste0("\nParent m/z: ", round(mean(scanInfo$precursorMZ),5))}else{""}
            )
        )
    }
    
    return("Mixed different MS levels")
    
}



#' specplot2
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
#' @param maxi max intensity to be plotted on side.
#' @param labels data.frame containing at least x and y coordinates,
#'  plus optional columns: label and color
#' @param highlights matrix or data.frame with at the first two columns containing x and y coordinates of peaks to highlight.
#' If \code{highlights} has an additional column called \code{color},
#'  that column is expected to contain color values for the highlights.
#' If \code{highlights} has an additional column called \code{type},
#'  that column is expected to contain point \code{type} values for the highlights,
#'  as detailed in \link[graphics]{plot.default}.
#' @param mar margins passed to par()
#' @param ylab y axis label
#' @param ylabshift shift horizontal position of y axis label 
#'
#' @importFrom TeachingDemos spread.labs
#' @importFrom Hmisc minor.tick
#' @export
specplot2 <- function (x=sc[,1],
                       y=sc[,2],
                       spectrum,
                       norm= NULL,
                       cx=1,
                       k = 10,
                       fileName = "",
                       yrange = NULL,
                       xrange = NULL,
                       maxi = NULL,
                       labels = NULL,
                       highlights = NULL,
                       mar = c(3,4,6,1),
                       ylab = "Relative Intensity (%)",
                       ylabshift = 2,
                       parseLabels = F
){
    if(!missing(spectrum) && !is.null(spectrum)){
        
        
        
        if(is.null(xrange)){
            xrange <- range(spectrum[,1])
            sel <- TRUE
        }else{
            sel <- spectrum[,1] <= max(xrange) & spectrum[,1] >= min(xrange)
        }
        
        if(nrow(spectrum) == 0){
            maxi <- 100
            norm <- 1
        }else{
            
            #by default, scale largest y value in view to 100,
            #to scale globally, set maxi to max(y) externally   
            if(is.null(maxi)){maxi <- if(!any(sel) || length(sel) == 0){100}else{max(spectrum[sel,2, drop = F])}}
            
            if(!any(sel) || length(sel) == 0 || maxi == 0){
                norm = 1 
            }
            
            if(is.null(norm)){
                norm = maxi/100
            }
            
            spectrum <- spectrum[sel,, drop = F]
        }
        
        spectrum[,2] <- spectrum[,2]/norm
        
    }else{
        
        if(is.null(xrange)){xrange = range(x)}
        
        sel <- x <= max(xrange) & x >= min(xrange)
        
        
        #by default, scale largest y value in view to 100,
        #to scale globally, set maxi to max(y) externally   
        if(is.null(maxi)){maxi <- if(length(sel) == 0){1}else{max(y[sel])}}
        
        if(length(sel) == 0 || maxi == 0){
            norm = 1 
        }
        
        if(is.null(norm)){
            norm = maxi/100
        }
        
        spectrum <- data.frame(x=x[sel],
                               y=y[sel]/norm)
        
        
        
    }
    
    if(is.null(yrange)){yrange = c(0, maxi/norm)}
    
    
    par(#oma=c(0,2,0,0), #oma causes issues with selectcallback
        mar = mar,
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
               ysci = F,
               textadj = 1,
               ylabshift = ylabshift
    )
    
    if(!is.null(highlights)){
        
        highlights[,2] <- highlights[,2]/norm
        
        points(highlights[,1],highlights[,2],
               type= if("type" %in% colnames(highlights)){highlights[,"type"]}else{"h"}, 
               col = if("color" %in% colnames(highlights)){highlights[,"color"]}else{"#20BACE80"},
               bty="n", lwd = 3, lend = 1)
        
        
    }  
    
    #plotting the actual spectrum data points
    points(spectrum[,1],spectrum[,2],type="h", bty="n", lwd = 1)
    
    
    
    #reduce spectrum to only peaks within yrange to add labels:
    spectrum <- spectrum[which(spectrum[,2] <= max(yrange)
                               & spectrum[,2] >= min(yrange)
    ),, drop = F]
    
    if(nrow(spectrum) > k){
        kn <-  order(spectrum[,2], decreasing = T)[seq(k)]
        spectrum <- spectrum[kn,, drop = F]
    }
    
    if(nrow(spectrum)>0){
        if(!is.data.frame(spectrum)){
            spectrum <- as.data.frame(spectrum, stringsAsFacotrs = F)
        }
        
        spectrum$label <- format(round(spectrum[,1],5), nsmall = 5, scientific = F)
        spectrum$color <- "blue"
        
    }
    
    
    
    
    #merge the automatically annotated peaks with labels 
    if(!is.null(labels) 
       && nrow(labels)>0){
        labels[,2] <- labels[,2]/norm
        labels <- labels[which(labels[,1] <= max(xrange)
                               & labels[,1] >= min(xrange)
                               & labels[,2] <= max(yrange)
                               & labels[,2] >= min(yrange)
        ),, drop = F]
        if(nrow(labels)>0){
            
            if(is.null(labels$label)){
                labels$label <- format(round(labels[,1],5), nsmall = 5, scientific = F)
            }
            
            if(is.null(labels$color)){
                labels$color <- "red" 
            }
            
            if(nrow(spectrum) == 0){
                
                spectrum <- labels
                
            }else{
                
                colnames(labels)[1:2] <- colnames(spectrum)[1:2]
                
                spectrum <- rbind(labels[,c(colnames(labels)[1:2], "label", "color")], spectrum[,c(colnames(labels)[1:2], "label", "color")])
                
                spectrum <- spectrum[!duplicated(round(spectrum[,1],5)),]
            }
        }
    }
    
    if(nrow(spectrum) > 0 ){
        par(xpd=NA)
        # wordcloud::textplot(spectrum[,1], spectrum[,2],
        #                     spectrum$label, cex=cx,new=FALSE,
        #                     show.lines=TRUE,
        #                     xlim = xrange,
        #                     ylim = yrange)#, col=spectrum$color, rotate90 = T)
        
        
        xcorr <- suppressWarnings({spread.labs(spectrum[,1],1.08*strwidth("A", cex = cx), maxiter=1000, min=min(spectrum[,1]), max=max(spectrum[,1]))})
        
        segments(spectrum[,1],spectrum[,2]+0.01*max(yrange),xcorr,spectrum[,2]+0.05*max(yrange), col="olivedrab4", lwd=0.8)
        
        
        
        if(parseLabels){filabs <- parse(text = spectrum$label)}else{filabs <- spectrum$label}
        
        text(xcorr,spectrum[,2]+0.055*max(yrange),labels=filabs, col=spectrum$color, srt=90,adj=c(0,0.3), cex=1*cx)
        
    }
    
    #show maximum intensity value at top left corner
    text(min(xrange), max(yrange)+1.5*strheight("M"),
         labels = format(maxi, scientific = T, digits =4), bty="n",
         font = 2, cex=cx*1)
    
}