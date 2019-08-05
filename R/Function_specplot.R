#' parseTitle
#'
#' parse a scanInfo table into a title for a mass spectrum plot
#' 
#' @return a character vector of length 1.
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
                       paste0("\nParent m/z: ",
                              format(round(scanInfo$precursorMZ,5), 
                                     nsmall = 5, scientific = F))}else{""},
                   collapse = " ")
        )
        
        
        
    }
    
    if(all(scanInfo$msLevel == scanInfo$msLevel[1])){
        return(
            paste0(
                if(length(unique(scanInfo$filename)) == 1){
                    paste0(scanInfo$filename[1],"#",
                           min(scanInfo$acquisitionNum),"-",
                           max(scanInfo$acquisitionNum),
                           "[",nrow(scanInfo)," scans]")
                }else{
                    paste0("[",length(unique(scanInfo$filename)),
                           " files][",nrow(scanInfo)," scans]")
                },
                "(",
                round(min(scanInfo$retentionTime)/60,3),
                " - ",
                round(max(scanInfo$retentionTime)/60,3),
                " min)",
                if(scanInfo$msLevel[1] > 1){
                    paste0("\nParent m/z: ",
                           round(mean(scanInfo$precursorMZ),5))}else{""}
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
#' @param spectrum matrix or data.frame, where the first column is m/z values,
#'  and the second column are intensity values
#' @param norm normalize by
#' @param cx font size
#' @param k top k intensity peaks will be labeled 
#' automatically with their mz values
#' @param fileName plot title
#' @param yrange y axis range
#' @param xrange x axis range
#' @param maxi max intensity to be plotted on side.
#' @param labels data.frame containing at least x and y coordinates,
#'  plus optional columns: label and color
#' @param highlights matrix or data.frame, see \code{details}
#' @param mar margins passed to par()
#' @param ylab y axis label
#' @param ylabshift shift horizontal position of y axis label 
#' @param parseLabels if TRUE, the \code{label} column in \code{labels} will
#'  be parsed (using \code{\link[base]{parse}})
#' 
#' @details 
#' \describe{
#' \item{highlights}{matrix or data.frame with at the first two columns 
#' containing x and y coordinates of peaks to highlight.
#' If \code{highlights} has an additional column called \code{color},
#'  that column is expected to contain color values for the highlights.
#' If \code{highlights} has an additional column called \code{type},
#'  that column is expected to contain point \code{type} values for the highlights,
#'  as detailed in \link[graphics]{plot.default}.}
#'  \item{labels}{
#'  The first two columns of this data.frame will be assumed to be mz and intensity values, respectively.
#'  If it only contains two columns, the first column values will be used as label text.
#'  If a \code{label} column is present, that column will be used as peak labels.
#'  If a \code{colors} column is present, that column will be used to define
#'   the label color, otherwise it will be red.
#'  }
#'  \item{parseLabels}{
#'  Parsing labels can be used to include subscript and
#'   superscript elements in peak labels, but requires careful preparation
#'    of the strings to be passed to \code{\link[base]{parse}}
#'  }
#' }
#'
#' @return plots a spectrum view in the current plotting device
#'
#' @importFrom TeachingDemos spread.labs
#' @importFrom Hmisc minor.tick
#' @export
specplot2 <- function (x,
                       y,
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
            if(is.null(maxi)){maxi <- if(!any(sel) 
                                         || length(sel) == 0){100}else{max(spectrum[sel,2, drop = F])}}
            
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
               type= if("type" %in% colnames(highlights)){
                   highlights[,"type"]}else{"h"}, 
               col = if("color" %in% colnames(highlights)){
                   highlights[,"color"]}else{"#20BACE80"},
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
        
        spectrum$label <- format(round(spectrum[,1],5),
                                 nsmall = 5, scientific = F)
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
                labels$label <- format(round(labels[,1],5), 
                                       nsmall = 5, scientific = F)
            }
            
            if(is.null(labels$color)){
                labels$color <- "red" 
            }
            
            if(nrow(spectrum) == 0){
                
                spectrum <- labels
                
            }else{
                
                colnames(labels)[1:2] <- colnames(spectrum)[1:2]
                
                spectrum <- rbind(labels[,c(colnames(labels)[1:2],
                                            "label",
                                            "color")],
                                  spectrum[,c(colnames(labels)[1:2],
                                              "label",
                                              "color")])
                
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
        
        
        xcorr <- suppressWarnings({spread.labs(spectrum[,1],
                                               1.13*strwidth("A", cex = cx),
                                               maxiter=1000,
                                               min=min(spectrum[,1]),
                                               max=max(spectrum[,1]))})
        
        segments(spectrum[,1],spectrum[,2]+0.01*max(yrange),
                 xcorr,spectrum[,2]+0.05*max(yrange), col="olivedrab4", lwd=0.8)
        
        
        
        if(parseLabels){filabs <- parse(text = spectrum$label)}else{filabs <- spectrum$label}
        
        text(xcorr,spectrum[,2]+0.055*max(yrange),
             labels=filabs, col=spectrum$color, 
             srt=90,adj=c(0,0.3), cex=1*cx)
        
    }
    
    #show maximum intensity value at top left corner
    text(min(xrange), max(yrange)+1.5*strheight("M"),
         labels = format(maxi, scientific = T, digits =4), bty="n",
         font = 2, cex=cx*1)
    
}