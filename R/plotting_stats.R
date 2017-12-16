#library(BiocGenerics)
#library(grDevices)
#library(graphics)
#library(stats)


#' densplot
#' 
#' 
#' generate a density plot for numeric data.
#' 
#' To accomodate logarithmized data,
#' Inf values will be set to 1.1*largest value that is not Inf
#' -Inf values will be set to 0.9*smallest value that is not -Inf
#' 
#' @param densin numeric vector or matrix
#' @param perc numeric(): draw lines at these quantiles
#' 
#' @export
densplot <-function(densin = log10(as.numeric(unlist(filtrate3[,scol]))),#filtrate3$rt,#input values
                    perc = c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 0.95, 0.99),
                    ... #pass arguments to plot()
){
    
    #densin <- log10(filtrate3$maxfold)
    densin[densin==Inf] <- 1.1*max(densin[densin!=Inf])
    densin[densin==-Inf] <- 0.9*min(densin[densin!=-Inf])
    densin <- na.omit(densin)
    
    dens <- BiocGenerics::density(densin,from=min(densin),to=max(densin), cut=0, n=4096, na.rm = T)
    
    #dens$x[is.infinite(dens$x)] <- 1*max(dens$x[dens$x!=Inf])
    plot(dens, type= "l", ...)
    
    # perc <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 0.95, 0.99)
    
    quan <- stats::quantile(densin,perc)
    
    ##plotquantile lines and legend
    colr <- grDevices::rainbow(length(perc), s = 1, v = 1, start = 0, end = max(1, length(perc) - 1)/length(perc), alpha = 0.5)
    graphics::segments(quan,min(dens$y),quan,max(dens$y), col=colr, lwd=0.8)
    legendtext <- paste0(perc,": ",round(quan,4) )
    graphics::legend("topright", inset=c(0,0.03*max(dens$y)),legendtext, lty=1,lwd=2.5, col=colr, bty="n",  cex=.5)
}


#' Plot summary data on grouped data
#' 
#' @param ... arguments passed to ggplot2::ggplot
#' @param main plot type
#' @param dotplot boolean whether or not to plot individual values as dots
#' @param mark select a value to be plotted
#' @param errorbar select type of error bar
#' @param rotate boolean whether or not to rotate x-axis labels
#' 
#' 
#' @import ggplot2
#' @export
groupedplot <- function(...,
                        main = c("boxplot", "barplot","violinplot"),
                        dotplot = T,
                        dsize = 1,
                        mark = c("mean", "median"),
                        errorbar = c("Standard Deviation", "95% Confidence Interval"),
                        rotate = T
){
  
  
  p<-ggplot2::ggplot(...) 
  
  switch(main,
         boxplot = {p <- p + ggplot2::geom_boxplot()},
         barplot = {p <- p + ggplot2::geom_bar(stat = "summary", fun.y = "mean")},
         violinplot = {p <- p + ggplot2::geom_violin(trim = F)})
  
  if(dotplot){#p <- p + ggplot2::geom_dotplot(binaxis='y', stackdir='center', binwidth = 1, dotsize = dsize)
    p <- p + ggplot2::geom_point()}
  
  switch(mark,
         mean = {p <- p + ggplot2::stat_summary(fun.y = mean, geom = "point", shape = 17, size = 3, color = "red")},
         median = {p <- p + ggplot2::stat_summary(fun.y = median, geom = "point", shape = 17, size = 3, color = "red")}
  )
  
  
  switch(errorbar,
         "Standard Deviation" = {p <- p + ggplot2::stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                                                       geom="errorbar", color="orange", width = 0.2)},
         "95% Confidence Interval" = {p <- p + ggplot2::stat_summary(fun.data=mean_cl_normal, fun.args = list(mult=1),
                                                            geom="errorbar", color="orange", width = 0.2)}
  )
  
  
  if(rotate){p <- p + ggplot2::theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5))}
  return(p)
}
