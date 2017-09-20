library(Hmisc)

EICgeneral <- function(rtmid = combino()[,"rt"],
                    mzmid = combino()[,"mz"],
                    glist = MSData$layouts[[MSData$active]]$grouping,
                    cols = MSData$layouts[[MSData$active]]$settings$cols,
                    colrange = MSData$layouts[[MSData$active]]$settings$colr,
                    transparency = MSData$layouts[[MSData$active]]$settings$alpha,
                    RTall = input$RTtoggle,
                    TICall = input$TICtoggle,
                    rtw = MSData$layouts[[MSData$active]]$settings$rtw,
                    ppm = MSData$layouts[[MSData$active]]$settings$ppm,
                    rdata = MSData$data,
                    pdfFile = file,
                    leadingTIC = T
){
    #number of plot rows
    rows <- ceiling(length(glist)/cols)
    
    #make color scales for each group, color shades based on group with most members
    colvec <- do.call(colrange,
                      list(n=max(sapply(glist,length)), alpha = transparency))
    colrs <- list()
    for(i in 1:length(glist)){
        
        colrs[[i]] <- colvec[1:length(glist[[i]])]
        
    }
    
    #generate rt boundary df
    if(RTall | is.null(rtmid)){
        rtmid <- NULL
        rtx <- NULL
    }else{
        rtx <- data.frame(rtmin = rtmid - rtw,
                          rtmax = rtmid + rtw)
    }  
    
    #generate mz boundary df
    if(TICall | is.null(mzmid) ){
        mzmid <- 100
        mzx <- data.frame(mzmin = 0,
                          mzmax = 1)
        titx <- "TICs"
        tictog <- TRUE
    }else{
        tictog <- FALSE
        mzx <- data.frame(mzmin = mzmid - mzmid*ppm*1e-6,
                          mzmax = mzmid + mzmid*ppm*1e-6)
        titx <- EICtitles(mzmid, rtmid, ppm)
    }
    


    
    
 #optionally export to pdf
    if(!is.null(pdfFile)){
    pdf(pdfFile,
        6*cols,
        6*ceiling(length(glist)/cols)+2
    )}
        
    #optionally plot TICs first (page 1 in pdf)
      if(leadingTIC){  
    EICsTIC <- multiEIC(rawdata= rdata,
                        mz = mzx[1,],
                        rt = NULL,
                        rnames = row.names(mzmid)[1], #major item names
                        byFile = F #if true, table will be sorted by rawfile, otherwise by feature
    )
    groupPlot(EIClist = EICsTIC,
              grouping = glist,
              plotProps = list(TIC = T, #settings for single plots
                               cx = 1,
                               colr = colrs),
              compProps = list(mfrow=c(rows,cols), #par options for the composite plot
                               oma=c(0,2,4,0),
                               xpd=NA, bg="white",
                               header =  "TICs",
                               header2 = NULL,
                               pdfFile = NULL,
                               pdfHi = 6*rows,
                               pdfWi = 6*cols,
                               cx = 1)
    )
      }
    
    EICs <- multiEIC(rawdata= rdata,
                     mz = mzx,
                     rt = rtx,
                     rnames = row.names(mzmid), #major item names
                     byFile = F #if true, table will be sorted by rawfile, otherwise by feature
    )
    
    groupPlot(EIClist = EICs,
              grouping = glist,
              plotProps = list(TIC = tictog, #settings for single plots
                               cx = 1,
                               colr = colrs,
                               xlim = rtx),
              compProps = list(mfrow=c(rows,cols), #par options for the composite plot
                               oma=c(0,2,4,0),
                               xpd=NA, bg="white",
                               header =  titx,
                               header2 = NULL,
                               pdfFile = NULL,
                               pdfHi = 6*rows,
                               pdfWi = 6*cols,
                               cx = 1)
    )
    if(!is.null(pdfFile)){dev.off()}
}







EICtitles <- function(mzs, rts, ppm){
  
  numbs <- matrix(mapply(sprintf,matrix(c(mzs,
                                          mzs-mzs*ppm*1e-6,
                                          mzs+mzs*ppm*1e-6),ncol=3),
                         MoreArgs = list(fmt = "%.5f")),
                  ncol=3)
  
  titx <- paste0('m/z range: ',numbs[,1], " +/- ", 
                 format(ppm, scientific = F),
                 " ppm (", numbs[,2]," - ", numbs[,3],")",
                 if(!is.null(rts)){
                   paste0(
                 " @ RT: ", sprintf( "%.2f", rts/60), " min",
                 " (", sprintf("%.1f", rts)," sec)")})
  return(titx)
  
}

groupPlot <- function(EIClist = res,
                      grouping = grouping2,
                      plotProps = list(TIC = T, #settings for single plots
                                       cx = 1,
                                       colr = topo.colors(nrow(minoritem), alpha=1),
                                       ylim = NULL, #these should be data.frames or matrices of nrow = number of plotted features
                                       xlim = NULL),
                      compProps = list(mfrow=c(1,2), #par options for the composite plot
                                     oma=c(0,2,8,0),
                                     xpd=NA, bg=NA,
                                     header =  paste0(names(res)),
                                     header2 = NULL,
                                     pdfFile = NULL,
                                     pdfHi = 6,
                                     pdfWi = 12,
                                     cx = 1)
                      ){
    
    #majoritem is the EIClist top level (typically by mz, but could be)
    
     if(!is.null(compProps$pdfFile)){pdf(compProps$pdfFile,compProps$pdfWi,compProps$pdfHi)}
    
    if(!is.null(compProps$header)){
        
        autosize <- compProps$cx*1.5
        while(max(strwidth(compProps$header, units = "figure", cex = autosize, font = 2)) > 1){
            autosize <- autosize*0.975
        }
    }
    
    for(majoritem in c(1:length(EIClist))){
       
        par(mfrow=compProps$mfrow,
            oma=compProps$oma,
            xpd=compProps$xpd,
            bg=compProps$bg)
       # layout(t(as.matrix(c(1,2))))
    
        for(plotgroup in c(1:length(grouping))){
            #par(mfrow=c(1,2))
         items = grouping[[plotgroup]]
         minoritem <- if(length(items) == 1){t(as.matrix(EIClist[[majoritem]][items,]))}else{EIClist[[majoritem]][items,]}
         if(length(items) == 1){row.names(minoritem) <- items}
         #exception handling: EIClists for only one file have to be addressed differently
    #     if(length(items) == 1){
     #    EICplot(EIClistItem = minoritem, cx = plotProps$cx, 
      #           ylim = if(plotProps$TIC){c(0,max(unlist(minoritem[,'tic'])))}
       #          else if (is.null(plotProps$ylim)){c(0,max(unlist(minoritem$intensity)))}
        #         else{c(min(plotProps$ylim[majoritem,]), max(plotProps$ylim[majoritem,]))}, 
         #        xlim = if (is.null(plotProps$xlim)){c(min(unlist(minoritem$rt)),
          #                                             max(unlist(minoritem$rt)))/60}
           #      else{c(min(plotProps$xlim[majoritem,]), max(plotProps$xlim[majoritem,]))/60},
            #     legendtext = paste(sub("^([^.]*).*", "\\1",basename(row.names(minoritem)))),
             #    colr = if(is.list(plotProps$colr)){plotProps$colr[[plotgroup]]}
              #   else{plotProps$colr[1:nrow(minoritem)]},
               #  heading = names(grouping)[plotgroup],
                # relto = NULL,
                 #TIC = plotProps$TIC
                 #mfrow=c(1,2)
         #)   }
         #else{
           
        EICplot(EIClistItem = minoritem, cx = plotProps$cx, 
                            ylim = if(plotProps$TIC){c(0,max(unlist(minoritem[,'tic'])))}
                                   else if (is.null(plotProps$ylim)){c(0,max(unlist(minoritem[,'intensity'])))}
                                   else{c(min(plotProps$ylim[majoritem,]), max(plotProps$ylim[majoritem,]))}, 
                            xlim = if (is.null(plotProps$xlim)){c(min(unlist(minoritem[,'rt'])),
                                     max(unlist(minoritem[,'rt'])))/60}
                                    else{c(min(plotProps$xlim[majoritem,]), max(plotProps$xlim[majoritem,]))/60},
                            legendtext = paste(sub("^([^.]*).*", "\\1",basename(row.names(minoritem)))),
                            colr = if(is.list(plotProps$colr)){plotProps$colr[[plotgroup]]}
                                      else{plotProps$colr[1:nrow(minoritem)]},
                            heading = names(grouping)[plotgroup],
                            relto = NULL,
                            TIC = plotProps$TIC
                #mfrow=c(1,2)
                ) 
        # }
    
        }
        
        if(!is.null(compProps$header)){
            
     #       autosize <- compProps$cx*1.5
      #      while(strwidth(compProps$header[majoritem], units = "figure", cex = autosize, font = 2) > 0.98){
       #         autosize <- autosize*0.95
        #    }
                
            
                mtext(compProps$header[majoritem], side=3, outer=T,line=2, cex=autosize, font = 2)}
        if(!is.null(compProps$header2)){mtext(compProps$header2[majoritem], side=3, outer=T,line=0.5, cex=compProps$cx)}
}

    if(!is.null(compProps$pdfFile)){dev.off()}
}

EICplot <- function(EIClistItem = res[[1]], cx = 1, 
                    ylim = c(0,max(unlist(EIClistItem[,'tic']))), 
                    xlim = c(min(unlist(EIClistItem[,'rt'])),
                             max(unlist(EIClistItem[,'rt'])))/60,
                    legendtext = paste(sub("^([^.]*).*", "\\1",basename(row.names(EIClistItem)))),
                    colr = topo.colors(nrow(EIClistItem), alpha=1),
                    heading = "test",
                    relto = NULL,
                    TIC = T,
                    single = F
                    ){
    if(TIC){int <- EIClistItem[,'tic']}
    else{int <- EIClistItem[,'intensity']}
    
    if(max(ylim)==0){ylim = c(0,1)}
    
        maxint <- format(max(unlist(int)), digits =3, scientific = T)

    if(!is.null(relto) && relto != 1 ){int <- lapply(lapply(int,"/",relto),"*",100)
                                      maxint <- format(relto, digits =3, scientific = T)
    }
    
    #convert to minutes
    rts <- lapply(EIClistItem[,"rt"],"/",60)
  #  par(xpd=NA)
       # options(scipen=20)
     
    if(single){
    par(#mfrow=c(1,2),
         oma=c(0,2,0,0),
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
     
     plot(numeric(),numeric(), type= "n", 
         ylim = ylim,
         xlim = xlim,
             axes=F, ylab="",xlab="")

     
   pn <- if(max(ylim)==0){1}else{5}
         #x axis
    axis(side=1, lwd=1, at = pretty(xlim),
         labels = format(pretty(xlim), scientific = F),
         mgp=c(0,0.4,0), cex=1*cx, xaxs = "i")#x-axis mgp[2] controls distance of tick labels to axis
    
    mtext(side=1, text= "RT (min)", line=1.5, cex=cx*1)
    
    if(!is.null(relto) && relto != 1 ){
        axis(side=2, lwd=1, las=2, at = pretty(ylim, n =pn),
             labels = format(pretty(ylim, n =pn), scientific = F),
             mgp=c(0,0.6,0), cex=1*cx)
        #axis labels
        mtext(side=2, text="Relative intensity (%)", line=4, cex=1*cx)
    }
    else{
    #y axis
    axis(side=2, lwd=1, las=2, at = pretty(ylim, n =pn),
         labels = format(pretty(ylim, n =pn), scientific = T,digits = 3),
         mgp=c(0,0.6,0), cex=1*cx)
     #axis labels
    mtext(side=2, text="Intensity", line=4, cex=1*cx)
        }
    
    
    abline(v=min(xlim), h=min(ylim))
   #colr <- topo.colors(2, alpha=1)#rainbow(length(eiccoll[[t]]), s = 1, v = 1, start = 0, end = max(1, length(eiccoll[[t]]) - 1)/length(eiccoll[[t]]), alpha = 0.7)#topo.colors(length(eiccoll[[t]]), alpha=1)
   

    tmp <-mapply(lines, x= rts, y = int, col = as.list(colr))
    Hmisc::minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
    
   par(xpd=NA)
   text(min(xlim), 1.04*max(ylim),
           labels = maxint, bty="n",
        font = 2, cex=cx*1)
    
    if(!is.null(legendtext)){
        legend("topright",
          # inset=c(-0.08,-0.08),#c(0.025*max(xlim),0.025*max(ylim)),
           legendtext, lty=1,lwd=2.5, col=colr, bty="n",  cex=cx*0.7)
    }
    title(main=heading, line=2, cex = cx)
    }