library(xcms)
library(graphics)
library(Hmisc)
#library(zoo) #for AUC calc
#library(peakPick) #for spike and peak detection in multiEIC
#library(tcltk)
library(MASS) #for 2d density
library(plot3D) #plot 2d density and other matrices with image2D()
library(CAMERA)
library(heatmaply)
library(pvclust)
library(data.table)
library(DT)
library(shiny)
library(rhandsontable)
library(plotly)
library(TeachingDemos) # for non overlapping label placement
library(jsonlite)
library(BiocParallel)
library(rcdk) #for molecule drawing
#source("https://bioconductor.org/biocLite.R")
#biocLite(c("xcms","CAMERA","mzR","BiocParallel"))
#biocLite(c("mclust"))



#' EICfiles
#' 
#' This function is used to select mzXML files and group them into categories that will later be plotted in one graph together

#' @param binvar1 vector with categories in which the files should be sorted. Defaults to one category ("all", one EIC graph for all files).
#' @param folders vector (c("folder1","folder2")) of folders which contain MS data files, will search all subfolders and sort files based on their file names containing the strings defined in binvar1. If left empty (folders = ""), file selection windows to add files for each category defined in binvar1 will appear. Defaults to folders="".
#' @param type   only files with this file extension will be selected (mzML or mzXML)
#' @examples 
#' function(binvar1 = c("all"), folders = "", type="mzXML")
#' @export
EICfiles<-function(binvar1 = c("all"),
                   folders = "",
                   type="mzXML"){
  #print(folders)
  #define your file categories
  #binvar1 <- c("sup", "pel","Blank")
  
  #select mzXML files
  if(folders==""){
    fileList=list()
    for(i in c(1:length(binvar1))){
      fileList[[i]] <- choose.files(default= if(i>1){dirname(fileList[[i-1]][1])}else{""}, caption=paste("Add files to group", binvar1[i]), multi=T)
      repeat{
        if(winDialog(type="yesno", "Do you want to add more files to this group?")=="YES")
        {fileList[[i]] <- c(fileList[[i]], choose.files(default= dirname(fileList[[i]][1]),caption=paste("Add more files to group", binvar1[i]), multi=T))}else{break}}}
    
    bin1_p <- fileList
  }else{
    ##ALTERNATIVE FILE LIST CREATION###
    #make file list from folders, using file name recognition of terms defined in binvar1
    #setwd("E:/users/MH/mzxml/161101/centroid/")
    #for(i in c(1:length(folders)))
      mzxml_p <- list.files(folders[1], pattern=type, recursive = TRUE, include.dirs=T, full.names=T)
    if(length(folders)>1){for(i in c(2:length(folders))){
      mzxml_p <- c(mzxml_p,list.files(folders[i], pattern=type, recursive = TRUE, include.dirs=T, full.names=T))
    }}
    #select pos or neg
    #mzxml_p <- mzxml_pos[grepl("*_pos", mzxml_pos, ignore.case = T)]
    #print(mzxml_p)
    
    bin1_p <- list()
    for(i in c(1:length(binvar1))){
      bin1_p[[i]] <- mzxml_p[grepl(binvar1[i], mzxml_p, ignore.case = T)]
    }
  }
  return(bin1_p)
}

#' EICraw

#' TIME CONSUMING. This step does not need to be repeated when adjusting other parameters
#' (e.g. feature list, EIC ppm or RT) Generates an R-readable data structure from MS data
#' files defined in the input file list crated with EICfiles.
#' @examples
#' EICraw(filelist)
#'
#' @export
EICraw <- function (bin1_p, MSn = T){
  #make separate rawEICs in groups, this is the most time consuming step!
  rawlist=list()
  
  rawcoll=list()
  for(t in c(1:length(bin1_p))){
    for (numb in c(1:length(bin1_p[[t]]))){
      rawlist[[numb]] <- xcmsRaw(bin1_p[[t]][numb], profstep=0, includeMSn = MSn)
      cat(basename(bin1_p[[t]][numb]), " ")}
    rawcoll[[t]] <-rawlist
    rawlist=list()}
  names(rawcoll)<-names(bin1_p)
  return(rawcoll)}

##Parallel enabled version for larger number of files
EICrawP <- function (bin1_p, MSn = T, workers=1){
        #make separate rawEICs in groups, this is the most time consuming step!
        rawcoll=list()
        if (length(unlist(bin1_p))<=10){workers<-1}
        param <- SnowParam(workers = workers)
        rawcoll <- bplapply(unlist(bin1_p),xcmsRaw,  profstep=0, includeMSn = MSn, BPPARAM= param)
        #cat(names(bin1_p)[t], " ")
        rawlist1 <- list()
        rawlist2 <- list()
        
        cl <- vector()
        for (f in rawcoll){
            cl <- c(cl,f@filepath[1])
        }
        
        for (i in c(1:length(bin1_p))){
            rawlist1[[i]] <- rawcoll[cl %in% bin1_p[[i]]]}
        
        names(rawlist1)<-names(bin1_p)
        
        return(rawlist1)}


#' loadfeats(seper):

#'  Will prompt a file selection window to load a table (text or csv file) that contains features, needs to contain columns named "mz" and "rt" (and any number of additional columns). 
#' @param seper select the list separator, defaults to "\t"
#' @examples 
#' loadfeats(",")
#' @export
loadfeats <- function (seper ="\t"){
  fileNamex <<- choose.files(caption="Select table with m/z and rt values")
  #reading the table, making column names into callable variables
  if(identical(fileNamex,character(0))){alli <- NULL}else{
  alli <- read.table(fileNamex, header= T, sep= seper, dec=".", fill= T, skip=0)
  if(is.null(alli$rt)){alli$rt <- alli$rtmed}
  if(is.null(alli$mz)){alli$mz <- alli$mzmed}
  }
  return(alli)}


#' EICplot
#'

#' plot EICs from the EICraw object using various settings
#' note: currently requires fileNameX object from loadfeats present in environment to correctly process EICs.
#' @param x        list of MS data files (same as for EICraw)
#' @param y        R-readable MS data object generated by EICraw
#' @param binvar1  vector with categories that should be used as headings for graphs.
#'           Defaults to one category ("all", one EIC graph for all files).
#'           vector must have same length as binvar1 in EICfiles
#' @param alli     feature list as imported with loadfeats(). If NULL (default), only TICs will be plotted!
#' @param ppr      ppm tolerance for EICs
#' @param rtw      RT window for EICs; seconds before and after feature shown (e.g. 90 gives a 180 sec window)
#' @param grn      number of graphs in one row
#' @param swABS    TRUE or FALSE, if TRUE, alli needs to have defined minimum and maximum m/z values 
#'           in columns called mzmin and mzmax (will be used instead of ppm around mz)
#' @param hi       figure height multiplier
#' @param wi       figure width multiplier
#' @param labex special mode for labeling experiments, requires mziso column in input feature list
#' @param rtline select T if you would like to plot a line indicating the selected feature rt
#' 
#' @examples 
#' EICplots(x, y, binvar1 = c("all"), alli = NULL, ppr = 10, rtw=90, grn= 2, swABS=F, hi=5, wi=3.5, labex = F, rtline=T)
#' @export
EICplot <- function(bin1_p, rawcoll, binvar1 = c("all"), alli = NULL, ppr = 10, rtw=90, 
                    grn= 2,  swABS=F, hi=5, wi=3.5, labex = F, rtline=T, evalmode=F, 
                    pdfout=T, fileNamex="./defaultoutput", 
                    deffile = F, #use if the filename should be used exactly as put in in fileNamex, will also suppress automatic pdf opening (for use in mosaic)
                    cx = 1){
  
  
  
  #how many graphs to show in a row
  #grn <- 2
  
  #mass window for EICS (+/- ppm) from input feature m/z
  #ppr <- 10 #ppm
  
  #"YES"=activate the absolute mode EICs (mz instead of ppm range)
  #swABS <- ""
  
  #RT window for EICs (+/- sec) from input feature RT
  #rtw <-90 #sec
  #input a tsv file with AT LEAST two columns, "mz" and "rt". (separator currently set to tabs; 
  #column names later read into output pdf are: mz, rt, Comments, Sample, Medium, Extraction, Tool - any other column names will be ignored; case-sensitive
  if(!is.null(alli)){
    #set the ppm tolerance for EICs
    mzr  <- data.frame(alli$mz-0.000001*ppr*alli$mz,alli$mz+0.000001*ppr*alli$mz)
    if(labex){mzriso <- data.frame(alli$mziso-0.000001*ppr*alli$mziso,alli$mziso+0.000001*ppr*alli$mziso)}
    if(swABS){mzr  <- data.frame(alli$mzmin,alli$mzmax)}
    #set the RT window for EICs
    rtr <- data.frame(alli$rt-rtw,alli$rt+rtw)}
  
  if(evalmode==T) {cat("Enter evaluation in console window. Presets: 1= Good, 2= OK, 3=Bad, 4= Unknown. Enter any value or string and press Enter. \n")}
  
  #set up the pdf file
  if(deffile){
    nakedFile <- sub("^([^.]*).*", "\\1",fileNamex)
    pdfFile <- fileNamex
  }else{
  if(is.null(alli)){
    nakedFile <- paste0(dirname(bin1_p[[1]][1]),"/TICs_only")
    pdfFile <- paste0(nakedFile,".pdf")
  }else{
    nakedFile <- sub("^([^.]*).*", "\\1",fileNamex)
    pdfFile <- paste0(nakedFile,"_EICv8_output.pdf")
  }
  if(file.exists(pdfFile)){pdfFile <-paste0(nakedFile,"_",round(runif(1,min=0.1,max=1)*10000, digits=0),"_EICv8_output.pdf")}  #{file.remove(pdfFile)}
  }
    
  nr <- if(length(bin1_p)>=grn){ceiling((length(bin1_p))/grn)}else{1}
  Fighi <- hi*nr+2 
  Figwi <- wi*grn
 if(evalmode==F & pdfout==T) {pdf(pdfFile,Figwi,Fighi)}
  par(mfrow=c(nr,grn), oma=c(0,0,10,0),xpd=NA, bg=NA)
  
  
  ##Make a set of TICs
  
  eiclist=list()
  maxlist=list()
  eiccoll=list()
  maxcoll=list()  
  for(t in c(1:length(bin1_p))){
    for (numb in c(1:length(bin1_p[[t]]))){
      eiclist[[numb]] <- rawEIC(rawcoll[[t]][[numb]],mzrange =c(0,5000),rtrange = c(0,max(rawcoll[[t]][[numb]]@scantime)))
      maxlist[[numb]] <- max(eiclist[[numb]]$intensity)
      maxlist[[numb]]$loop <- numb}
    eiccoll[[t]] <- eiclist
    eiclist=list()
    maxcoll[[t]] <- maxlist
    maxcoll[[t]]$cloop <- t
    maxlist=list()}
  
  
  #make list of maxima for each TIC group
  maxsi=list()
  for(t in c(1:length(bin1_p))){
    for (numb in c(1:length(bin1_p[[t]]))){
      maxsi[[t]] <- max(unlist(maxcoll[[t]]))#,maxcoll[[t]][[numb]][[1]]))
    }}
  
  
  
  
  
  for(t in c(1:length(bin1_p))){
    #making RT out of scan#
    RTwr <- c(min(rawcoll[[t]][[1]]@scantime)/60,max(rawcoll[[t]][[1]]@scantime)/60)
    RT1 <- (eiccoll[[t]][[1]]$scan-eiccoll[[t]][[1]]$scan+RTwr[[1]]+(eiccoll[[t]][[1]]$scan-min(eiccoll[[t]][[1]]$scan))*((RTwr[[2]]-RTwr[[1]])/(max(eiccoll[[t]][[1]]$scan)-min(eiccoll[[t]][[1]]$scan))))
   
     options(scipen=-20)
    plot(RT1,eiccoll[[t]][[1]]$intensity, type= "n", xlim= c(0,max(rawcoll[[t]][[1]]@scantime)/60), ylim =c(0,maxsi[[t]]), axes=F, ylab="Intensity",xlab="")
    axis(side=2, lwd=1, las=2, mgp=c(0.7,0.6,0))
    options(scipen=20)
    axis(side=1, lwd=1, minor.tick(nx=10,ny=5, tick.ratio=0.5), mgp=c(1,0.4,0))#x-axis mgp[2] controls distance of tick labels to axis
    mtext(side=1, text= "RT (min)", line=1.2, cex=cx*0.7)
    title(main=binvar1[t], line=4+0.1*length(eiccoll[[t]]))
    colr <- rainbow(length(eiccoll[[t]]), s = 1, v = 1, start = 0, end = max(1, length(eiccoll[[t]]) - 1)/length(eiccoll[[t]]), alpha = 0.7)#topo.colors(length(eiccoll[[t]]), alpha=1)
    for (numb in c(1:length(eiccoll[[t]]))){
      RTa <- (eiccoll[[t]][[numb]]$scan-eiccoll[[t]][[numb]]$scan+RTwr[[1]]+(eiccoll[[t]][[numb]]$scan-min(eiccoll[[t]][[numb]]$scan))*((RTwr[[2]]-RTwr[[1]])/(max(eiccoll[[t]][[numb]]$scan)-min(eiccoll[[t]][[numb]]$scan))))
      
      lines(RTa,eiccoll[[t]][[numb]]$intensity, col=colr[numb], lwd=1)}
    legend("topright", inset=c(0,-0.025*length(eiccoll[[t]])),basename(bin1_p[[t]]), lty=1,lwd=2.5, col=colr, bty="n",  cex=cx*0.5)}
  mtext("TICs", side=3, outer=T,line=7.2, cex=cx*1)
  
  #execute these lines here if you only want TICs
  if(is.null(alli)){
    dev.off()
    if (pdfout==T){shell.exec(pdfFile)}}
  
  
  ##loop through the mz/rt list
  eiclist=list()
  maxlist=list()
  eiccoll=list()
  maxcoll=list()
  if(labex){
  eiclistiso=list()
  maxlistiso=list()
  eiccolliso=list()
  maxcolliso=list()}
  
  for(m in c(1:NROW(mzr))){
    #if(evalmode==T) {windows(Figwi,Fighi)}
    
    RTwr <- if(nrow(rtr)==nrow(mzr)){rtr[m,]}else{rtr[1,]}
    
    #Make EICs
    
    for(t in c(1:length(bin1_p))){
      for (numb in c(1:length(bin1_p[[t]]))){
        eiclist[[numb]] <- rawEIC(rawcoll[[t]][[numb]],mzrange = mzr[m,],rtrange = RTwr)
        maxlist[[numb]] <- max(eiclist[[numb]]$intensity)
        maxlist[[numb]]$loop <- numb}
      eiccoll[[t]] <- eiclist
      eiclist=list()
      maxcoll[[t]] <- maxlist
      maxcoll[[t]]$cloop <- t
      maxlist=list()}
    
    if(labex){
    for(t in c(1:length(bin1_p))){
      for (numb in c(1:length(bin1_p[[t]]))){
        eiclistiso[[numb]] <- rawEIC(rawcoll[[t]][[numb]],mzrange = mzriso[m,],rtrange = RTwr)
        maxlistiso[[numb]] <- max(eiclistiso[[numb]]$intensity)
        maxlistiso[[numb]]$loop <- numb}
      eiccolliso[[t]] <- eiclistiso
      eiclistiso=list()
      maxcolliso[[t]] <- maxlistiso
      maxcolliso[[t]]$cloop <- t
      maxlistiso=list()}  }
    
    
    #make list of maxima for each EIC group
    maxsi=list()
    for(t in c(1:length(bin1_p))){
      for (numb in c(1:length(bin1_p[[t]]))){
        maxsi[[t]] <- max(unlist(maxcoll[[t]]))#,maxcoll[[t]][[numb]][[1]]))
      }}
    if(labex){
    maxsiiso=list()
    for(t in c(1:length(bin1_p))){
      for (numb in c(1:length(bin1_p[[t]]))){
        maxsiiso[[t]] <- max(unlist(maxcolliso[[t]]))#,maxcoll[[t]][[numb]][[1]]))
      }}}
    
    
    par(mfrow=c(nr,grn), oma=c(0,0,10,0),xpd=NA, bg=NA)
    
    for(t in c(1:length(bin1_p))){
      #making RT out of scan#
      RT1 <- (eiccoll[[t]][[1]]$scan-eiccoll[[t]][[1]]$scan+RTwr[[1]]+(eiccoll[[t]][[1]]$scan-min(eiccoll[[t]][[1]]$scan))*((RTwr[[2]]-RTwr[[1]])/(max(eiccoll[[t]][[1]]$scan)-min(eiccoll[[t]][[1]]$scan))))/60
      options(scipen=-20)
      plot(RT1,eiccoll[[t]][[1]]$intensity, type= "n", 
           ylim =if (labex){c(0,max(c(maxsi[[t]],maxsiiso[[t]])))}else{c(0,maxsi[[t]])}, axes=F, ylab="Intensity",xlab="")
      axis(side=2, lwd=1, las=2, mgp=c(0.7,0.6,0))
      options(scipen=20)
      axis(side=1, lwd=1, minor.tick(nx=10,ny=5, tick.ratio=0.5), mgp=c(1,0.4,0))#x-axis mgp[2] controls distance of tick labels to axis
      mtext(side=1, text= "RT (min)", line=1.2, cex=cx*0.7)
      
      title(main=binvar1[t], line=4+0.1*length(eiccoll[[t]]))
      colr <- topo.colors(length(eiccoll[[t]])+if(labex){length(eiccolliso[[t]])}else{0}, alpha=1)#rainbow(length(eiccoll[[t]]), s = 1, v = 1, start = 0, end = max(1, length(eiccoll[[t]]) - 1)/length(eiccoll[[t]]), alpha = 0.7)#topo.colors(length(eiccoll[[t]]), alpha=1)
      for (numb in c(1:length(eiccoll[[t]]))){
        RTa <- (eiccoll[[t]][[numb]]$scan-eiccoll[[t]][[numb]]$scan+RTwr[[1]]+(eiccoll[[t]][[numb]]$scan-min(eiccoll[[t]][[numb]]$scan))*((RTwr[[2]]-RTwr[[1]])/(max(eiccoll[[t]][[numb]]$scan)-min(eiccoll[[t]][[numb]]$scan))))/60
        
        lines(RTa,eiccoll[[t]][[numb]]$intensity, col=colr[numb], lwd=1)
        }
      if(labex){
       for (numb in c(1:length(eiccolliso[[t]]))){
        RTa <- (eiccolliso[[t]][[numb]]$scan-eiccolliso[[t]][[numb]]$scan+RTwr[[1]]+(eiccolliso[[t]][[numb]]$scan-min(eiccolliso[[t]][[numb]]$scan))*((RTwr[[2]]-RTwr[[1]])/(max(eiccolliso[[t]][[numb]]$scan)-min(eiccolliso[[t]][[numb]]$scan))))/60
        
        lines(RTa,eiccolliso[[t]][[numb]]$intensity, col=colr[length(eiccoll[[t]])+numb], lwd=1)}}
      
    
      
        legendtext <- if(labex){
        c(paste(sub("^([^.]*).*", "\\1",basename(bin1_p[[t]]))),
          paste0(sub("^([^.]*).*", "\\1",basename(bin1_p[[t]])),"+LABEL"))
        }else{paste(sub("^([^.]*).*", "\\1",basename(bin1_p[[t]])))}
      legend("topright", inset=c(0,-0.025*length(eiccoll[[t]])),legendtext, lty=1,lwd=2.5, col=colr, bty="n",  cex=cx*0.5)
      if(rtline){segments(alli$rt[m]/60,0,alli$rt[m]/60,if (labex){max(c(maxsi[[t]],maxsiiso[[t]]))}else{maxsi[[t]]},lty=2,lwd=1, col="black")}
      }
    if(labex){
      mtext(paste("m/z range:", round(alli$mz[m],5), "/", round(alli$mziso[m],5), "(labeled) +/-", ppr," ppm @ RT",round(alli$rt[m],1),"sec /",round(alli$rt[m]/60,2), "min"), side=3, outer=T,line=7.2, cex=cx*1)
    }else{
      mtext(paste("m/z range:", round(mzr[m,1],5),"-",round(mzr[m,2],5),"(",round(alli$mz[m],5),"+/-", ppr, "ppm @ RT",round(alli$rt[m],1),"sec /",round(alli$rt[m]/60,2), "min"), side=3, outer=T,line=7.2, cex=cx*1)
    }
      mtext(paste("Category:", alli$Sample[m], alli$Medium[m], alli$Extraction[m], alli$Tool[m]), side=3, outer=T,line=6, cex=cx*1)
    mtext(paste("Comment:",alli$Comments[m]), side=3, outer=T,line=4.8, cex=cx*0.5)
    
    if(evalmode==T) { 

    cat("Manual evaluation of ", paste0("m/z range:", round(alli$mz[m],5), if(labex){paste0("/", round(alli$mziso[m],5), "(labeled)")}," +/-", ppr," ppm @ RT",round(alli$rt[m],1),"sec /",round(alli$rt[m]/60,2), "min"))
      doneVal <- readline(prompt = "Enter here > ")
      
    # Test the result
      alli$Comments[m] <- doneVal

    #dev.off()
    }
    
    }
 
   if(evalmode==T) {return(alli)}
   if(pdfout){ dev.off()
  #open your output pdf file
     if(!deffile){
  shell.exec(pdfFile)}}
}
######multiEIC###################
#' multiEIC
#' 
#' Get intensities from a table of mz/rt features from an EICraw- created object. Requires rtmin and rtmax  
#'
#' New version with apply instead of loops 5x faster for EICs
#'
#'

multiEIC <- function (rawfile= rawdata[[1]][[39]] ,
                         featuretable=alli,
                         mz = alli$mz,
                         ppm=5,
                         rtw= data.frame(alli$rtmin-5,alli$rtmax+5),
                         mini=5000,
                         coltag="test",
                         pval=0.05,
                         gauss=T,
                         XIC=T,
                         abovex=T){
  
  #cat(paste0("Reference list with ",length(featuretable[,1])," features, iterating through list, feature #" ))
  
  
  mx <- matrix(data= c(mz-ppm*(mz/1000000),
                       mz+ppm*(mz/1000000),
                       rowMin(as.matrix(rtw)),
                       rowMax(as.matrix(rtw))), nrow= length(mz), ncol=4)
  
  mxl <-unname(as.list(data.frame(t(mx[,1:2]))))
  rxl <-unname(as.list(data.frame(t(mx[,3:4]))))
  
  
  summe <- mapply(rawEIC, mzrange = mxl,
                  rtrange = rxl, MoreArgs=list(object=rawfile), SIMPLIFY = F)
  
  #substract "baseline" and get rid of scan#
  fx <- function(x) x$intensity-min(x$intensity)
  summe <- lapply(summe, fx )
  
  if(XIC){
    import <- sapply(summe, mean)
    featuretable$importxx<-import
    colnames(featuretable)[which(names(featuretable) == "importxx")] <- paste0(sub("^([^.]*).*", "\\1",basename(coltag)),"_XIC")
  }
  
  if(abovex){
    #how many consecutive intensity values are above mini (note this is after baseline correction!)
    fx <- function(x) max(rle(x>mini)$lengths[which(rle(x>mini)$values ==T)])
    abovemini <- sapply(summe, fx)
    abovemini[abovemini==-Inf] <-0
    #how many consecutive intensity values are above average of values in rt window (note this is after baseline correction!)
    fy <- function(x) max(rle(x>mean(x))$lengths[which(rle(x>mean(x))$values ==T)])
    abovemean <- sapply(summe, fy)
    abovemean[abovemean==-Inf] <-0
    
    featuretable$aboveminixx <- abovemini
    featuretable$abovemeanxx <- abovemean
    
    colnames(featuretable)[which(names(featuretable) == "aboveminixx")] <- paste0(sub("^([^.]*).*", "\\1",basename(coltag)),"_Abovemini")
    colnames(featuretable)[which(names(featuretable) == "abovemeanxx")] <- paste0(sub("^([^.]*).*", "\\1",basename(coltag)),"_Abovemean")
  }
  
  if(gauss){    
    #normalize intensities to 1
    fx <- function(x) if(max(x)>0){x/max(x)}else{x}
    summe <- sapply(summe, fx)
    
    #here starts the gaussian test, cf. http://www.metabolomics-forum.com/index.php?topic=1031.0 (Krista Longnecker/Tony Larson)
    #fit gauss and let failures to fit through as corr=1
    fy <- function(sm){
      fit <- try(nls(y ~ SSgauss(x, mu, sigma, h), data.frame(x =
                                                                1:length(sm), y = sm)),silent=T)
      if(class(fit) == "try-error")
      {
        0
      } else
      {
        #calculate correlation of summe$intensity against gaussian fit
        if(length(which(!is.na(sm-fitted(fit)))) > 4 &&
           length(!is.na(unique(sm)))>4 && length(!is.na(unique(fitted(fit))))>4)
        {
          cor <- NULL
          options(show.error.messages = FALSE)
          cor <- try(cor.test(sm,fitted(fit),method="pearson",use="complete"))
          options(show.error.messages = TRUE)
          if (!is.null(cor))
          {
            if(cor$p.value <= pval) cor$estimate else 0
          } else 0
        } else 0
      }}
    gauss <- sapply(summe, fy)
    
    featuretable$gaussxx <-gauss
    colnames(featuretable)[which(names(featuretable) == "gaussxx")] <- paste0(sub("^([^.]*).*", "\\1",basename(coltag)),"_Gauss")
    
  }
  return(featuretable)}



######featlistCompare################################################################################

featlistCompare <- function(reflist=unlabpeaks,
                            complist=labpeaks,
                            mzdiff=2*1.00335,
                            pwi= reflist$rtmax-reflist$rtmin,
                            pktol = 2,#fold difference in peak width allowed
                            rtd=10,
                            ppm=5
){
  
  cat(paste0("Reference list with ",length(reflist[,1])," features, iterating through list, feature #" ))
  
  collector <- vector()
  collectlist <- list()
  for (i in c(1:length(reflist[,1]))){
    peakwi <- pwi[i]
    
    # find labeled counterparts of feature in labeled sample
    selection1 <- complist[which(abs(reflist$mz[i]+mzdiff-complist$mz)<reflist$mz[i]*ppm*0.000001 
                                 & abs(reflist$rt[i]-complist$rt)<rtd
                                 & complist$rtmax-complist$rtmin < peakwi*pktol
                                 & complist$rtmax-complist$rtmin > peakwi/pktol
    ),]
    
    
    #find "labeled" compounds in unlabeled sample
    selection2 <- reflist[which(abs(reflist$mz[i]+mzdiff-reflist$mz)<reflist$mz[i]*ppm*0.000001 
                                & abs(reflist$rt[i]-reflist$rt)<rtd
                                # & reflist$rtmax-reflist$rtmin < peakwi*pktol
                                #  & reflist$rtmax-reflist$rtmin > peakwi/pktol
    ),]
    
    #find "unlabeled" compounds in labeled sample
    selection3 <- complist[which(abs(reflist$mz[i]-complist$mz)<reflist$mz[i]*ppm*0.000001 
                                 & abs(reflist$rt[i]-complist$rt)<rtd
                                 #  & complist$rtmax-complist$rtmin < peakwi*pktol
                                 #  & complist$rtmax-complist$rtmin > peakwi/pktol
    ),]
    
    #selection1e <-selection1
    
    
    if(length(selection1[,1])>0){
      #selection1e$unlabmz <- reflist$mz[i]
      #selection1e$unlabrt <- reflist$rt[i]
      #selection1e$unlabrtmin <- reflist$rtmin[i]
      #selection1e$unlabrtmax <- reflist$rtmax[i]
      #selection1e$unlabintb <- reflist$intb[i]
      
      
      #if (length(collector)>0){
      #collector <- rbind(collector,selection1e)}else{
      #  collector <- selection1e  
      #}
      
      
      collectlist$iso1sam1[[length(collectlist$iso1sam1)+1]] <- reflist[i,]
      collectlist$iso2sam2[[length(collectlist$iso1sam1)]] <- selection1[order(abs(selection1$rt-reflist$rt[i])),]
      collectlist$iso2sam1[[length(collectlist$iso1sam1)]] <- selection2[order(abs(selection1$rt-reflist$rt[i])),]
      collectlist$iso1sam2[[length(collectlist$iso1sam1)]] <- selection3[order(abs(selection1$rt-reflist$rt[i])),]
      
    }
    if(i %% 200==0){cat(i, " ")}}
  return(collectlist)}

######mergecollectlist###################################################################################

mergecollectlist <- function(collectlist){
  #combining the lists into one file, using only the features as filtered with the smallest RT difference (1 to 1 to 1 to 1 feature)
  combinat2 =vector()
  for (n in c(1:length(collectlist$iso1sam1))){
    collectlist$iso1sam1[[n]]$feat <- n
    collectlist$iso2sam2[[n]]$feat <- n
    collectlist$iso2sam1[[n]]$feat <- n
    collectlist$iso1sam2[[n]]$feat <- n
    
    colnames(collectlist$iso1sam1[[n]]) <- paste0("I1S1.",colnames(collectlist$iso1sam1[[n]]))
    colnames(collectlist$iso2sam2[[n]]) <- paste0("I2S2.",colnames(collectlist$iso2sam2[[n]]))
    colnames(collectlist$iso2sam1[[n]]) <- paste0("I2S1.",colnames(collectlist$iso2sam1[[n]]))
    colnames(collectlist$iso1sam2[[n]]) <- paste0("I1S2.",colnames(collectlist$iso1sam2[[n]]))
    
    
    combinat <-cbind(collectlist$iso1sam1[[n]][1,],collectlist$iso2sam2[[n]][1,],
                     collectlist$iso2sam1[[n]][1,],collectlist$iso1sam2[[n]][1,])
    if( length(combinat2)>0){
      combinat2 <-rbind(combinat2,combinat)}
    else{
      combinat2 <-combinat
    }
  }
  return(combinat2)}

######redundfind###########################################################################################

redundfind <- function(pl = xset3.pos.pl,
                       mzppm = 3,
                       rtsec = 3){
  
  #sort dataset by mz and see how large the mz gaps are between closest mz values
  pl <- pl[order(pl$mz),]
  pl$diff <-c(diff(pl$mz),1000)
  
  plnc <- vector()
  plvc <- vector()
  
  
  counter<-1
  
  selector <- c(0,which(pl$diff > pl$mz*mzppm*1e-6))
  for (n in c(1:(length(selector)-1))){
    pls <- data.frame(mz=pl$mz[(selector[n]+1):(selector[n+1])],rt=pl$rt[(selector[n]+1):(selector[n+1])]) #shrinking version of pl
    plv <- vector()
    plv[1:nrow(pls)] <- 0
    pln <- vector()
    pln[1:nrow(pls)] <- 1
    
    for (i in c(1:nrow(pls))){#
      # plsm <- pls[i+100,]
      # pln[i]  <- sum(abs(pls$mz-pls$mz[i])< pls$mz[i]*1e-6*mzppm
      #               & abs(pls$rt-pls$rt[i])< rtsec)
      if(plv[i]==0){
        #' assign a feature group number - note that the number can be overwritten, e.g when there are 2 members 
        #' of the group (based on the mz of the first member, then the script hits another mz that was not inside 
        #' the original group, but is within ppm range of the second, not the first feature -> second feature number
        #' gets overwritten. could be prevented by directionally asking mzdifference, but that doesnt really solve the
        #' problem of breaking up of groups - probably best adressed by iterating either way)
        plv[which(abs(pls$mz-pls$mz[i])< pls$mz[i]*1e-6*mzppm
                  & abs(pls$rt-pls$rt[i])< rtsec
        )]  <- counter
        pln[which(plv==counter)]  <- length(pln[which(plv==counter)])
        
        counter <- counter+1
      }
    }
    plvc<-c(plvc,plv)
    plnc<-c(plnc,pln)
    
    if (n%%1000==0){cat(selector[n], " ")}}
  
  
  pl$redundancy<-plnc
  pl$redgroup<-plvc
  
  
  return(pl)
}


##############redundfind2#########
#Group peaks that coelute and may be isotope peaks, adducts or charge states
redundfind2 <- function(pl,
                        coln = "redgroup",
                        ...
                       #mzppm = 3,
                       #rtsec = 3,
                       #shifts = c(Na= 21.98249281,
                        #          NH3= 17.0265491,
                         #         C13=1.003354838),
                       #charges = c(1,2),
                       #polarity = "positive"
    ){

    constants <- c(e=0.0005485799, proton=1.00727646681)
 
    #pt1<-proc.time()
    #groups <- integer(nrow(pl))
    #counter <- 1
    #pll <- split(pl,seq(nrow(pl)))
    
#    param <- SnowParam(workers = 4)
 #   selection <- lapply(pll,redundhelp)#, BPPARAM=param)
    
  #  for(n in selection){
   #     if (max(groups[n])>0){
    #        groups[n] <- min(groups[n][groups[n]!=0])
     #   }else{
      #      groups[n] <- counter
       #     counter <- counter + 1
        #}

        #pt1<-proc.time()-pt1
        pt2<-proc.time()
        groups <- integer(nrow(pl))
        counter <- 1
        
            selection <- apply(pl,1,
                               redundhelp,
                               pls = pl,
                               ...)
           # param <- SnowParam(workers = 4)
            #selection <- bplapply(t(pl[1:3,]),print, BPPARAM=param)
            
            for(n in selection){
            if (max(groups[n])>0){
                groups[n] <- min(groups[n][groups[n]!=0])
            }else{
                groups[n] <- counter
                counter <- counter + 1
            }
                
    }
    pt2<-proc.time()-pt2
    pl[[coln]] <- groups
    
    return(pl)}

    #helper function that returns which items in pl matych with feature under investigation
    
redundhelp <- function(     item = pl[1,],
                               pls = pl,
                               mzppm = 3,
                               rtsec = 3,
                               shifts = c(Na= 21.98249281,
                                          NH3= 17.0265491,
                                          C13=1.003354838,
                                          Noshift = 0 ),
                               charges = c(1,2),
                               polarity = "positive",
                               colnums = list(mz= which(colnames(pls)=="mz"),
                                              rt= which(colnames(pls)=="rt"))){
    constants <- c(e=0.0005485799, proton=1.00727646681, H=1.007825032)
    
    #make possible variants of mz value based on shifts list
    if(polarity=="positive"){
        uncharged = (as.numeric(item[colnums$mz])*charges)-charges*constants['proton']
        charged = unique(as.vector(t(outer(uncharged,charges*constants['proton'],FUN="+")))/charges)           
        shiftvars = unique(as.vector(outer(shifts,charges,"/")))
        combinat = unique(as.vector(outer(charged,shiftvars,"+")))
        
    }
    if(polarity=="negative"){
        uncharged = (as.numeric(item[colnums$mz])*charges)+charges*(constants['H']-constants['e'])
        charged = unique(as.vector(t(outer(uncharged,charges*(constants['H']-constants['e']),FUN="-")))/charges)           
        shiftvars = unique(as.vector(outer(shifts,charges,"/")))
        combinat = unique(as.vector(outer(charged,shiftvars,"+")))
        
    }
    
    # new take, this time without loop:
    tb <- which(abs(pls$rt - as.numeric(item[colnums$rt])) < rtsec)
    
    if(length(tb)>0){
    return(tb[which(rowMin(t(abs(outer(combinat,pls$mz[tb],"-"))/combinat)) < mzppm*1e-6)])}
    else
    {return(integer(0))}}



redundrank <- function(pl,
                       groupingcolumn=pl$pcgroupown,
                        intcols = c(12:16),
                       colnls = list(rank="pcgrank",membernum = "nummembers")){
    
    pcgmembers <- integer(nrow(pl)) 
    pcgrank <- integer(nrow(pl))
    
    intens <- rowMeans(as.matrix(pl[,intcols]))
    
    for(i in c((min(groupingcolumn)):(max(groupingcolumn)))){
        
        selection <- which(groupingcolumn==i)
        
        pcgrank[selection] <- rank(intens[selection])
        pcgmembers[selection] <- length(selection)
    }
    pl[[colnls$rank]] <- pcgrank
    pl[[colnls$membernum]] <- pcgmembers
    
    return(pl)}
        


######isofind###################################################################################################

isofind <- function(pl = xset3.pos.pl,
                    mztol = 0.001,
                    rtsec = 3){
  pt1<-proc.time()
  #sort dataset by mz and see how large the mz gaps are between closest mz values
  pl <- pl[order(pl$mz),]
  pl$diff <-c(diff(pl$mz),0.1)
  pl$preselect1 <- cumsum(pl$diff)
  pl$preselect2 <-0
  
  for(z in c(1:floor(max(pl$preselect1)))){
    pl$preselect2[which.min(abs(z*1.00335-pl$preselect1))]<-1
  }
  
  
  plnc <- vector()
  plvc <- vector()
  
  
  counter<-1
  
  selector <- c(0,which(pl$preselect2==1,nrow(pl)))
  for (n in c(1:(length(selector)-2))){
    #this is at least a good approximation of the required search space for isotope searches!
    pls <- data.frame(mz=pl$mz[(selector[n+1]+1):(selector[n+2])],rt=pl$rt[(selector[n+1]+1):(selector[n+2])])
    srch <- data.frame(mz=pl$mz[(selector[n]+1):(selector[n+2])],rt=pl$rt[(selector[n]+1):(selector[n+2])])
    plv <- vector()
    plv[1:nrow(pls)] <- 0
    pln <- vector()
    pln[1:nrow(pls)] <- 1
    
    for (i in c(1:nrow(pls))){#
      # plsm <- pls[i+100,]
      # pln[i]  <- sum(abs(pls$mz-pls$mz[i])< pls$mz[i]*1e-6*mzppm
      #               & abs(pls$rt-pls$rt[i])< rtsec)
      #if(pls$mz[i]-(min(srch$mz))>=1.003350){
      # temp <- data.frame(mz=NA,rt=NA)
      temp    <- srch[which(srch$mz-pls$mz[i]>1.00335-mztol & srch$mz-pls$mz[i]<1.00335+mztol),]
      
      plv[i]  <-  if(sum(abs(temp$rt-pls$rt[i])<rtsec)>0){temp$mz[which.min(abs(temp$rt-pls$rt[i])<rtsec)]}else{0}#sum(abs(temp$rt-pls$rt[i])<rtsec)
      #}
      # if (i%%1000==0){cat(i, " ")}
      
    }
    plvc<-c(plvc,plv)
    #   plnc<-c(plnc,pln)
    
    if (n%%10==0){cat(selector[n], " ")}}
  
  filler <-vector()
  filler[1:selector[2]] <-0 
  
  #pl$redundancy<-plnc
  pl$isocheck<-c(filler,plvc)
  #pli<-pl[pl$isocheck!=0,]
  #pli$isocheck-pli$mz
  
  pt1 <- proc.time()-pt1
  cat(pt1)
  
  return(pl)
}
######sampid###############
sampid <- function(pl=filtrate1,
                   sfactor = 5,#fold change over max wiltype intensity value
                   wtc= c(10,11),# columns in pl which contain control sample intensities
                   scol= c(12:50), # colums in pl which contain test sample intensities
                   rem = "_XIC",
                   snames=NULL) # part of columnnames to be removed to get sample name
{
  samples <- vector()
  samples[1:nrow(pl)]<- ""
  nsamples <- vector()
  nsamples[1:nrow(pl)]<- 0
  
  cnames <- if (is.null(snames)) {colnames(pl)[scol]}else{snames}
  
  for (n in c(1:length(scol))){
    sc <- scol[n]
    rat <- pl[,sc]/rowMax(as.matrix(pl[,wtc]))
    
    sname <- gsub(rem,"",cnames[n])
    
    samples[which(rat>=sfactor)] <- paste(samples[which(rat>=sfactor)],sname)
    nsamples[which(rat>=sfactor)] <- nsamples[which(rat>=sfactor)]+1
  }
  
  
  samples <- gsub("[[:space:]]*$","", samples)
  samples <- gsub("^*[[:space:]]","", samples)
  #pl$maxfold <- rowMax(as.matrix(pl[,scol]))/rowMax(as.matrix(pl[,wtc]))
  pl$numsamples <- nsamples
  pl$upinsamples <- samples
  return(pl)}

##############densplot###########################

densplot <-function(densin = log10(as.numeric(unlist(filtrate3[,scol]))),#filtrate3$rt,#input values
                    perc = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 0.95, 0.99),
                    ... #pass arguments to plot()
                    ){
  
  #densin <- log10(filtrate3$maxfold)
  densin[densin==Inf] <- 1.1*max(densin[densin!=Inf])
  densin[densin==-Inf] <- 0.9*min(densin[densin!=-Inf])
  densin <- na.omit(densin)
  
  dens <- density(densin,from=min(densin),to=max(densin), cut=0, n=4096, na.rm = T)
  
  #dens$x[is.infinite(dens$x)] <- 1*max(dens$x[dens$x!=Inf])
  plot(dens, type= "l", ...)
  
 # perc <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 0.95, 0.99)
  
  quan <- quantile(densin,perc)
  
  ##plotquantile lines and legend
  colr <- rainbow(length(perc), s = 1, v = 1, start = 0, end = max(1, length(perc) - 1)/length(perc), alpha = 0.5)
  segments(quan,min(dens$y),quan,max(dens$y), col=colr, lwd=0.8)
  legendtext <- paste0(perc,": ",round(quan,4) )
  legend("topright", inset=c(0,0.03*max(dens$y)),legendtext, lty=1,lwd=2.5, col=colr, bty="n",  cex=.5)
}

###########################
##############densplot2D###########################

densplot2D <-function(densin = log10(rowvars),
                      densin2 = filtrate1$numsamples,#filtrate3$rt,#input values
                      quants = T, #print quantile lines
                      points = T, #print data points
                      perc = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 0.95, 0.99),
                      ...){#arguments to be passed to image2D
  
  nas <- unique(c(which(is.na(densin)),which(is.na(densin2))))
  
  #densin <- log10(filtrate3$maxfold)
  if (length(nas)>0) {densin <- densin[-nas]}
  densin[densin==Inf] <- 1.1*max(densin[densin!=Inf])
  densin[densin==-Inf] <- 0.9*min(densin[densin!=-Inf])
  
  if (length(nas)>0) {densin2 <- densin2[-nas]}
  densin2[densin2==Inf] <- 1.1*max(densin2[densin2!=Inf])
  densin2[densin2==-Inf] <- 0.9*min(densin2[densin2!=-Inf])

  

  
  dens2d <- kde2d(densin,densin2, n = 512)
  
  #dens$x[is.infinite(dens$x)] <- 1*max(dens$x[dens$x!=Inf])
  #plot(dens, type= "l")
  #plot(dens2, type= "l")
  image2D(dens2d, ...)
  # perc <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 0.95, 0.99)
  if(points){points(densin,densin2, pch=3, cex = 0.5)}
  if(quants){
  dens <- density(densin,from=min(densin),to=max(densin), cut=0, n=4096, na.rm = T)
  dens2 <- density(densin2,from=min(densin2),to=max(densin2), cut=0, n=4096, na.rm = T)  
  quan <- quantile(densin,perc)
  quan2 <- quantile(densin,perc)
  ##plotquantile lines and legend
  colr <- rainbow(length(perc), s = 1, v = 1, start = 0, end = max(1, length(perc) - 1)/length(perc), alpha = 0.5)
  par(xpd=F)
  segments(quan,min(dens2$x),quan,max(dens2$x), col=colr, lwd=0.8)
  segments(min(dens$x),quan2,max(dens$x),quan2, col=colr, lwd=0.8)
  legendtext <- paste0(perc,": ",round(quan,4),"/",round(quan2,4) )
  legend("topright", inset=c(0,0),legendtext, lty=1,lwd=2.5, col=colr, bty="n",  cex=.5)
  }
  }

############gausscheck()####
gausscheck <- function(inp=allfilt,#an input dataframe with a column $upinsamples from the sampid function
                       rawdata,
                       gauss=T,
                       abovex=F){ #a list object with xcmsraw objects, no list substructures allowed
  #optimized 170310
  #grab file names from xcmsraw objects in rawdata list to make a vector of xcmsraw items in list
  filesel <- vector()
  

  rawdata <- unlist(rawdata, recursive=T)
  
  
  for (n in c(1:length(rawdata))){
    filesel[n] <-  sub("^([^.]*).*", "\\1",basename(rawdata[[n]]@filepath@.Data))}
  
  filesel <- gsub("-",".",filesel)
  
  #filesel <-  sub("^([^.]*).*", "\\1",basename(as.character(filelist)))
  if(gauss){inp$gauss <- " "}
  if(abovex){inp$abovemean <- " "}
  
  #split up the upinsamples column into individual file/sample name descriptors to be searched for
    for(n in c(1:length(filesel))){
      
   subsel <- inp[which(inp$upinsamples %like% filesel[n]),]

   if(nrow(subsel)>0){
  #for each feature in the input dataframe
     # for each raw file that is listed in upinsamples for this feature, get the EIC (gauss)

      microl <-    multiEIC(rawfile= rawdata[[n]] ,
                            featuretable=data.frame(numeric(nrow(subsel))),
                            mz = subsel$mz,
                            ppm=5,
                            rtw= data.frame(subsel$rtmin-5,subsel$rtmax+5),
                            mini=5000,
                            coltag="exp",
                            pval=0.05,
                            XIC=F,
                            abovex = abovex,
                            gauss= gauss)
    
    #get the gauss information for the feature into a new column of the input df
    if(gauss){inp$gauss[which(inp$upinsamples %like% filesel[n])] <- paste(inp$gauss[which(inp$upinsamples %like% filesel[n])], 
                                                                           round(microl$exp_Gauss,4),collapse=NULL)}
    if(abovex){inp$abovemean[which(inp$upinsamples %like% filesel[n])] <- paste(inp$abovemean[which(inp$upinsamples %like% filesel[n])], 
                                                                             round(microl$exp_Abovemean,4),collapse=NULL)}
      
    if (n%%500==0){cat(n, " ")}}
    }
  inp$gauss <- trimws(inp$gauss)
  inp$abovemean <- trimws(inp$abovemean)
  
  #Make a vector with the max gauss value for each feature
  if(gauss){
  inp$gauss <- trimws(inp$gauss)
  inp$gaussmax <- strmax(inp$gauss)}
  if(abovex){
  inp$abovemean <- trimws(inp$abovemean)
  inp$abovemeanmax <- strmax(inp$abovemean)}
  return(inp)
}


##### quanquot() #####

#calculate fold change of maximum in a row of n columns over a defined quantile of the row's values
# outputs a vector of 
quanquot <- function(inp, #input dataframe
                     cols = c(1,2), #columns of dataframe to use for calculation
                     quanti = 0.75){ #define the quantile over which to calculate the fold change
  quanx <-vector()
  for (n in c(1:nrow(inp))){
    quanx[n] <- (rowMax(as.matrix(inp[n,cols]))/(quantile(inp[n,cols],quanti)))
  }
  return(quanx)
}

##### rowVar() #####

#calculate Variance across coluns for each row
# outputs a vector of numeric variance values
rowVar <- function(inp, #input dataframe
                     cols = c(1,2) #columns of dataframe to use for calculation
                     ){ 
  quanx <-vector()
  for (n in c(1:nrow(inp))){
    quanx[n] <- var(as.numeric(inp[n,cols]))
  }
  return(quanx)
}

##########normdata()###########
NormDataCols <- function(inp,
                     cols = c(1),
                     replace=T,
                     suffix="_norm"){
  if (replace){
  for (n in c(1:length(cols))){
    cl <-cols[n]
    inp[,cl] <- inp[,cl]/(mean(inp[,cl]))
    colnames(inp)[cl]<- paste0(colnames(inp)[cl],suffix)
  } }else{
    for (n in c(1:length(cols))){
      cl <-cols[n]
      inp$normxx <- inp[,cl]/(mean(inp[,cl]))
      colnames(inp)[which(names(inp)=="normxx")]<- paste0(colnames(inp)[cl],suffix)
    }}
  return(inp)
}
  
  
##########normdata()###########
NormDataCols <- function(inp,
                         cols = c(1),
                         replace=T,
                         suffix="_norm"){
  if (replace){
    for (n in c(1:length(cols))){
      cl <-cols[n]
      inp[,cl] <- inp[,cl]/(mean(inp[,cl]))
      colnames(inp)[cl]<- paste0(colnames(inp)[cl],suffix)
    } }else{
      for (n in c(1:length(cols))){
        cl <-cols[n]
        inp$normxx <- inp[,cl]/(mean(inp[,cl]))
        colnames(inp)[which(names(inp)=="normxx")]<- paste0(colnames(inp)[cl],suffix)
      }}
  return(inp)
}  
  
#####ttestx()####

ttestx <- function(x=c(1,2,2,3,3,3,4,4,4,4,4,5,5,5,6,6,7) #input vector
                      , ltail=T,
                    over= NULL,
                    calc = NULL){
  #http://www.r-tutor.com/elementary-statistics/hypothesis-testing/upper-tail-test-population-mean-unknown-variance
  
  if (is.null(over)){over <- c(1:length(x))} 
  if (is.null(calc)){calc <- c(1:length(x))} 
  
  
  m <- mean(x[over])
  s <-sd(x[over])
  n <- length(x[over])
  
  #test statistic
  fx <- function(x) (m-x)/(s/sqrt(n))
  t <- sapply(x[calc], fx )
  
  #ttest
  fy <- function(x) pt(x, df=n-1, lower.tail = ltail)
  pvals <- sapply(t, fy)
  
  return(pvals)
  
}

#########heatmapx()########
heatmapx <- function(inp = filtrate2,
                     cols = c(10:50),
                     rel = T,
                     log = T,
                     ...){
  
  heatq <- as.matrix(inp[,cols])
  rownames(heatq) <- paste0(round(inp$mz,5),"_",(round(inp$rt,1)))
  
  if(rel){heatq <- heatq/(rowMeans(heatq))}
  if (log){heatq <- log10(heatq)}
  
  heatq[heatq==Inf] <- 1.1*max(heatq[heatq!=Inf])
  heatq[heatq==-Inf] <- 0.9*min(heatq[heatq!=-Inf])
  
  #cat("NA: ",sum(is.nan(heatq)))
  
  heatmaply(heatq,
            scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
              low = "blue", high = "red", midpoint = mean(heatq), limits = c(min(heatq), max(heatq))), ...)
}


####strmax()#########
  
strmax <- function(x){
  
  fil<-  strsplit(x," ")
  tt<- as.numeric(sapply(fil, max))
return(tt)
}

####kth###
#returns vector of k-largest value from each row of a dataframe
kth <- function(x, k){
  
  k <- ncol(x)+1-k
  
  return(as.vector(unlist(sapply(split(unname(x), seq(nrow(x))),sort)[k,])))
}

#######mttest##
# apply rowwise ttest between two dataframes of equal nrow, returns vector of p-values 
mttest <- function (x,y){
  
  xl <- as.list(data.frame(t((x))))
  yl <- as.list(data.frame(t((y))))
  
  listo <- mapply(sttest,xl, 
                  yl, SIMPLIFY = F)
  
  return(sapply(listo,"[[",3))}


#######msana1###
# analyzes dataframe columns of groups of samples, returns df with additional information:
# maxfold (now all values >=1), group_up(upregulated in which sample group)
# can normalize data frames prior to other processing (replaces original values)
# can do ttest between groups
msana1 <- function (pl,
                  groups = list(wtc,scol),
                  groupnames = c("N2", "Mut"),
                  norm=T,
                  ttest=T){
  
  if(norm){pl <- NormDataCols(pl,cols=unlist(groups),replace=T,suffix = "")}
  pl$maxfold <- rowMax(as.matrix(pl[,groups[[2]]]))/rowMax(as.matrix(pl[,groups[[1]]]))
  pl$group_up <- ""
  pl$group_up[which(pl$maxfold>1)] <- groupnames[2]
  pl$group_up[which(pl$maxfold<1)] <- groupnames[1]
  pl$maxfold[which(pl$maxfold<1)] <- 1/(pl$maxfold)[which(pl$maxfold<1)]
    
  pl$massdefppm <- ((pl$mz-floor(pl$mz))/pl$mz)*1e6
  if(ttest){pl$pval <- mttest(pl[,wtc],pl[,scol])}
  return(pl)
  
}

#####msampid()####
# run sampid on multiple groups
msampid <- function(pl, 
                    groups = list(wtc,scol),
                    groupnames = c("N2", "Mut"),
                    ...){ 

pl2 <- sampid(pl=pl[which(pl$group_up==groupnames[1]),], #sampid checks the values of which samples are above the control*sfactor
                        wtc= unlist(groups)[which(!unlist(groups) %in% groups[[1]])],# columns in pl which contain control sample intensities
                        scol= groups[[1]], # colums in pl which contain test sample intensities (upregulated features)
                        snames = gsub("_XIC","",colnames(pl)[groups[[1]]]),
                        ...)

for (i in c(2:length(groups))){
  if(sum(pl$group_up==groupnames[i])>0){
  pl2 <- rbind(pl2, sampid(pl=pl[which(pl$group_up==groupnames[i]),], #sampid checks the values of which samples are above the control*sfactor
                  wtc= unlist(groups)[which(!unlist(groups) %in% groups[[i]])],# columns in pl which contain control sample intensities
                  scol= groups[[i]], # colums in pl which contain test sample intensities (upregulated features)
                  snames = gsub("_XIC","",colnames(pl)[groups[[i]]]),
                  ...))}
return (pl2)
  
  }
}

#######msana2###
# analyzes dataframe columns of groups of samples, returns df with additional information:
# a new column for each sample group that contains the average fold change grup/ all other samples,
# relative standard deviation within group, pvalue and adjusted pvalue.
# also counts the number of samples in group above fold threshold in GROUP 1 and above fold in all other groups
# pvalue calculation is different if either group or non group is only one sample (since then not comparing two populations)
# list of groups has to have names!
# can normalize data frames prior to other processing (replaces original values)
# can do ttest between groups
msana2 <- function (pl = f2gf[1:20,],
                    groups = glist,
                    norm=T,
                    ttest=T,
                    adjmethod='holm',
                    fold =10){
  
  if(norm){pl <- NormDataCols(pl,cols=unlist(groups),replace=T,suffix = "")}

  count <-1
  for (i in groups){
    noni <- unlist(groups)[which(!unlist(groups) %in% i)]
    meanint <- rowMeans(as.matrix(pl[,i]))
    maxint <- rowMax(as.matrix(pl[,i]))
    fold2 <- rowMeans(as.matrix(pl[,i]))/rowMeans(as.matrix(pl[,noni]))
    sdev <- sapply(as.list(data.frame(t((pl[,i])))),sd)/rowMeans(as.matrix(pl[,i]))
    sdev2 <- sapply(split(pl[,i],seq(nrow(pl))),sd)/rowMeans(as.matrix(pl[,i]))
    sdev[which(is.na(sdev))] <-0
    if(ttest){if (min(length(i),length(noni)) > 1){
                        pval <- mttest(pl[,i],pl[,noni])}
                  else{ pval <- sapply(as.list(data.frame(t((pl[,c(i,noni)])))),
                                       ttestx,calc=1,over=c(2:(length(i)+length(noni))))
                        pval[which(pval>0.5)] <- 1-pval[which(pval>0.5)]}
              padj <- p.adjust(pval, method = adjmethod)}
    
    overg1 <- mapply(minfolds,as.list(data.frame(t((pl[,groups[[1]]])))),as.list(data.frame(t((pl[,i])))),MoreArgs = list(fold=fold, mode="Max"))
    overall <- mapply(minfolds,as.list(data.frame(t((pl[,noni])))),as.list(data.frame(t((pl[,i])))),MoreArgs = list(fold=fold, mode="Average"))
    
    
    options(scipen = -100, digits = 4)
    pl[[paste0(names(groups)[count],"_stats")]] <- gsub("\\s+", " ", trimws(paste(format(fold2,digits=5),format(sdev,digits=5),
                                                                if(ttest){format(pval,digits=5)},if(ttest){format(padj,digits=5)},
                                                                format(overg1,digits=3),format(overall,digits=3),
                                                                format(meanint,digits=3),format(maxint,digits=3))))
    options(scipen = 1, digits = 10)
    
    
    count=count+1
  }
  
  pl$massdefppm <- ((pl$mz-floor(pl$mz))/pl$mz)*1e6
  
  return(pl)
  
}

#######groupfilter()
#' find features meeting thresholds for each sample group
#' requires msana2() analyzed dataframe
groupfilter <- function (pl,
                         groups = glist,
                         minfold=0,
                         maxfold=Inf,
                         maxsd=1,
                         maxpval=1,
                         maxpadj=1,
                         overg1=0, #how many samples (fraction of total in group) have to be above control*fold
                         overall=0){ #how many samples (fraction of total in group) have to be above allgroups*fold

  pl$groups <- ""

  for (i in names(groups)){
    pl[[paste0(i,"_stats")]][1]
    tb <- do.call(rbind,strsplit(pl[[paste0(i,"_stats")]], split=" "))
    sel <- which(as.numeric(tb[,1])>=minfold
                 & as.numeric(tb[,1])<=maxfold
                 & as.numeric(tb[,2])<=maxsd
                 & as.numeric(tb[,3])<=maxpval
                 & as.numeric(tb[,4])<=maxpadj
                 & as.numeric(tb[,5])>=overg1*length(groups[[i]])
                 & as.numeric(tb[,6])>=overall*length(groups[[i]])
                 )
    pl$groups[sel] <- paste(pl$groups[sel],i)
  } 
  return(pl)
}

##minfolds
# returns number of values in y that are fold times higher than max or average value in x
minfolds <- function (x, # vector with values to compare y values to
                      y, # values
                      fold=10, #min fold change
                      mode='Max'){
  max(0,0)
  
  if (mode=="Max"){
    vec <- y/max(x)
    vec[which(is.na(vec))]<-0
    return(sum(vec>=fold))
  }
  
if (mode=="Average"){
  vec <- y/mean(x)
  vec[which(is.na(vec))]<-0
  return(sum(vec>=fold))
}
}
  
###splitter()

splitter <- function (x,
                      sep=" ",
                      coln = NULL){
  x<- as.character(x)
  x <- do.call(rbind,strsplit(x, split=sep))
  colnames(x) <- coln
  return(x)
}

####foldgr

foldgr <- function (pl = f1[1:2,],
                    groups = glist,
                    mode ="Max"){ #or "Average"
  
 if (mode == "Max"){f <-rowMax}else{f<-rowMeans}
  
  count <-1
  for (i in groups){
    fold <- vector(mode='character', length=nrow(pl))
    noni <- unlist(groups)[which(!unlist(groups) %in% i)]
    for (n in i){
      options(scipen = -100, digits = 4)
      fold <- paste(fold, trimws(format(pl[,n]/f(as.matrix(pl[,noni])),digits=3)))}
    

    pl[[paste0(names(groups)[count],"_fold_",mode)]] <- trimws(fold)
   # options(scipen = 1, digits = 10)
    count=count+1
  }
  
  return(pl)}

### a solution to the multi-factor filtering problem
whichvec <- function(obj=d,
                     fil=t[1,which(t[1,]!="")]){
  
  cn <- colnames(fil)[which(fil[1,]!="")]
 # print(cn)
 # print(cn[which(cn %in% colnames(obj))])
  whi=1:nrow(obj)
  whit <- whi
  
  for (i in cn[which(cn %in% colnames(obj))]){
    
    mm <- splitter(as.character(fil[,i])) 
    #print(mm)
    if(ncol(mm)==1){
    whi <- which(as.numeric(obj[,i]) >= as.numeric(mm[1]))
    }else{
      if(mm[1]==''){
      whi <- which(as.numeric(obj[,i]) <= as.numeric(mm[2]))}
    else{
    whi <- which( as.numeric(obj[,i]) >= as.numeric(mm[1])
                  & as.numeric(obj[,i]) <= as.numeric(mm[2]))
    }
    }
   # print(whi)
    wx <- c(whit,whi)
    whit <- as.numeric(names(table(wx)[which(table(wx)==2)]))
  #print(whit)
  }
  
  return(unique(whit))
  
}

###dftrans()
#### transfer data from one df to the other
dftrans <- function(a,b){

    for (i in colnames(a)[which(colnames(a) %in% colnames(b))]){
      b[c(1:(length(a[,i]))),i] <- as.character(a[,i])}
  
  print(paste('DFtrans from',deparse(substitute(a)),'to',deparse(substitute(b))))
  return(b)
}


#######
###note that the last scan will always be omitted because of an error in methods-xcmsRaw (c(match(TRUE, scanidx), length(scanidx) - match(TRUE, rev(scanidx)))[2] should have +1)
rawread <- function(x, ...){
sc <- rawEIC(x, ...)
sc$rt <- x@scantime[sc$scan]
sc$tic <- x@tic[sc$scan]
sc$fname <- x@filepath[[1]]
sc$maxint <- max(sc$intensity)
sc$maxtic <- max(sc$tic)
return(sc)}
  

##################
#custom plotting for MS spectra
specplot <- function (x=tt2[,1],
                      y=tt2[,2],
                      norm=max(y)/100,
                      cx=1.5,
                      k = 10,
                      fileName = basename(rawdata[[1]][[1]]@filepath),
                      yrange = c(0,100),
                      xrange = range(x),
                      maxi = max(y),
                      ...
                      ){

  pd <- data.frame(x=x,y=y/norm)  
  par(mar=c(5,4,10,2))
plot(pd$x,pd$y,type="h", bty="n", axes=F,lwd=0.8,
     main=fileName, cex.main=0.5*cx, ann=FALSE, ylab="Relative intensity", 
     xlab= expression(italic(m/z)), 
     xaxs="i",yaxs="i",
     xlim=xrange,
     ylim=yrange,
     ...)


  currview <- pd[which(pd$y <= max(yrange)
                         & pd$y >= min(yrange)
                         & pd$x <= max(xrange) 
                         & pd$x >= min(xrange)),]
 
  if (length(currview$y) >= k){
  kn <-  sort(currview$y, decreasing = T)[k]
  labs <- currview[which(currview$y>=kn),]
  }else{
  labs <- currview}

  if(nrow(labs) > 0 ){
  labs$xcorr <- spread.labs(labs[,1],1.05*strwidth("A"), maxiter=1000, min=min(labs[,1]), max=max(labs[,1]))
  par(xpd=NA)
  
  segments(labs[,1],labs[,2]+0.01*max(yrange),labs$xcorr,labs[,2]+0.05*max(yrange), col="red", lwd=0.8)
  text(labs$xcorr,labs[,2]+0.055*max(yrange),labels=labs[,1], col="blue3", srt=90,adj=c(0,0.3), cex=0.5*cx)
  mtext(side=3, text=format(maxi*(max(labs$y)/100), scientific = T, digits =4), line=0, cex=0.5*cx, adj=1)
  }
  
mtext(side=1, text= expression(italic(m/z)), line=0.7, cex=0.5*cx)
mtext(side=2, text="Relative intensity (%)", line=1.1, cex=0.5*cx)
mtext(side=1, text=fileName, line=1.2, cex=0.5*cx)
#mtext(side=3, text=Ptext, line=0.6, cex=0.5, adj=1)
par(cex.axis=0.5*cx, tcl=-0.3)            
axis(side=1, lwd=1, minor.tick(nx=10,ny=5, tick.ratio=0.5), mgp=c(0.5,0,0)) #x-axis mgp[2] controls distance of tick labels to axis
axis(side=2, lwd=1, las=2, mgp=c(0.5,0.4,0)) #y-axis
}

#Find all xcmsRaw objects that are in a vector of filenames
rawselect <- function(namelist,rawlist){
  
  cl <- vector()
  for (i in rawlist)
    cl<- c(cl, basename(i@filepath[1]) %in% namelist)
  
  return(rawlist[which(cl)])
  
  
}

#return xcmsRaw object that corresponds to a filename
rawselect2 <- function(namex,rawlist){
    
     for (i in unlist(rawlist)){
        if(basename(i@filepath[[1]]) == namex){return (i)}}
    
    print("rawselect2: file not found!")
    
}
#####Assemble a Chemcalc mz query

massquery <- function(mz, range=0.01, ppm=5,
                    elem= "C0-100H0-202N0-10O0-10F0-3Cl0-3Br0-1",
                    charge = 1){

    charge <- as.numeric(charge)
    if (charge > 0 ){charge2 <- paste0("%2B",abs(charge))}
    if (charge < 0 ){charge2 <- paste0("-",abs(charge))}
    if (charge == 0 ){charge2 <- paste0("")}
    
    
         if (!is.null(ppm)){range <- as.numeric(mz)*ppm*1e-6}             
                      
    mzq <-  paste0("http://www.chemcalc.org/service?action=em2mf&monoisotopicMass=",
                  mz,"&massRange=",range,"&mfRange=",
                  elem,
                  "(",charge2,")")
    res <- fromJSON(mzq)
    return(res$results)}

###Default data analysis method for mass spec result table####
####MSTana######
MSTana <- function(pl=plist_own,
                   glist = glist,
                   ctrl = glist[1],
                   normalize = T,
                   pval = T){
    
    wtc <- unlist(ctrl)
    scol <- unlist(glist)[which(!unlist(glist) %in% wtc)]
    
    #Raise the floor
    
    pl <- msana1(pl,
                 groups = glist,
                 groupnames = names(glist),
                 norm=normalize,
                 ttest=F)
    
    #replace 0 in any column of interest by the smallest value in any of these columns (detection limit)
    pl[which(pl==0, arr.ind=T)[which(which(pl==0, arr.ind=T)[,2] %in% c(wtc,scol)),]]<- min(as.matrix(pl[,c(wtc,scol)])[which(as.matrix(pl[,c(wtc,scol)])>0)])
    
    #Recalculate maxfold change
    pl <- msana1(pl,
                 groups = glist,
                 groupnames = names(glist),
                 norm=F,
                 ttest=F)
    
    ##Filtering suspended####
    #sthresh = quantile(as.matrix(pl[,c(wtc,scol)])[which(as.matrix(pl[,c(wtc,scol)])>min(pl[,c(wtc,scol)]))],0.05) #scol noise threshold
    #sfactor <- 3 #min fold change
    #everything that is upregulted in scol only (very mild filtering)
    #f1    <- pl[which(pl$maxfold  >= sfactor
    #                  & rowMax(as.matrix(pl[,c(wtc,scol)]))>=sthresh
    #                  & pl$group_up == "Exp"
    #                  & pl$massdefppm <800 & pl$massdefppm >=0
    #),]
    
    ##label features which are sfactor fold over ctrl... suspended
    #f1 <- msampid(pl, 
    #             groups = glist,
    #            groupnames = names(glist),
    #           sfactor=10,
    #          rem="X")
    
    
    #' Get specific per sample
    ##gausscheck suspended
    #pl <- gausscheck(pl,rawdata,gauss=T,abovex=T)
    
    #write.csv(pl, file = paste0("autofiltered_v9_pl.csv"))
    
    pl <- foldgr(pl, groups = glist, mode="Max")
    pl <- foldgr(pl, groups = glist, mode="Average")
    
    
    pl$maxint <- rowMax(as.matrix(pl[,unlist(glist)]))
    pl$maxfoldgr_max <- rowMax(apply(do.call(cbind,lapply(pl[,grep("fold_Max",colnames(pl))],splitter)),2,as.numeric))
    pl$maxfoldgr_ave <- rowMax(apply(do.call(cbind,lapply(pl[,grep("fold_Average",colnames(pl))],splitter)),2,as.numeric))
    
    
    #f2gf <- f2g[which(f2g$abovemeanmax >= 5
    #                 &f2g$gaussmax >=0.9
    #                &f2g$group_up == "Exp"
    # & filtrate1scol$maxvar < 20
    #),]
    
    ###Making groups (column numbers for each group)
    pl <- msana2(pl,
                 groups = glist,
                 norm=F,
                 ttest=pval,
                 adjmethod='holm',
                 fold = 10)
    
    if(pval){
        groupsplitnames <- c("mean_fold_over_other_groups", "sdev", "pval", "pval_adjusted", "samples_10fold_over_ctrl", "samples_10fold_over_other_groups", "mean_intensity", "max_intensity")
    }else{
        groupsplitnames <- c("mean_fold_over_other_groups", "sdev", "samples_10fold_over_ctrl", "samples_10fold_over_other_groups", "mean_intensity", "max_intensity")
    }
    return(list(data=pl,groupsplitnames=groupsplitnames))
}

##failsafe ttest
sttest <- function(out=NA,...){
    res <-try(t.test(...), silent = T)
    if(is(res,"try-error")){return(out)}else{return(res)}
}

###find MS1 and MS2 scans for parent masses in any number of xcmsraw objects
setClass("MSps", slots= c( featuremz="vector",featurert="vector", 
                           MS1meta = "data.frame",
                           MS2meta="data.frame"))  


Parentsearch <- function (bin1_p, mzs = c(702.3249,451.26903),rts = c(400,300), partol= 0.02, rttol=200, MSn = T, MS1 = T){
    
    if(MS1==T){
        mzlist=list() 
        rawlist=list()
        rawcoll=list()
        for (n in c(1:length(mzs))){
            for(t in c(1:length(bin1_p))){
                for (numb in c(1:length(bin1_p[[t]]))){
                    rawlist[[numb]] <- data.frame(getScan(bin1_p[[t]][[numb]],which.min(abs(bin1_p[[t]][[numb]]@scantime-rts[n]))))}#, mzrange= c(mzs[n]-2,mzs[n]+5) ))}
                rawcoll[[t]] <-rawlist
                rawlist=list()}
            mzlist[[n]] <-rawcoll
            rawcoll=list()}
        MS1speclist <- mzlist   
        
        mzlist=list() 
        rawlist=list()
        rawcoll=data.frame(scan=integer(),rt=numeric(),scannum=integer(),MS1target=numeric(),MS1hit=numeric(),MS1hitI=numeric(),MS1ppm=numeric(),file=character(), stringsAsFactors = F)
        for (n in c(1:length(mzs))){
            for(t in c(1:length(bin1_p))){
                for (numb in c(1:length(bin1_p[[t]]))){
                    scan <- which.min(abs(bin1_p[[t]][[numb]]@scantime-rts[n]))
                    rawlist[[numb]] <- data.frame(scan)
                    rawlist[[numb]]$rt <- bin1_p[[t]][[numb]]@scantime[scan]
                    rawlist[[numb]]$scannum <- bin1_p[[t]][[numb]]@acquisitionNum[scan]
                    rawlist[[numb]]$MS1target <- mzs[n]
                    rawlist[[numb]]$MS1hit <- MS1speclist[[n]][[t]][[numb]]$mz[which.min(abs(mzs[n]-MS1speclist[[n]][[t]][[numb]]$mz))]
                    rawlist[[numb]]$MS1hitI <- MS1speclist[[n]][[t]][[numb]]$intensity[which.min(abs(mzs[n]-MS1speclist[[n]][[t]][[numb]]$mz))]
                    rawlist[[numb]]$MS1ppm <- (abs((MS1speclist[[n]][[t]][[numb]]$mz[which.min(abs(mzs[n]-MS1speclist[[n]][[t]][[numb]]$mz))]-mzs[n]))/mzs[n])*1000000
                    rawlist[[numb]]$file <- basename(bin1_p[[t]][[numb]]@filepath[[1]])
                    
                }
                rawcoll <-rbind(rawcoll,rbindlist(rawlist))
                rawlist=list()}
            mzlist[[n]] <-rawcoll
            rawcoll=list()}
        MS1metalist <- rbindlist(mzlist)
    }else{
        MS1metalist =data.frame()}
    
    if(MSn==T){
        #output list: [[mz]][[categorz in mzxml_pos]][[file in category]] $mz and $intensity
        rawlist=list()
        msnlist=list()
        metalist=list() 
        rawcoll=list()
        
        for (n in c(1:length(mzs))){
            for(t in c(1:length(bin1_p))){
                for (numb in c(1:length(bin1_p[[t]]))){
                    scans <- which(abs(bin1_p[[t]][[numb]]@msnPrecursorMz-mzs[n])<partol
                                   & abs(bin1_p[[t]][[numb]]@msnRt-rts[n])<rttol)
                    if(length(scans)>0){
                        for(s in c(1:length(scans))){
                            msnlist[[s]]<-data.frame(scans[s], stringsAsFactors = F)
                            msnlist[[s]]$msnPrecursorMz <- bin1_p[[t]][[numb]]@msnPrecursorMz[scans[s]]
                            msnlist[[s]]$rt <- bin1_p[[t]][[numb]]@msnRt[scans[s]]
                            msnlist[[s]]$scannum <- bin1_p[[t]][[numb]]@msnAcquisitionNum[scans[s]]
                            msnlist[[s]]$msnPrecursorIntensity <- bin1_p[[t]][[numb]]@msnPrecursorIntensity[scans[s]]
                            msnlist[[s]]$msnCollisionEnergy <- bin1_p[[t]][[numb]]@msnCollisionEnergy[scans[s]]
                            msnlist[[s]]$file <- basename(bin1_p[[t]][[numb]]@filepath[[1]])
                        }}else{msnlist[[1]] <- data.frame(scans=integer(),msnPrecursorMz=numeric(),rt=numeric(),scannum=numeric(),msnPrecursorIntensity=numeric(),msnCollisionEnergy=numeric(),file=character(), stringsAsFactors = F)}
                    rawlist[[numb]] <- rbindlist(msnlist)
                    msnlist=list()
                }
                rawcoll[[t]] <-rbindlist(rawlist)
                rawlist=list()}
            metalist[[n]] <-rbindlist(rawcoll)
            rawcoll=list()}
        
    }else{scanlist=list()
    metalist=data.frame()}
    
    
    
    tata <- new("MSps",  featuremz = mzs, featurert = rts, 
                MS1meta=MS1metalist,
                MS2meta=rbindlist(metalist)
    )
    
    return(tata)}

## draw single molecule from smile
psmile <- function(SMILE, width=500, height=500){
    
    mols <- parse.smiles(SMILE)    
    par(mar=c(0,0,0,0)) # set margins to zero since this isn't a real plot
    plot(NA,NA,xlim=c(1,10),ylim=c(1,10),xaxt='n',yaxt='n',xlab='',ylab='') 
    temp1 = view.image.2d(mols[[1]],width,height)    
    rasterImage(temp1,1,1,10,10, col = "black")  
}

######### Decode an annotation string with mz, sum formula and SMILES
decodeanno <- function(x, sep1="*", sep2 = " "){
    tinp2 <- strsplit(x, sep1, fixed = T)[[1]]
    
    mz <- numeric(0)
    sum_formula <- character(0)
    SMILE <- character(0)
    #emp <- data.frame(mz=numeric(0),sum_formula=character(0),SMILE = character(0))
    for (i in tinp2){
        tinp3 <- strsplit(i, sep2)[[1]]
        mz <- c(mz,as.numeric(tinp3[1]))
        sum_formula <- c(sum_formula,as.character(tinp3[2]))
        SMILE <- c(SMILE,as.character(tinp3[3]))
    }
    return(data.frame(mz,sum_formula,SMILE, stringsAsFactors = F))}

encodeanno <- function(x, sep1="*", sep2 = " "){
    coll <-""
    if (nrow(x)==0){return("")}
    
    item <- paste(x[1,1],x[1,2],x[1,3], sep = sep2)
    if (nrow(x)>1){
    for ( n in 2:nrow(x)){
       itemt <- paste(x[n,1],x[n,2],x[n,3], sep = sep2)
       item <- paste(item,itemt, sep=sep1)
    }}
    return(item)}

#### get msn scans by actual acquisition number (scannum)
getMSnOwn <-  function(object,
                       scannum,
                       mzrange = numeric()) {
    
    scan <- which(object@msnAcquisitionNum==scannum)
    
    if (scan < 0)
        scan <- length(object@msnRt) + 1 + scan
    
    idx <- seq(object@msnScanindex[scan]+1, min(object@msnScanindex[scan+1],
                                                length(object@env$msnMz), na.rm=TRUE))
    
    if (length(mzrange) >= 2) {
        mzrange <- range(mzrange)
        idx <- idx[object@env$msnMz[idx] >= mzrange[1] & object@env$msnMz[idx] <= mzrange[2]]
    }
    
    points <- cbind(mz = object@env$msnMz[idx], intensity = object@env$msnIntensity[idx])
    
    invisible(points)
}