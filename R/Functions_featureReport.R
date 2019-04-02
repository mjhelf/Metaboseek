#' featureReport
#'
#' 
#' @param pdf_settings if NULL, plot to current device, otherwise takes list with items file, height and width. If height and/ or width are NULL, plot to A4 format pdf with some safeguards increasing width and hieght as necessary
#' @param layout_settings list of arguments passed to layout(), OR an integer defining how many columns to use for the EIC plots
#' @param EICplots list of arguments passed to EICgeneral()
#' @param MS1 list of arguments passed to specplot()
#' @param MS2 list of arguments passed to specplot()
#' @param tree list of arguments passed to plotTree()
#' @param selectMS2 if not NULL, and if one of the groups in EICplots is called "MS2", only the EIC for this file will be shown in the MS2 EIC group
#' 
#'
#' @export
featureReport <- function(pdf_settings = list(file = "testReport.pdf", width = NULL, height = NULL),
                          layout_settings = 3,
                          EICplots = NULL,
                          MS1 = NULL,
                          MS2 = NULL,
                          tree = NULL,
                          fragments = NULL,
                          cx = 1,
                          selectMS2 = NULL){
  
  checknum <- function(x){if(length(x) == 0){0}else{max(x)}}
  
  
  if(is.null(layout_settings)){
    #use supplied layout
    layout_settings <- 3
  }
  
  if (is.numeric(layout_settings)){
    #calculate layout based on supplied number of columns
    nc <- as.integer(layout_settings)
    
    lv <- integer(0)
    
    if(!is.null(EICplots)){
      #make sure the external layout will be used, not column number
      EICplots$cols <- NULL
      
      lv <- c(lv, seq(length(EICplots$glist)))
      #fill the rows with empty plots:
      if(length(EICplots$glist)%%nc != 0){
        lv <- c(lv,rep(0,nc - length(EICplots$glist)%%nc))
        # smallrows <- length(lv)/nc
      }
    }else{
      #ignore number of columns if no EICs are plotted
      nc <- 3 
      #  EICrows <- 0
    }
    
    if(!is.null(MS1) || !is.null(MS2)){
      #set up rows for MS1 and MS2 scans:
      if(is.null(MS1) || is.null(MS2)){
        # if it is only one spectrum, fill a row with it
        lv <- c(lv, rep(checknum(lv)+1, nc))
      }else{
        
        ms1w <- ceiling(nc/4)
        ms2w <- max(1, nc - ms1w)
        lv <- c(lv, rep(checknum(lv)+1, ms1w), rep(checknum(lv)+2, ms2w))
        
        
      }
      
    }
    
    if(!is.null(tree)){
      lv <- c(lv, rep(checknum(lv)+1, nc))
    }
    
    heights <- rep(5.5,length(lv)/nc)
    if(!is.null(tree)){
      heights[length(heights)] <- 11 
    }
    
    if(length(lv)>0){
      
      layout_settings <- list(mat = matrix(lv, byrow = T, ncol = nc), heights = lcm(heights))
      
      #print(matrix(lv, byrow = T, ncol = nc))
     # print(lcm(heights))
      
    }else{
      layout_settings <- NULL
    }
  }
  
  if(!is.null(pdf_settings)){
    if(is.null(pdf_settings$height) 
       && !is.null(layout_settings)
       && !is.null(layout_settings$heights)){
      #automatically increase height of pdf to avoid errors with margins
      h <- length(layout_settings$heights) + !is.null(tree)
      pdf_settings$height <- 11.69 + max(0,(h-5))*(11.69/5)
    }
    
    if(is.null(pdf_settings$width) 
       && !is.null(layout_settings)
       && !is.null(layout_settings$mat)){
      #automatically increase width of pdf to avoid errors with margins
      pdf_settings$width <- 8.27 + max(0,(ncol(layout_settings$mat)-4))*(8.27/4)
    }
    
    do.call(pdf, pdf_settings)
  }
  
  if(!is.null(layout_settings)){
    do.call(layout, layout_settings)
  }
  
  if(!is.null(EICplots)){
    
    if(!is.null(selectMS2) && "MS2" %in% names(EICplots$glist)){
      

      MS2here <-  which(EICplots$glist$MS2 %in% selectMS2)
      
      #this way making sure all selected items were in original list
      EICplots$glist$MS2 <- EICplots$glist$MS2[MS2here]
      

      if(is.list(EICplots$colrange)){
        
        #remove the group if no match
        if(length(EICplots$glist$MS2) == 0){
          EICplots$colrange <- EICplots$colrange[names(EICplots$colrange) != "MS2"]
        }else{
        
        EICplots$colrange$MS2 <- EICplots$colrange$MS2[MS2here,]
        EICplots$colrange$MS2$label <- sub("^([^.]*).*", "\\1",basename(EICplots$glist$MS2))
        }
      }
      #remove the group if no match
      if(length(EICplots$glist$MS2) == 0){
        EICplots$glist <- EICplots$glist[names(EICplots$glist) != "MS2"]
      }
      
     
      
    }
    
    
    EICplots$cx <- cx
    EICplots$margins <- c(2.7,2,4,0.5)
    EICplots$ylabshift <- 2.2
    EICplots$relPlot = T
    EICplots$raise = T
    
    do.call(EICgeneral, EICplots)
    
  }
  
  if(!is.null(MS1)){
    MS1$cx <- cx
    MS1$mar <- c(2.7,2,4,0.5)
    MS1$ylabshift = 2.2
    do.call(specplot, MS1)
  }
  
  if(!is.null(MS2)){
    
    if(!is.null(fragments) && length(fragments$fragments)> 0){

      inttemp <- sapply(fragments$fragments,function(x){x$relativeIntensity})
      mztemp <- sapply(fragments$fragments,function(x){x$mz})
      labs <- paste0(format(round(mztemp,5),nsmall = 5, scientific = F), " (", sapply(fragments$fragments,function(x){x$molecularFormula}), ")")
      
      if(any(inttemp>0)){
      MS2$labels <- data.frame(x = mztemp[inttemp>=0.02],
                               y = inttemp[inttemp>=0.02]*100,
                               label = labs[inttemp>=0.02],
                               stringsAsFactors = F)
      }
      
                     }
    
    MS2$cx <- cx
    MS2$mar <- c(2.7,2,4,0.5)
    MS1$ylabshift = 2.2
    if(!is.null(MS1)){
      MS2$ylab = ""
      }
    do.call(specplot, MS2)
  }
  
  if(!is.null(tree) && !is.null(tree$tree)){
    do.call(plotTree, tree)
    
  }
  
  if(!is.null(pdf_settings)){
    
    dev.off()
  }
}


#' plotTree
#'
#' plot a grViz- generated tree in an R plotting device
#'
#' @param tree DiagrammeR::grViz output object
#' @param resolution resolution  of the image along its longest edge
#'
plotTree <- function(tree, resolution = 2000, filename= NULL){
  
  
#  fn <- paste0(digest("a", algo = "xxhash32", seed = runif(1)*10000),'_temp.png')
  
  checkpackages <- c("DiagrammeRsvg", "xml2", "rsvg", "png")
  
  missing <- checkpackages[which(is.na( match(checkpackages, rownames(installed.packages()))))]
  
  if(length(missing)>0){
    
    plot(numeric(0),
         numeric(0),
         ylim = c(0,1),
         xlim = c(0,1),
         type = "n", ann = FALSE, bty = "n", axes = F, asp = 1)
    
    text(0.5,0.5, labels = paste0("Please install missing packages: \n",
                                  paste(missing, collapse = ", ")), adj = 0.5)
    
  }else{
  
  
  cc <- (DiagrammeRsvg::export_svg(tree))
  
 
  
  dat <- xml2::read_xml(cc)
  xml2::xml_ns_strip( dat )
  datdims <- xml2::xml_attrs(xml2::xml_find_all(dat,"//svg"))
  wi <- as.numeric(gsub("pt","",datdims[[1]]["width"]))
  hi <- as.numeric(gsub("pt","",datdims[[1]]["height"]))
  ar <- hi/wi
  
  
   if(!is.null(filename)){
     rsvg::rsvg_pdf(charToRaw(cc),
                    file = filename,
                    #fn,
                    width = wi, height = hi)
     
    
    }else{
 cap <- rsvg::rsvg_png(charToRaw(cc),
                       file = NULL,
                       #fn,
                       width = resolution, height = ar*resolution)
 
  
  par(#mfrow=c(1,2),
    #oma=c(0,2,0,0),
    mar = c(1,0,0,0),#oma causes issues in interactive mode
    # mai=c(0,0.5,0,0),
    xpd=FALSE,
    bg=NA,
    xaxs = "i", yaxs = "i"
  )
  plot(numeric(0),
       numeric(0),
       ylim = c(0,hi),
       xlim = c(0,wi),
       type = "n", ann = FALSE, bty = "n", axes = F, asp = 1)
  
  
  pic <- png::readPNG(cap)#fn)
 
  
  rasterImage(pic, 0, 0,wi,hi,interpolate = T)
    }
  }
}