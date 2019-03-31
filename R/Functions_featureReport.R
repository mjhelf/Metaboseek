#' featureReport
#'
#' 
#' @param pdf_settings if NULL, plot to current device, otherwise takes list with items file, height and width. If height and/ or width are NULL, plot to A4 format pdf with some safeguards increasing width and hieght as necessary
#' @param layout_settings list of arguments passed to layout(), OR an integer defining how many columns to use for the EIC plots
#' @param EICplots list of arguments passed to EICgeneral()
#' @param MS1 list of arguments passed to specplot()
#' @param MS2 list of arguments passed to specplot()
#' @param tree list of arguments passed to plotTree()
#' 
#'
#'
#' @export
featureReport <- function(pdf_settings = list(file = "testReport.pdf", width = NULL, height = NULL),
                          layout_settings = 3,
                          EICplots = NULL,
                          MS1 = NULL,
                          MS2 = NULL,
                          tree = NULL){
  
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
        ms2w <- nc - ms1w
        lv <- c(lv, rep(checknum(lv)+1, ms1w), rep(checknum(lv)+2, ms2w))
        
        
      }
      
    }
    
    if(!is.null(tree)){
      lv <- c(lv, rep(checknum(lv)+1, nc))
    }
    
    heights <- rep(5,length(lv)/nc)
    if(!is.null(tree)){
      heights[length(heights)] <- 10 
    }
    
    if(length(lv)>0){
      
      layout_settings <- list(mat = matrix(lv, byrow = T, ncol = nc), heights = heights)
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
    do.call(EICgeneral, EICplots)
  }
  
  if(!is.null(MS1)){
    do.call(specplot2, MS1)
  }
  
  if(!is.null(MS2)){
    
    do.call(specplot2, MS2)
  }
  
  if(!is.null(tree)){
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
plotTree <- function(tree, resolution = 2000){
  
  
  fn <- paste0(digest("a", algo = "xxhash32", seed = runif(1)*10000),'_temp.png')
  
  cc <- (DiagrammeRsvg::export_svg(tree))
  
  dat <- xml2::read_xml(cc)
  xml2::xml_ns_strip( dat )
  datdims <- xml2::xml_attrs(xml2::xml_find_all(dat,"//svg"))
  wi <- as.numeric(gsub("pt","",datdims[[1]]["width"]))
  hi <- as.numeric(gsub("pt","",datdims[[1]]["height"]))
  ar <- hi/wi
  
  rsvg::rsvg_png(charToRaw(cc),fn, width = resolution, height = ar*resolution)
  
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
  
  
  imp <- png::readPNG('test3.png')
  
  pic <- png::readPNG(fn)
  if(file.exists(fn)){
    cap <- file.remove(fn)
  }
  
  rasterImage(pic, 0, 0,wi,hi,interpolate = T)
  
  
}