context("Plotting functions")


test_that("peptide annotation and plotting works",{
    
    
    
    

    Mseek.colors(5,1)
    
    assignColor(1:5,c("1","2","3","4","5"), center = 4)
    
    assignColor(1:10,c("1","2","3","4","5"), center = 4)
    
    assignColor(1:20,c("1","2","3","4","5"), center = 4)
    
    crange <- colorRampPalette(c("blue", "gray", "red"))
    colr <- crange(200)
    
    drange <- c(-10:10)
    colorRampLegend(drange, assignColor(drange,colr, center = 0))
    
    drange <- c(-10:100)
    colorRampLegend(drange, assignColor(drange,colr, center = 0))
    colorRampLegend(drange, assignColor(drange,colr, center = 0, symmetric = T))
    
    drange <- 1:100
    colorRampLegend(drange, assignColor(drange,colr, center = 10))
    colorRampLegend(drange, assignColor(drange,colr, center = 30, symmetric = T))
    
    MseekExamplePreload(data=F)
    testdf <- Metaboseek:::analyzeTable(tab2$df, tab2$intensities,
                                       tab2$anagroupnames,
                                       analyze = c("Basic analysis"), 
                                       normalize = T,
                                       useNormalized = T,
                                       logNormalized = F,
                                       MSData = NULL,
                                       ppm = 5)
    
    
    
    assignColor <- function(datarange,
                            colscale,
                            scalerange =range(datarange),
                            center = NULL,
                            symmetric = F){
        
        ncolors <- length(colscale)
        
        
        
        if(max(abs(datarange)) > 0){
            if(!missing(center) && !is.null(center)){
                
                #First, set all color selections to the center value
                middleColInt <- as.integer((ncolors+1)/2)
                col <- rep(colscale[middleColInt],length(datarange))
                
                #now, modify for the data values above and below the center value:
                selabove <- which(datarange > center)
                if(length(selabove)){
                    col[selabove] <- assignColor(datarange = datarange[selabove],
                                                 colscale = colscale[(middleColInt+1):ncolors],
                                                 scalerange = if(!symmetric){range(datarange[selabove])
                                                 }else{ range(c(center, center - scalerange[which.max(abs(scalerange-center))])) })
                }
                selbelow <- which(datarange < center)
                if(length(selbelow)){
                    col[selbelow] <- assignColor(datarange[selbelow],
                                                 if(!symmetric){colscale[1:(middleColInt-1)]}else{rev(colscale[1:(middleColInt-1)])},
                                                 scalerange = if(!symmetric){range(datarange[selbelow])
                                                 }else{ c(scalerange[which.max(abs(scalerange-center))], center) })
                }
                return(col)
            }else{
                
                if(diff(scalerange) == 0){
                    colsel <- rep(colscale[1], length(datarange))
                }else{
                    colsel <- abs((ncolors-1)/(max(scalerange)-min(scalerange)) * (datarange-max(scalerange)) + ncolors)
                    colsel <- round(colsel,0)
                }
            }
            #introducing minor inaccuracyfor low values, should become less relevant with larger ncolors:
            colsel[colsel == 0] <- 1
            return(colscale[colsel])
        }
        else{
            return(rep(colscale[1], length(datarange)))
        }
        
    }
    
    
    
    
    
    colr <- crange(50)
    
    
    colorRampLegend(datarange = Metaboseek:::safelog(testdf$df$mut__foldOverRest),
                    assignColor(datarange = sort(Metaboseek:::safelog(testdf$df$mut__foldOverRest)),
                                colscale = colr,
                                scalerange = range(datarange),
                                center = 0,
                                symmetric = T),
                    #internalValues$colscale,
                    "TEST")
    
    
    colorRampLegend(Metaboseek:::safelog(testdf$df$wt__foldOverRest),
                    assignColor(sort(Metaboseek:::safelog(testdf$df$wt__foldOverRest)),
                                colr,
                                center = 0,
                                symmetric = T),
                    #internalValues$colscale,
                    "TEST")
    
    colorRampLegend(datarange = Metaboseek:::safelog(testdf$df$mut__foldOverRest),
                    assignColor2(datarange = sort(Metaboseek:::safelog(testdf$df$mut__foldOverRest)),
                                colscale = colr,
                                scalerange = range(datarange),
                                center = 0,
                                symmetric = T),
                    #internalValues$colscale,
                    "TEST")
    
    
    colorRampLegend(Metaboseek:::safelog(testdf$df$wt__foldOverRest),
                    assignColor2(sort(Metaboseek:::safelog(testdf$df$wt__foldOverRest)),
                                colr,
                                center = 0,
                                symmetric = T),
                    #internalValues$colscale,
                    "TEST")
    
    
    colorRampLegend(datarange = -50:1000,
                    assignColor2(datarange = -50:1000,
                                 colscale = colr,
                                 scalerange = range(datarange),
                                 center = 0,
                                 symmetric = T),
                    #internalValues$colscale,
                    "TEST")
    
    colorRampLegend(datarange = -10:100,
                    assignColor2(datarange = -10:100,
                                 colscale = colr,
                                 scalerange = range(datarange),
                                 center = NULL,
                                 symmetric = F),
                    #internalValues$colscale,
                    "TEST")
    
    colorRampLegend(datarange = -10:100,
                    assignColor2(datarange = -10:100,
                                 colscale = colr,
                                 scalerange = range(datarange),
                                 center = 50,
                                 symmetric = T),
                    #internalValues$colscale,
                    "TEST")
    
    colorRampLegend(datarange = 1:1000,
                    assignColor2(datarange = 1:1000,
                                 colscale = colr,
                                 scalerange = range(datarange),
                                 center = 0,
                                 symmetric = T),
                    "TEST")
    
                    #internalValues$colscale,
    colorRampLegend(datarange = -1000:1000,
                    assignColor2(datarange = -1000:1000,
                                 colscale = colr,
                                 center = 0,
                                 symmetric = T),
                    #internalValues$colscale,
                    "TEST")
    
    
    colorRampLegend(datarange = -1000:1000,
                    assignColor2(datarange = -1000:1000,
                                 colscale = colr,
                                 center = 0,
                                 symmetric = T),
                    #internalValues$colscale,
                    "TEST")
    
    colorRampLegend(Metaboseek:::safelog(testdf$df$wt__foldOverRest),
                    assignColor2(sort(Metaboseek:::safelog(testdf$df$wt__foldOverRest)),
                                 colr,
                                 center = 0,
                                 symmetric = T),
                    #internalValues$colscale,
                    "TEST")
    
    Metaboseek:::safelog(testdf$df$wt__foldOverRest)[which.min(abs(Metaboseek:::safelog(testdf$df$wt__foldOverRest)))]
    
    assignColor(sort(Metaboseek:::safelog(testdf$df$wt__foldOverRest)),
                colr,
                center = 0,
                symmetric = T)[which.min(abs(Metaboseek:::safelog(testdf$df$wt__foldOverRest)))]
    
    colr[100]
    
    
    
    
    colorRampLegend(safelog(vertex_attr(internalValues$activelayout$graph,input$vlabelcol)),
                    assignColor(sort(safelog(vertex_attr(internalValues$activelayout$graph,input$vlabelcol))),
                                internalValues$colscale,
                                center = 0,
                                symmetric = T),
                    #internalValues$colscale,
                    input$vlabelcol)
    
    })