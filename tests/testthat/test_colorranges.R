context("Plotting functions")


test_that("peptide annotation and plotting works",{
    
    
    expect_equal(assignColor(1:5,1:5, center = NULL),
                 1:5)
    
    expect_equal(assignColor(1:5,1:10, center = NULL),
                 c(1,3,6,8,10))
    
    expect_equal(assignColor(1:10,1:5, center = NULL),
                 c(1,1,2,2,3,3,4,4,5,5))
    
    expect_equal(assignColor(1:10,1:5, center = NULL),
                 c(1,1,2,2,3,3,4,4,5,5))
    
    expect_equal(assignColor(1:11,1:5, center = 6),
                 c(1,1,2,2,3,3,3,4,4,5,5))
    
    expect_equal(assignColor(1:11,1:5, center = 8),
                 c(1,1,2,2,2,2,3,3,3,4,4))
    
    expect_equal(assignColor(c(1,2,3,NA,5),
                             1:5,
                             center = NULL,
                             NAcolor = 0),
                 c(1,2,3,0,5))
    
    
    MseekExamplePreload(data=F)
    testdf <- Metaboseek:::analyzeTable(tab2$df, tab2$intensities,
                                       tab2$anagroupnames,
                                       analyze = c("Basic analysis"), 
                                       normalize = T,
                                       useNormalized = T,
                                       logNormalized = F,
                                       MSData = NULL,
                                       ppm = 5)
    
    crange <- colorRampPalette(c("blue", "gray", "red"))
    colr <- crange(200)
    
    
    
    colorRampLegend(datarange = Metaboseek:::safelog(testdf$df$mut__foldOverRest),
                    assignColor(datarange = seq(min(Metaboseek:::safelog(testdf$df$mut__foldOverRest)),
                                                max(Metaboseek:::safelog(testdf$df$mut__foldOverRest)),
                                                length.out = 200),
                                colscale = colr,
                                center = 0),
                    #internalValues$colscale,
                    "TEST")
    
    
    
    colorRampLegend(Metaboseek:::safelog(testdf$df$wt__foldOverRest),
                    assignColor(seq(min(Metaboseek:::safelog(testdf$df$wt__foldOverRest)),
                                    max(Metaboseek:::safelog(testdf$df$wt__foldOverRest)),
                                    length.out = 200),
                                colr,
                                center = 0),
                    #internalValues$colscale,
                    "TEST")
    
    
    colorRampLegend(datarange = -5000:100000,
                    assignColor(datarange = -5000:100000,
                                 colscale = colr,
                                 center = 0),
                    "TEST")
    
    colorRampLegend(datarange = -10:100,
                    assignColor(datarange = -10:100,
                                 colscale = colr,
                                 center = NULL),
                    "TEST")
    
    colorRampLegend(datarange = -10:100,
                    assignColor(datarange = -10:100,
                                colscale = colr,
                                manualRange = -1000:1000,
                                center = NULL),
                    "TEST")
    
    colorRampLegend(datarange = -10:100,
                    assignColor(datarange = -10:100,
                                 colscale = colr,
                                 center = 90),
                    "TEST")
    
    colorRampLegend(datarange = 1:1000,
                    assignColor(datarange = 1:1000,
                                 colscale = colr,
                                 center = 0),
                    "TEST")
    
    colorRampLegend(datarange = -1000:1000,
                    assignColor(datarange = -1000:1000,
                                 colscale = colr,
                                 center = 0),
                    "TEST")
    
    colorRampLegend(datarange = -1000:1000,
                    assignColor(datarange = -1000:1000,
                                colscale = colr,
                                manualRange = -5000:5000,
                                center = 0),
                    "TEST")
    
    colorRampLegend(datarange = -1000:1000,
                    assignColor(datarange = -1000:1000,
                                colscale = colr,
                                manualRange = -50:50,
                                center = 0),
                    "TEST")
    
    colorRampLegend(datarange = -100:1000,
                    assignColor(datarange = -100:1000,
                                colscale = colr,
                                manualRange = NULL,
                                center = 0),
                    "TEST")
    
    
    colorRampLegend(datarange = -10:10,
                    assignColor(datarange = -10:10,
                                colscale = colr,
                                manualRange = -5:5,
                                center = 0))
    
    colorRampLegend(datarange = -10:10,
                    assignColor(datarange = -10:10,
                                colscale = colr,
                                manualRange = -10:10,
                                center = 0))
    
    })