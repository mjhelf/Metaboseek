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
    
    })