context("xcms integration and object histories")

test_that("peak areas are calculated correctly",{
    
    expect_equal(.peakArea(c(1,2),
                           c(1,1)),
                 1)
    
    
    expect_equal(.peakArea(c(1,2),
                           c(0,1)),
                 0.5)
    
    expect_equal(.peakArea(c(1,2),
                           c(1,0)),
                 0.5)
    
    expect_equal(.peakArea(c(1,2),
                           c(1,2)),
                 1.5)
    
    expect_equal(.peakArea(c(1,2),
                           c(2,1)),
                 1.5)
    
    expect_equal(.peakArea(c(1,2,3),
                           c(1,1,1)),
                 2)
    
    expect_equal(.peakArea(c(1,2,3),
                           c(0,1,1)),
                 1.5)
    expect_equal(.peakArea(c(1,2,3),
                           c(1,0,1)),
                 1.0)
    expect_equal(.peakArea(c(1,2,3),
                           c(1,1,0)),
                 1.5)
    
    expect_equal(.peakArea(c(1,2,3),
                           c(0,1,0)),
                 1.0)
    
    expect_equal(.peakArea(c(1,3,4),
                           c(0,1,0)),
                 1.5)

    expect_equal(.peakArea(c(1,3,4,6),
                           c(0,1,0,1)),
                 2.5)
    
})
