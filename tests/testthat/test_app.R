
context("Run Mseek")


#recordTest("./tests/testthat/FullMseek", loadTimeout = 120000)

# inp <- readLines("./tests/testthat/FullMseek/tests/mytest-expected/001.json")
# inp2 <- gsub(.MseekOptions$recentProjects[1], "<RECENT_PROJECT>", inp)
# writeLines(inp2,"./tests/testthat/FullMseek/testresult.json")


#testApp("./tests/testthat/FullMseek", "mytest.R", compareImages = FALSE, quiet = F)

# test_that("Mseek app starts up", {
#   expect_pass(testApp("FullMseek", "mytest.R", compareImages = FALSE, quiet = T))
# 
# })


test_that("Mseek app starts up", {
    
    testApp("FullMseek", "mytest.R", compareImages = FALSE, quiet = T, interactive = F)
    
    inp <- readLines("FullMseek/tests/mytest-expected/001.json")
    inp2 <- gsub(.MseekOptions$recentProjects[1], "<RECENT_PROJECT>", inp)
   expect_equal(readLines("FullMseek/testresult.json"),inp2)
})

#' NOTES:
#' in "MinimalMseekExampleData", 1714942.5 is rounded down to 1714942 in Linux, but rounded up to 1714943 in Windows.
#' Expected example output is the linux value
#' 
#' 