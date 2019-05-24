
context("Run Mseek")


#recordTest("./tests/testthat/FullMseek", loadTimeout = 120000)

# inp <- readLines("./tests/testthat/FullMseek/tests/mytest-expected/001.json")
# inp2 <- gsub(.MseekOptions$recentProjects[1], "<RECENT_PROJECT>", inp)
# writeLines(inp2,"./tests/testthat/FullMseek/testresult.json")


#all.equal(inp2, readLines("./tests/testthat/FullMseek/expected_testresult.json"))

#expect_mapequal(tres$input, jsonlite::fromJSON("./tests/testthat/FullMseek/expected_testresult.json")$input)

#expect_setequal(inp2, readLines("./tests/testthat/FullMseek/expected_testresult.json"))
#testApp("./tests/testthat/FullMseek", "mytest.R", compareImages = FALSE, quiet = F)


test_that("Mseek app starts up", {
  expect_pass(testApp("FullMseek", "mytest.R", compareImages = FALSE, quiet = T))
})


# 
# test_that("Mseek app starts up", {
#     expect_success({
#         
#     testApp("FullMseek", "mytest.R", compareImages = FALSE, quiet = T, interactive = F)
#     
#         # file.copy("FullMseek/tests/mytest-expected/001.json",
#         #           "C:/Workspace/troubleshooting/001.json")
#         
#     inp <- readLines("FullMseek/tests/mytest-expected/001.json")
#     inp2 <- gsub(.MseekOptions$recentProjects[1], "<RECENT_PROJECT>", inp)
#     writeLines(inp2,"FullMseek/tests/mytest-expected/001.json")
#     
#     expect_setequal(readLines("FullMseek/expected_testresult.json"),inp2)
#     
#     })
# })
# 
# test_that("Mseek app starts up (stricter check on output)", {
#   expect_success({
#   expected <- jsonlite::fromJSON("FullMseek/expected_testresult.json")
#   thisrun <- jsonlite::fromJSON("FullMseek/tests/mytest-expected/001.json")
#   
#   expect_mapequal(expected$input, thisrun$input)
#   expect_mapequal(expected$output, thisrun$output)
#   })
# })

#' NOTES:
#' in "MinimalMseekExampleData", 1714942.5 is rounded down to 1714942 in Linux, but rounded up to 1714943 in Windows.
#' Expected example output is the linux value
#' 
#' 