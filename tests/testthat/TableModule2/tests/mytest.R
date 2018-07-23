app <- ShinyDriver$new("../", loadTimeout = 120000)
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(`Testtab-sortBy` = "rt")
app$setInputs(`Testtab-sortCheck` = TRUE)
app$setInputs(`Testtab-decreasingCheck` = FALSE)
app$snapshot()
