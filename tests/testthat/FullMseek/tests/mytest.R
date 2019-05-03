app <- ShinyDriver$new("../", loadTimeout = 120000)
Sys.sleep(5)
app$snapshotInit("mytest")

app$setInputs(`Mseek-MseekSB` = "exploredata")
Sys.sleep(10)
app$snapshot()
