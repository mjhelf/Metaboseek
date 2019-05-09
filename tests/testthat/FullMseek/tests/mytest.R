app <- ShinyDriver$new("../", loadTimeout = 120000)
Sys.sleep(15)
app$snapshotInit("mytest")

app$setInputs(`Mseek-MseekSB` = "exploredata")
Sys.sleep(15)
app$snapshot()
