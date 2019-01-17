app <- ShinyDriver$new("../", loadTimeout = 120000)
app$snapshotInit("mytest")
Sys.sleep(5)

app$snapshot()
app$setInputs(`Mseek-MseekSB` = "exploredata")
Sys.sleep(5)

app$snapshot()
app$setInputs(`Mseek-MseekSB` = "XCMSrunpanel")
Sys.sleep(5)

app$snapshot()
app$setInputs(`Mseek-MseekSB` = "updateTab")
Sys.sleep(5)

app$snapshot()
