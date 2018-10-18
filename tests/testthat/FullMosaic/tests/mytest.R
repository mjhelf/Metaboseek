app <- ShinyDriver$new("../", loadTimeout = 120000)
app$snapshotInit("mytest")
Sys.sleep(5)

app$snapshot()
app$setInputs(`Mosaic-MosaicSB` = "exploredata")
Sys.sleep(5)

app$snapshot()
app$setInputs(`Mosaic-MosaicSB` = "XCMSrunpanel")
Sys.sleep(5)

app$snapshot()
app$setInputs(`Mosaic-MosaicSB` = "updateTab")
Sys.sleep(5)

app$snapshot()
