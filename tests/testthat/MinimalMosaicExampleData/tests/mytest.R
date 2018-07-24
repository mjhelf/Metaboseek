app <- ShinyDriver$new("../", loadTimeout = 120000)
app$snapshotInit("mytest")
Sys.sleep(10)

app$snapshot()
