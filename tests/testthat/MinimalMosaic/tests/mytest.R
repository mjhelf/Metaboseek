app <- ShinyDriver$new("../", loadTimeout = 120000)
app$snapshotInit("mytest")

app$snapshot()
