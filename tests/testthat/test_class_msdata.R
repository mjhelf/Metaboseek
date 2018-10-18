context("Class MSData")

#A simple list from files in the trimmed example
mutfile <-  list.files(system.file("extdata", "examples", "ms1", "mut", package = "Mosaic"), full.names = T, pattern = ".mzXML")

wtfile <-  list.files(system.file("extdata", "examples", "ms1", "wt", package = "Mosaic"), full.names = T, pattern = ".mzXML")

rgt <- data.frame(File = c(mutfile,wtfile),
                  Group = c(rep("mut",length(mutfile)), rep("wt",length(wtfile))),
                  stringsAsFactors = F)
xt <- constructRawLayout(rgt)
y <- loadRawM(filelist = xt$filelist, MSn = T, workers =1)

#Tests for the constructRawLayout function#

test_that("rawgrouptable in the constructRawLayout function works",{
  expect_equal(xt$rawgrouptable[[1]][1], system.file("extdata", "examples", "ms1", "mut","AA03.mzXML", package = "Mosaic"))})

test_that("groups in rawgrouptable in the constructRawLayout function are correct",{
  expect_equal(names(xt$grouping), c("mut", "wt"))})

test_that("the class of rawgrouptable is rawLayout",{
          expect_equal(class(xt), "rawLayout")})

test_that("rtw in settings of rawgrouptable are correct",{
  expect_equivalent(xt$settings$rtw,30)
})

test_that("ppm in settings of rawgrouptable are correct",{
  expect_equivalent(xt$settings$ppm,5)
})

test_that("cols in settings of rawgrouptable are correct",{
  expect_equivalent(xt$settings$cols,1)
})

test_that("colr in settings of rawgrouptable are correct",{
  expect_equivalent(xt$settings$colr,"mosaic.colors")
})

test_that("alpha in settings of rawgrouptable are correct",{
  expect_equivalent(xt$settings$alpha, 0.8)
})

#Several Tests for the loadRawM function#
test_that("loadRawMfunction works",{

  expect_equal(length(y[[1]]@tic), 570)
  expect_equal(y[[1]]@scantime[1], 200.267)
  expect_equal(y[[1]]@scanindex[1], 0)
})


#Tests for rawGrouping fxn#
###help###



#Tests for MultiEIC#
#!what is pcame_mini??? cannot execute fxn without this. doesn't seem to be in other scripts
###help###


#Tests for rawEicM, y[[1]] is an xcmsRaw object
#is this correct? should all intensities be 0?
test_that("rawEICm function works",{
  u <- Mosaic::rawEICm(y[[1]])
  expect_equal(class(u), "list")
  expect_equal(length(u$scan),570)
  expect_equal(u$intensity[10], 0)
})

#Tests for getgauss
#not sure what cor is equal to...
test_that("getgauss function works",{
  g <- getgauss(y = c(1:10,10:1),1) 
  expect_equivalent(g, 0.9959037, tolerance = 0.000001)
})

#Tests for bestgauss
###help, object 'pcame_mini was not found

#Tests for exIntensities
##help, tb is required as an object(only appears once in the script)

#remove default values for pcame_mini (in bestgauss), and tb (in exIntensities)