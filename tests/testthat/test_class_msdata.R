context("Class MSData")

#A simple list from files in the trimmed example
mutfile <-  c(system.file("trimmed", "mut","AA03.mzXML", package = "Mosaic"),
            system.file("trimmed", "mut", "AA05.mzXML", package = "Mosaic"),
            system.file("trimmed", "mut", "AA07.mzXML", package = "Mosaic"))

wtfile <-  c(system.file("trimmed", "wt", "AA10.mzXML", package = "Mosaic"),
             system.file("trimmed", "wt", "AA12.mzXML", package = "Mosaic"),
             system.file("trimmed", "wt", "AA14.mzXML", package = "Mosaic"),
             system.file("trimmed", "wt", "AA16.mzXML", package = "Mosaic"))
z <- list(mutfile, wtfile)
x <- list(mutfile, wtfile)
names(x) = c("MUT", "WT")

#Tests for the constructRawLayout function#

test_that("rawgrouptable in the constructRawLayout function works",{
  xt <- constructRawLayout(x)
  expect_equal(xt$rawgrouptable[[1]][1], system.file("trimmed", "mut","AA03.mzXML", package = "Mosaic"))})

test_that("groups in rawgrouptable in the constructRawLayout function are correct",{
  expect_equal(names(xt$rawgrouptable), c("MUT", "WT"))})

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
y <- print(loadRawM(filelist = xt$rawgrouptable[[1]], MSn = T, workers =1))
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
  g <- getgauss(x = c(1:100),1) 
  expect_equal(substr(g,1,9), "0.9961025")
})

#Tests for bestgauss
###help, object 'pcame_mini was not found

#Tests for exIntensities
##help, tb is required as an object(only appears once in the script)








loadRawM(y)

updateRawLayout(y)

rawGrouping(y)
xt
xt$rawgrouptable
xt$filelist
xt$Grouping
xt$settings$rtw
class(xt)
x$Group
x
##Why are x$filelist and x$group empty lists????
