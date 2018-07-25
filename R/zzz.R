.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("\nWelcome to MOSAiC version",
          utils::packageVersion("Mosaic"),
          "\nVisit our websites: http://mosaic.bti.cornell.edu and https://github.com/mjhelf/Mosaic\n"))
  


  
}

.onLoad <- function(libname, pkgname) {
  
  options("MosaicSettings" = list("MosaicDefaultRoot" = if(Sys.info()['sysname'] == "Windows"){getwd()}else{"/"},
                                  "MosaicDevelMode" = FALSE))
}