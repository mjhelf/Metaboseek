.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("\nWelcome to MOSAiC version",
          packageVersion("Mosaic"),
          "\nVisit our websites: http://mosaic.bti.cornell.edu and https://github.com/mjhelf/Mosaic"))
}