.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("\nWelcome to MOSAiC version",
          utils::packageVersion("Mosaic"),
          "\nVisit our websites: http://mosaic.bti.cornell.edu and https://github.com/mjhelf/Mosaic\n"))
  
  
}

.onLoad <- function(libname, pkgname) {
  
  options(scipen = 5)

  MosaicOptions()
  
  #Update example ms file locations
  if(dirname(system.file(package = "Mosaic")) %in% .libPaths()
     && !file.exists(system.file("extdata", "examples", "example projectfolder", "filegroups.csv", package = "Mosaic"))){
    rawgroups <- read.csv(system.file("extdata", "examples", "example projectfolder", "filegroups_base.csv", package = "Mosaic"), stringsAsFactors = F, row.names = 1)
    rawgroups$File <- file.path(system.file("extdata", "examples", package = "Mosaic"), rawgroups$File)
    write.csv(rawgroups, file.path(system.file("extdata", "examples", "example projectfolder", package = "Mosaic"), "filegroups.csv"))
    #write.csv(rawgroups[rawgroups$Group != "MS2",], file.path(system.file("extdata", "examples", "example projectfolder", package = "Mosaic"), "filegroups_ms1.csv"))
   # write.csv(rawgroups[rawgroups$Group == "MS2",], file.path(system.file("extdata", "examples", "example projectfolder", package = "Mosaic"), "filegroups_ms2.csv"))
  }
  
  
}