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
  if(gsub("/Mosaic","",system.file(package = "Mosaic")) %in% .libPaths()
     && !file.exists(system.file("data", "tables", "filegroups_ms1.csv", package = "Mosaic"))){
    rawgroups <- read.csv(system.file("data", "tables", "filegroups_base.csv", package = "Mosaic"), stringsAsFactors = F, row.names = 1)
    rawgroups$File <- file.path(system.file("data", package = "Mosaic"), rawgroups$File)
    write.csv(rawgroups, system.file("data", "tables", "filegroups.csv", package = "Mosaic"))
    write.csv(rawgroups[rawgroups$Group != "MS2",], file.path(system.file("data", "tables", package = "Mosaic"), "filegroups_ms1.csv"))
    write.csv(rawgroups[rawgroups$Group == "MS2",], file.path(system.file("data", "tables", package = "Mosaic"), "filegroups_ms2.csv"))
  }
  
  
}