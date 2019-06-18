.onAttach <- function(libname, pkgname) {
  #utils::data("isotopes", package = "enviPat", envir = as.environment("package:METABOseek"))
  
 packageStartupMessage(
    paste("\nWelcome to METABOseek version",
          utils::packageVersion("METABOseek"),
          "\nVisit our websites: http://metaboseek.com and https://github.com/mjhelf/METABOseek\n"))
  
  
}

.onLoad <- function(libname, pkgname) {
  
  options(scipen = 5)

  MseekOptions()
  
    #Update example ms file locations
  if(dirname(system.file(package = "METABOseek")) %in% .libPaths()
     && !file.exists(system.file("extdata", "examples", "example projectfolder", "filegroups.csv", package = "METABOseek"))){
    rawgroups <- read.csv(system.file("extdata", "examples", "example projectfolder", "filegroups_base.csv", package = "METABOseek"), stringsAsFactors = F, row.names = 1)
    rawgroups$File <- file.path(system.file("extdata", "examples", package = "METABOseek"), rawgroups$File)
    write.csv(rawgroups, file.path(system.file("extdata", "examples", "example projectfolder", package = "METABOseek"), "filegroups.csv"))
    #write.csv(rawgroups[rawgroups$Group != "MS2",], file.path(system.file("extdata", "examples", "example projectfolder", package = "METABOseek"), "filegroups_ms1.csv"))
   # write.csv(rawgroups[rawgroups$Group == "MS2",], file.path(system.file("extdata", "examples", "example projectfolder", package = "METABOseek"), "filegroups_ms2.csv"))
  }
  
  
}