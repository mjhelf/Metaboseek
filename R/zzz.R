.onAttach <- function(libname, pkgname) {
  #utils::data("isotopes", package = "enviPat", envir = as.environment("package:Metaboseek"))
  
 packageStartupMessage(
    paste("\nWelcome to Metaboseek version",
          utils::packageVersion("Metaboseek"),
          "\nVisit our websites: http://metaboseek.com and https://github.com/mjhelf/Metaboseek\n"))
  
  
}

.onLoad <- function(libname, pkgname) {
  
  options(scipen = 5)

  MseekOptions()
  
    #Update example ms file locations
  if(dirname(system.file(package = "Metaboseek")) %in% .libPaths()
     && !file.exists(system.file("extdata", "examples", "example_projectfolder", "filegroups.csv", package = "Metaboseek"))){
    rawgroups <- read.csv(system.file("extdata", "examples", "example_projectfolder", "filegroups_base.csv", package = "Metaboseek"), stringsAsFactors = F, row.names = 1)
    rawgroups$File <- file.path(system.file("extdata", "examples", package = "Metaboseek"), rawgroups$File)
    write.csv(rawgroups, file.path(system.file("extdata", "examples", "example_projectfolder", package = "Metaboseek"), "filegroups.csv"))
    #write.csv(rawgroups[rawgroups$Group != "MS2",], file.path(system.file("extdata", "examples", "example_projectfolder", package = "Metaboseek"), "filegroups_ms1.csv"))
   # write.csv(rawgroups[rawgroups$Group == "MS2",], file.path(system.file("extdata", "examples", "example_projectfolder", package = "Metaboseek"), "filegroups_ms2.csv"))
  }
  
  
}