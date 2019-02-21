
tryCatch({
  message("Please wait while METABOseek is being updated")
  Sys.sleep(5)
  oldver <- packageVersion("METABOseek")[1]  
  
  if(!file.exists(file.path(system.file("config", package = "METABOseek"), "MseekOptions.json")) || defaults){
    oldSettings <- NULL
    
  }
  else{
    oldSettings <- jsonlite::unserializeJSON(readChar(system.file("config", "MseekOptions.json", package = "METABOseek"), file.info(system.file("config", "MseekOptions.json", package = "METABOseek"))$size))
  }
  

devtools::install_github("mjhelf/METABOseek", ref = commandArgs(trailingOnly=TRUE)[1], quiet = F)
  message("Mseek version before update: ", oldver)
  message("Mseek version after update: ", packageVersion("METABOseek")[1])
  message(paste0("Update complete! (from ", commandArgs(trailingOnly=TRUE)[1], " branch on GitHub: https://github.com/mjhelf/METABOseek)"))
  message("You can close this window now and restart METABOseek.")
  
  do.call(METABOseek::MseekOptions, oldSettings)
  
  Sys.sleep(10000)
},
error = function(e){
  warning("Installation failed. Try to install from the R console using the command:")
  message(paste0('devtools::install_github("mjhelf/METABOseek", ref ="',commandArgs(trailingOnly=TRUE)[1],'")'))
  message("You can close this window now.")
  Sys.sleep(10000)
})