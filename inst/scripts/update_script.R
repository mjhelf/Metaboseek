
tryCatch({
  message("Please wait while METABOseek is being updated")
  Sys.sleep(5)
  devtools::install_github("mjhelf/Mseek", ref = commandArgs(trailingOnly=TRUE)[1], quiet = F)
  message("Mseek version before update: ", packageVersion("METABOseek")[1])
  message("Mseek version after update: ", packageVersion("METABOseek")[1])
  message(paste0("Update complete! (from ", commandArgs(trailingOnly=TRUE)[1], " branch on GitHub: https://github.com/mjhelf/Mseek)"))
  message("You can close this window now and restart METABOseek.")
  Sys.sleep(10000)
},
error = function(e){
  warning("Installation failed. Try to install from the R console using the command:")
  message(paste0('devtools::install_github("mjhelf/Mseek", ref ="',commandArgs(trailingOnly=TRUE)[1],'")'))
  message("You can close this window now.")
  Sys.sleep(10000)
})