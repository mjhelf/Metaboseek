
tryCatch({
  message("Please wait while MOSAiC is being updated")
  Sys.sleep(5)
  devtools::install_github("mjhelf/Mosaic", ref = commandArgs(trailingOnly=TRUE)[1], quiet = F)
  message("Mosaic version before update: ", packageVersion("Mosaic")[1])
  message("Mosaic version after update: ", packageVersion("Mosaic")[1])
  message(paste0("Update complete! (from ", commandArgs(trailingOnly=TRUE)[1], " branch on GitHub: https://github.com/mjhelf/Mosaic)"))
  message("You can close this window now and restart MOSAiC.")
  Sys.sleep(10000)
},
error = function(e){
  warning("Installation failed. Try to install from the R console using the command:")
  message(paste0('devtools::install_github("mjhelf/Mosaic", ref ="',commandArgs(trailingOnly=TRUE)[1],'")'))
  message("You can close this window now.")
  Sys.sleep(10000)
})