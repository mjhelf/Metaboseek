
tryCatch({
  print("Please wait while MOSAIC is being updated")
  Sys.sleep(5)
  message("Mosaic version before update: ", packageVersion("Mosaic")[1])
  devtools::install_github("mjhelf/Mosaic", ref = commandArgs(trailingOnly=TRUE)[1], quiet = F)
  message("Mosaic version after update: ", packageVersion("Mosaic")[1])
  print("update complete!")
  print("You can close this window now.")
  Sys.sleep(Inf)
},
error = function(e){
  print("Installation failed. Try to install from the R console using the command:")
  print(paste0('devtools::install_github("mjhelf/Mosaic", ref ="',commandArgs(trailingOnly=TRUE)[1],'")'))
  print("You can close this window now.")
  Sys.sleep(Inf)
})