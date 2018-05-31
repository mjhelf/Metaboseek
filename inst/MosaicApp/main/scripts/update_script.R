
tryCatch({
  print("Please wait while MOSAIC is being updated")
  Sys.sleep(10)
  message("Mosaic version before update: ", packageVersion("Mosaic")[1])
  devtools::install_github("mjhelf/Mosaic", ref = commandArgs(trailingOnly=TRUE)[1], quiet = F)
  message("Mosaic version after update: ", packageVersion("Mosaic")[1])
  print("update complete")
  Sys.sleep(10)
},
error = function(e){
  print("Installation failed")
})

