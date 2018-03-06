
tryCatch({
  message("Mosaic version before update: ", packageVersion("Mosaic")[1])
  devtools::install_github("mjhelf/Mosaic", ref = commandArgs(trailingOnly=TRUE)[1], quiet = F)
  message("Mosaic version after update: ", packageVersion("Mosaic")[1])
},
error = function(e){
  print("Installation failed")
})

