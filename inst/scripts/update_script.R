
tryCatch({
  message("Please wait while Metaboseek is being updated")
  Sys.sleep(5)
  oldver <- packageVersion("Metaboseek")[1]  
  
  if(!file.exists(file.path(system.file("config", package = "Metaboseek"), "MseekOptions.json"))){
    oldSettings <- NULL
    
  }else{
    oldSettings <- jsonlite::unserializeJSON(readChar(system.file("config", "MseekOptions.json", package = "Metaboseek"), file.info(system.file("config", "MseekOptions.json", package = "Metaboseek"))$size))
  }
  

  tryCatch({
devtools::install_github("mjhelf/Metaboseek", ref = commandArgs(trailingOnly=TRUE)[1], quiet = F, dependencies = FALSE)
},
error = function(e){
  message("Dependncies have to be installed...")
  devtools::install_github("mjhelf/Metaboseek", ref = commandArgs(trailingOnly=TRUE)[1], quiet = F)
  
   
})

  message("Mseek version before update: ", oldver)
  message("Mseek version after update: ", packageVersion("Metaboseek")[1])
  message(paste0("Update complete! (from ", commandArgs(trailingOnly=TRUE)[1], " branch on GitHub: https://github.com/mjhelf/Metaboseek)"))
  message("You can close this window now and restart Metaboseek.")
  
  do.call(Metaboseek::MseekOptions, oldSettings)
  
  Sys.sleep(10000)
},
error = function(e){
  
  tryCatch({
   source("http://metaboseek.com/files/install_Metaboseek.R")
    },
    error = function(e){
  message("Installation failed. Try to install from the R console using the command:")
  message(paste0('devtools::install_github("mjhelf/Metaboseek", ref ="',commandArgs(trailingOnly=TRUE)[1],'")'))
  message("You can close this window now.")
  Sys.sleep(10000)
})
  })