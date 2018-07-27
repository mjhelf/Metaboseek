function(request){
  if(!.MosaicOptions$develMode){
  source(file.path("ui_main.R"), local = TRUE)$value
  }else{
  source(file.path("ui_diagnostic.R"), local = TRUE)$value
  }
    
}