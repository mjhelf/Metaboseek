function(request){
  MosaicMinimalUI(skin = "black",
  htmlOutput("allUI"),
  diagnostics = .MosaicOptions$develMode,
  dashboard = T,
  id = NULL)
}