fluidPage(
  textInput('xcms_folder', "mzXML file folder"),
  actionButton('xcms_loadfolder', "load files"),
  
  textInput('xcms_name', "Title of this analysis", "xcms_run"),
  
  fileInput('xcms_settingsLoad',"Load settings", accept = "application/zip"),
  
  #htmlOutput('xcms_selectTab'),
  selectizeInput('xcms_selectTab',"Change settings for...", choices = list("File Grouping" = "filegroups",
                                                                           "Peak Detection" = "centWave",
                                                                           "Peak filling" = "peakfilling",
                                                                           "Feature grouping" = "group",
                                                                           "CAMERA settings" = "camera",
                                                                           "RT correction" = "retcor",
                                                                           "Output Files" = "outputs")
  ),
  
  rHandsontableOutput('xcms_settingstab'),
  
  htmlOutput("xcms_legend_master"),
  
  downloadButton("xcms_settingsDL", "Download settings"),
  
  actionButton('xcms_start',"Start analysis!"),
  
  rHandsontableOutput('xcms_statustab'),
  
  
  # Output: Verbatim text for data summary ----
  verbatimTextOutput("summary")
  
  
)