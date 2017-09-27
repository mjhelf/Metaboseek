fluidPage(
  h3("Run XCMS analysis"),
  p("This module runs and observes an XCMS analysis with customizable settings and generates a new folder inside the mzXML file folder with results from the xcms analysis."),

  p(strong("Not available in Server mode!")," Currently only one xcms job per MOSAiC session (concurrent job monitoring coming later)."),
  textInput('xcms_folder', "mzXML file folder"),
  actionButton('xcms_loadfolder', "load files"),
  
  textInput('xcms_name', "Title of this analysis", "xcms_run"),
  
  fileInput('xcms_settingsLoad',"Load settings", accept = "application/zip"),
  
  htmlOutput('xcms_selectTab'),
  
  
  rHandsontableOutput('xcms_settingstab'),
  
  htmlOutput("xcms_legend_master"),
  
  downloadButton("xcms_settingsDL", "Download settings"),
  
  actionButton('xcms_start',"Start analysis!"),
  
  rHandsontableOutput('xcms_statustab'),
  
  
  # Output: Verbatim text for data summary ----
  verbatimTextOutput("summary")
  
  
)