fluidPage(
  fluidRow(
  box(title = "Run XCMS analysis", width = 12, status= "danger",
  
      p("This module runs and observes an XCMS analysis with customizable settings and generates a new folder inside the mzXML file folder with results from the xcms analysis."),

  p(strong("Not on by default in Server mode!")," Currently only one xcms job per MOSAiC session (concurrent job monitoring coming later)."),
fluidRow(
  column(6,
  actionButton('xcms_loadfolderOffline', "load MS file folder"),
  shinyDirButton('xcms_loadfolder', "load MS file folder", title = "select a folder with MS data files"),
  
  
  textInput('xcms_name', "Title of this analysis", "xcms_run"),
  actionButton('xcms_start',"Start analysis!", style="color: #fff; background-color: #C41230; border-color: #595959")),
  
  column(6,
  fileInput('xcms_settingsLoad',"Load settings", accept = "application/zip"),
  
  downloadButton("xcms_settingsDL", "Download settings")
  ))
  
  )),
  
  fluidRow(
    tabBox(title = "XCMS Settings",
           id = "xcms_settingsBox",
           width = 12, side = "right", selected = "XCMS Settings",
           
           tabPanel("_"),
           
           
           
           tabPanel("XCMS Settings",
  
  
  htmlOutput('xcms_selectTab'),
  
  
  rHandsontableOutput('xcms_settingstab'),
  
  htmlOutput("xcms_legend_master")
  
  
  ))),
  fluidRow(
    box(title = "Job status", width = 12, status= "danger",
        p("View status of a running XCMS job here"),
  rHandsontableOutput('xcms_statustab')
    ))
    
  
  
  # Output: Verbatim text for data summary ----
  #verbatimTextOutput("summary")
  
  
)