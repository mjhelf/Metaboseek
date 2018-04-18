

fluidPage(   
  source(file.path("ui_main.R"), local = TRUE)$value
  
  ,
  source(file.path("modules_nonformal", "diagnostics_ui.R"), local = TRUE)$value
  ,
  fluidRow(
    plotOutput("tester",
               click = "tester_click",
               dblclick = "tester_dblclick")
  )
)    