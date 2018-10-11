fluidPage(
  fluidRow(
    source(file.path("modules_nonformal", "mainPlots_options_ui.R"), local = TRUE)$value
  ),
  fluidRow(
    WelcomePageModuleUI("welcome")
  ),
  fluidRow(
    source(file.path("modules_nonformal", "mainPlots_ui.R"), local = TRUE)$value
  ),
  fluidRow(
    source(file.path("modules_nonformal", "mainTable_ui.R"), local = TRUE)$value
  ),
  
  fluidRow(
    tabBox(title = "Filter and Sort",
           id = "PlotOpts", width = 12, side = "right", selected = "Filter and Sort",
           
           tabPanel("_"),
           tabPanel("PCA Viewer",
                    PcaViewModuleUI("pcaviewfeatures")
           ),
           tabPanel("Filter and Sort",
                    fluidPage(
                      fluidRow(
                        column(6,
                               columnSelModuleUI('ColSelMod')
                        ),
                        column(6,
                               source(file.path("modules_nonformal", "mainSort_ui.R"), local = TRUE)$value
                        )
                      )
                    )
           )
    )
  )
)