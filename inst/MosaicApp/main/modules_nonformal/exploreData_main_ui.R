fluidPage(
    fluidRow(
        source(file.path("modules_nonformal", "mainPlots_options_ui.R"), local = TRUE)$value
        ),
    fluidRow(
        source(file.path("modules_nonformal", "mainPlots_ui.R"), local = TRUE)$value
    ),
    fluidRow(
    source(file.path("modules_nonformal", "mainTable_ui.R"), local = TRUE)$value
          ),
    
    fluidRow(
      box(title = "Filter and Sort", status = "danger", collapsible = T, width = 12,
          fluidRow(
        column(6,
    htmlOutput('selnormdata'),
    htmlOutput('mainSelGroup'),
    htmlOutput('mainSelgProps'),
    htmlOutput('mainSelsProps'),
    htmlOutput('mainSelIntensities'),
    htmlOutput('mainSelOthers')
        ),
    column(6,
           source(file.path("modules_nonformal", "mainSort_ui.R"), local = TRUE)$value
           )
    )
      )
    )
)