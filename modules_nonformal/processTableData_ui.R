fluidPage(
    fluidRow(
    #NormalizeModuleUI("intensityNormalizer")
    actionButton('normbutton',"Normalize data"),
    htmlOutput('selctrl'),
    htmlOutput('usenormdata'),
    htmlOutput('selAna'),
    actionButton('analyzebutton',"Analyze data")
    
    ),
    fluidRow(
        column(6,
    densplotModuleUI("nonNormalizedPlot")
        ),
        column(6,
    densplotModuleUI("NormalizedPlot")
        )
    )
    
)