fluidPage(
    fluidRow(
    #NormalizeModuleUI("intensityNormalizer")
    actionButton('normbutton',"Normalize data"),
    htmlOutput('selctrl'),
    htmlOutput('usenormdata'),
    htmlOutput('selAna'),
    p("if feature clustering with a method based on k-medoids (clara) is selected, define number of clusters in which to group features based on their intensities across samples. Defaults to #of defined groups plus 1."),
    htmlOutput('kclusternum'),
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