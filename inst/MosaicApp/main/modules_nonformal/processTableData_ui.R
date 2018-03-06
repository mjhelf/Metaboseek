fluidPage(
    fluidRow(
    #NormalizeModuleUI("intensityNormalizer")
    
      h3("Prepare data"),
    div(title= "Apply normalization factor based on mean intensities for each column, and replace values of 0 by the lowest non-zero value in each column",actionButton('normbutton',"Normalize data")),
    htmlOutput('selctrl'),
    hr(),
    h3("Step 1: Feature analysis"),
    htmlOutput('usenormdata'),
    htmlOutput('selAna'),
    p("Number of clusters in which to group features based on their intensities across samples by k-medoids (clara). Defaults to #of defined groups plus 1.",
      id = "clarainfo"),
    htmlOutput('kclusternum'),
    actionButton('analyzebutton',"Analyze data",style="color: #fff; background-color: #C41230; border-color: #595959"),
    hr(),
    h3("Step 2: Advanced feature analysis"),
    hr(),
    h3("Step 3: Sample group analysis")
    
    
    )#,
   # fluidRow(
    #    column(6,
    #densplotModuleUI("nonNormalizedPlot")
    #    ),
     #   column(6,
    #densplotModuleUI("NormalizedPlot")
     #   )
    #)
    
)