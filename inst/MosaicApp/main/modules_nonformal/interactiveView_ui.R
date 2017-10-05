fluidPage(
  fluidRow(
    column(2,
   htmlOutput('interactiveRTcorr')
           ),
    column(2,
                 htmlOutput('EicTic')
    ),
    column(3,
      htmlOutput('selEICs')
    ),
    column(3,
    htmlOutput('selMZ')

    )),
    
  fluidRow(
    
    p("Brush and double click to zoom in, double click to zoom out, press SHIFT and click to select a data point to view Spectrum. SHIFT + click in Spectrum generates EIC for selected peak."),
  # EICinteractiveUI("chrom1"),
  plotOutput("plainplot",
             click = "plainplot_click",
             hover = hoverOpts(id = "plainplot_hover",
                               delay = 150),
             dblclick = "plainplot_dblclick",
             brush = brushOpts(
               id = "plainplot_brush",
               #direction = "x",
               resetOnNew = TRUE)
  )
  ),
  
  fluidRow(
  plotOutput("Mspec",
             click = "Mspec_click",
             hover = hoverOpts(id = "Mspec_hover",
                               delay = 150),
             dblclick = "Mspec_dblclick",
             brush = brushOpts(
               id = "Mspec_brush",
               #direction = "x",
               resetOnNew = TRUE),
             height = "550px"
  )
  )
  
  
)