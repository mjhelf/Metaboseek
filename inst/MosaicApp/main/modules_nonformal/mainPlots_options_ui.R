tabBox(title = "Plotting options",
       id = "PlotOpts", width = 12, side = "right", selected = "EICs",

       tabPanel("_"),
       tabPanel("EICs",
              fluidRow(
             column(3,
       htmlOutput("PPMwindow")
),
column(3,
       htmlOutput("plotCols")
       ),
column(1,
       htmlOutput("TICtoggle")
),
column(3,
       # htmlOutput("RTwindow")
       HTML('
            <div class="form-group shiny-input-container" style="display:inline-block">
            <label>RT window (sec): </label>
            <input id="RTwindow" type="number" class="form-control" value="30" min="0"/>
            </div>
            ')
       ),
column(2,
       htmlOutput("RTtoggle")
)
       )
)
)