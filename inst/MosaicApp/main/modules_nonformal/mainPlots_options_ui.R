tabBox(title = "Options",
       id = "PlotOpts", width = 12, side = "right", selected = "EIC options",

       tabPanel("_"),
       
       tabPanel("Load Data (NEW)",
                LoadDataModuleUI("maindataload")
       ),
       
       tabPanel("EIC options",
              fluidRow(
             column(3,
       htmlOutput("PPMwindow")
),
column(3,
       htmlOutput("plotCols")
       ),
column(2,
       htmlOutput("TICtoggle")
),
column(2,
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
       ),
fluidRow(
  column(3,
  htmlOutput("plotYzoom")),
  
  column(3,
  htmlOutput("plotLw")),
  
  column(2,
         htmlOutput("MLtoggle")),
  
  column(2,
  htmlOutput("plotCx")),
  
  column(2,
         htmlOutput("colorscheme"))
  
  
  
)

),

tabPanel("Mass shifts",
         fluidRow(
           
           column(9,
                  rHandsontableOutput('massShiftTab'),
                  actionButton('updateshifts', "Update mass shifts"),
                  downloadButton("savemassShiftTab","Save mass shifts")
           ),
           column(3,
                  fileInput('loadmassShift', "Upload a mass shift table", accept= NULL)
           )
           
           )
         ),

tabPanel("RT correction",
         fluidRow(
           
           column(4,
                  fileInput('RtCorrLoad', "Load Retention time correction data",
                            accept= NULL
                            #placeholder=""
                  )
           ),
           
           column(8,
                  plotOutput("rtcorr")
           ))),

tabPanel("Molecular formula prediction",
         MzqueryModuleUI("mz1")
),

# tabPanel("Project options",
#          source(file.path("modules_nonformal", "project_options_ui.R"), local = TRUE)$value
# ),
tabPanel("Global options",
         GlobalOptionsModuleUI("globalopts")
)

)
