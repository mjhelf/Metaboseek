fluidPage(
fluidRow(
column(6,
       tags$div(title = "Give a name to your current analysis - this tag will be part of all file names generated in this session.", 
                htmlOutput("projectName")
                )
),
column(6,
htmlOutput("rootfol"),
actionButton('changeRoot', "Change parent folder")
)
))