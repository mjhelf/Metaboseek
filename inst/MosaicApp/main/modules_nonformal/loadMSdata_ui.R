  fluidRow(
    column(4,
    htmlOutput("groupingName"),
    htmlOutput("groupingEditSelect")),
    column(8,
    numericInput("rnamelvl", "Naming scheme", 1, step=1, min = 1),
    rHandsontableOutput('rawgrouping'),
    actionButton("confrgroups","Confirm & Load"),
    downloadButton("savergroups","Save Grouping"),
    fileInput('loadrgroups', NULL,
              accept= NULL
              #placeholder=""
    ))
)