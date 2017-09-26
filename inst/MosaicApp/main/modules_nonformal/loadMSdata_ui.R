verticalLayout(
    #actionButton('uzi','Unzip'),
  
  #note: rfileload moved to mainPLots_ui.R
   # fileInput('rfileload',"ZIP file", accept = "application/zip"),
    # numericInput('nrgroups', "Number of Groups", value = 1, min = 1),
    # rHandsontableOutput('rgroupnames'),
    htmlOutput("groupingName"),
    htmlOutput("groupingEditSelect"),
    numericInput("rnamelvl", "Naming scheme", 1, step=1, min = 1),
    rHandsontableOutput('rawgrouping'),
    actionButton("confrgroups","Confirm & Load"),
    downloadButton("savergroups","Save Grouping"),
    fileInput('loadrgroups', NULL,
              accept= NULL
              #placeholder=""
    )
)