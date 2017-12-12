tabBox(title = "EIC viewer",
       id = "EICplots", width = 12, side = "right", selected = "Grouped EICs",
       
       tabPanel("_"),
       tabPanel("Regroup MS data",
                source(file.path("modules_nonformal", "loadMSdata_ui.R"), local = TRUE)$value),
       tabPanel("Grouped EICs",
                fluidPage(
  fluidRow(
    #tags$head(
     # tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
    #),
    
    
column(2,
    checkboxInput("RtCorrActive", "RT correction", value = F)
    ),
    
    
    column(2,
downloadButton("pdfButton", "Save Plot")
),

column(3,
htmlOutput("groupingActiveSelect")
),



column(3,
       
fileInput('rfileload',"Upload ZIP file with mzXML files", accept = "application/zip")
),

column(2,
       
       tags$div(title = "Load MS data directly from a local folder.",
                shinyDirButton('loadRawFolder', label="Load folder", title='Please select a folder')),
       
       tags$div(title = "Load MS data directly from a local folder.",
                actionButton('loadRawFolderOffline', label="Load folder"))
       
       
       )
),


fluidRow(
  #imageOutput("mainPlotPlaceholder"),
htmlOutput("mainPlotEICs")),
#plotOutput("mainPlotEICsPre")
fluidRow(
  plotOutput("adductLegend", height = "30px")
)

)
),
tabPanel("Interactive View (beta)",
         source(file.path("modules_nonformal", "interactiveView_ui.R"), local = TRUE)$value
         )

)