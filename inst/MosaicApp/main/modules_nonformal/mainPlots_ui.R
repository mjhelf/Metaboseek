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
       checkboxInput("ShowSpec", "Show spectrum", value = F)
),
    
    column(2,
downloadButton("pdfButton", "Save Plot")
),

column(2,
htmlOutput("groupingActiveSelect")
),



column(2,
       
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
  htmlOutput("adductPlot")
),
fluidRow(
  SpecmoduleUI("Spec1")
)
)
),
tabPanel("Interactive View (beta)",
        # p("temp")
         source(file.path("modules_nonformal", "interactiveView_ui.R"), local = TRUE)$value
         ),
tabPanel("Quickplots",
         source(file.path("modules_nonformal", "quickPlots_ui.R"), local = TRUE)$value
)

)