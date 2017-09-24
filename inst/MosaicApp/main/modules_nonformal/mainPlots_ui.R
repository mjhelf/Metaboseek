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
    column(4,
downloadButton("pdfButton", "Save Plot")
),

column(4,
htmlOutput("groupingActiveSelect")
),

column(4,
fileInput('rfileload',"Upload ZIP file with mzXML files", accept = "application/zip")
)
),


fluidRow(
  #imageOutput("mainPlotPlaceholder"),
htmlOutput("mainPlotEICs")
#plotOutput("mainPlotEICsPre")
)
)
)
)