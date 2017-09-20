tabBox(title = "EIC viewer",
       id = "EICplots", width = 12, side = "right", selected = "Load MS data",
       
       tabPanel("_"),
       tabPanel("Load MS data",
                source(file.path("modules_nonformal", "loadMSdata_ui.R"), local = TRUE)$value),
       tabPanel("Grouped EICs",
                fluidPage(
  fluidRow(
    #tags$head(
     # tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
    #),
downloadButton("pdfButton", "Save Plot"),
htmlOutput("groupingActiveSelect")
),


fluidRow(
htmlOutput("mainPlotEICs")
#plotOutput("mainPlotEICsPre")
)
)
)
)