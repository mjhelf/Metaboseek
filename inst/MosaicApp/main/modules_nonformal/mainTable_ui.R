tabBox(title = "Feature table",
       id = "TableBox",
       #status = "primary",
       #collapsible = T,
       width = 12,
       side = "right",
       selected = "View Table",
       
       tabPanel("_"),
       tabPanel("Load Table",
                source(file.path("modules_nonformal", "loadtables_ui.R"), local = TRUE)$value),
       
       tabPanel("Analyze Table",
                source(file.path("modules_nonformal", "processTableData_ui.R"), local = TRUE)$value
                ),
       
       tabPanel("View Table",
        fluidPage(
            fluidRow(
   
    rHandsontableOutput('maintable')
  
),
           fluidRow(
               column(2,
                      downloadButton("tbButton",label = "Save Table")
                      ),
               column(2,
              actionButton("newTable",label = "Save internally")
              ),
              column(3,
                     htmlOutput("tablePage")
                     ),
              column(5,
                     textOutput("tableInfo")
              )
)),
#note: hidden needs to be outside of renderUI (probably reactivity issue)
shinyjs::hidden(
htmlOutput("tableSaver")
)
)
)
