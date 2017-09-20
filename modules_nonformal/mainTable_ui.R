
    box(title = "Feature table", status = "primary", collapsible = T, width = 12,
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
),
#note: hidden needs to be outside of renderUI (probably reactivity issue)
shinyjs::hidden(
htmlOutput("tableSaver")
)
)
)
