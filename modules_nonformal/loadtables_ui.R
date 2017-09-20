fluidPage(
    useShinyjs(),
    fluidRow(
        column(4,
               h3("Upload a feature Table"),
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"'),
    div(style="display:inline-block",actionButton("ldtbl","Preview Table"),actionButton("confgroups","Confirm & Load Data"))
    ),
    column(8,
           checkboxInput('anagroupswitch', 'Define Sample Groups', TRUE),
           #h3("Define sample groups"),

              fileInput('loadgroups', "Upload a sample grouping table", accept= NULL),
              rHandsontableOutput('anagrouping'),
    downloadButton("savegroups","Save Grouping")
       )
    ),
    fluidRow(
        conditionalPanel(condition = "input.ldtbl > 0",
    tags$hr(),
    h3("Table Preview"),
    div(title = "The selected columns should contain all intensity columns of interest!", actionButton("intcols","Select Columns")),
    rHandsontableOutput('preview'))
    )
)