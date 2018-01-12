fluidPage(
    h3("Upload a feature Table"),
    fluidRow(
        column(3,
               
        checkboxInput('toggleTabOpts', "Show Table Options", value = F),

   
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
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv'))
    ),
    column(6,
           checkboxInput('anagroupswitch', 'Define Sample Groups', TRUE),
           #h3("Define sample groups"),

              fileInput('loadgroups', "Upload a sample grouping table", accept= NULL),
              rHandsontableOutput('anagrouping'),
    downloadButton("savegroups","Save Grouping")
       ),
    column(3,
           actionButton("confgroups","Confirm & Load Data", style="color: #fff; background-color: #C41230; border-color: #595959")
    )),
    fluidRow(
      #shinyjs::hidden(
    tags$hr(id = "loadLine"),
    h3("Table Preview", id = "previewH3"),
    #div(title = "The selected columns should contain all intensity columns of interest!", 
        actionButton("intcols","Select Columns"),
        #),
    rHandsontableOutput('preview')
    #)
    )
)