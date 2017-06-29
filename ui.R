fluidPage(
    
    
    navbarPage("MoSAIC",
               
               
               tabPanel("Setup",
                        
                        ##DETECT KEYBOARD ACTIONS
                        ##key being held down
                        tags$script('
                                    $(document).on("keydown", function (e) {
                                    Shiny.onInputChange("keyd", e.which);
                                    });
                                    $(document).on("keyup", function (e) {
                                    Shiny.onInputChange("keyd", "NO");
                                    });
                                    '),
                        # mainPanel(
                        #   "This is where the data structure will be set up in the future",
                        tabsetPanel(
                            tabPanel(
                                "Feature table",
                                verticalLayout(
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
                                    actionButton("ldtbl","Load Table"),
                                    
                                    tags$hr(),
                                    rHandsontableOutput('preview'),
                                    numericInput('rmcols',"start column", value = 1, min = 1),
                                    numericInput('rmrows', "start row", value = 1, min = 1),
                                    actionButton("intcols","Select Columns"),
                                    tags$hr(),
                                    #numericInput('ngroups', "Number of Groups", value = 1, min = 1),
                                    #rHandsontableOutput('groupnames'),
                                    #tags$hr(),
                                    rHandsontableOutput('anagrouping'),
                                    actionButton("confgroups","Confirm Grouping"),
                                    downloadButton("savegroups","Save Grouping"),
                                    fileInput('loadgroups', NULL,
                                              accept= NULL),
                                    tags$hr(),
                                    actionButton("anatbl","run analysis"),
                                    checkboxInput('pv', 'Calculate Pvalues', FALSE)#,
                                    # bookmarkButton()
                                )),
                            tabPanel(
                                "MS data files",
                                verticalLayout(
                                    textInput('rawpath','Enter raw file folder'),
                                    actionButton('rfileload',"Confirm folder"),
                                    # numericInput('nrgroups', "Number of Groups", value = 1, min = 1),
                                    # rHandsontableOutput('rgroupnames'),
                                    
                                    rHandsontableOutput('rawgrouping'),
                                    actionButton("confrgroups","Confirm & Load"),
                                    downloadButton("savergroups","Save Grouping"),
                                    fileInput('loadrgroups', NULL,
                                              accept= NULL)
                                    
                                    
                                ))#end TabPanel
                            #)# end sidebarlayout
                        )# end tabsetPanel
                        ),#End Setup
               
               tabPanel("Filter",
                        sidebarLayout(
                            
                            sidebarPanel(
                                "Filter options",
                                htmlOutput('selgr'),
                                htmlOutput('subsel1'),
                                #      rHandsontableOutput('filtable1'), 
                                
                                #  numericInput('grfold',"Min group fold change",value = 0),
                                #   numericInput('grint',"Min group mean intensity",value = 0),
                                #   numericInput('grintmx',"Min group max intensity",value = 0),
                                
                                htmlOutput('subsel2'),
                                htmlOutput('subsel3'),
                                #  numericInput('saint',"Min sample intensity",value = 0),
                                #   numericInput('safoldmx',"Min sample fold over max",value = 0),
                                #   numericInput('safoldav',"Min group fold over mean",value = 0),
                                htmlOutput('subsel4'),
                                htmlOutput('subsel5'),
                                hr(),
                                textInput("fname", label = "Filename:", value = "Enter here.."),
                                downloadButton("pdfButton", "EICplot PDF"),
                                # actionButton("tbButton", "Export table"),
                                downloadButton("tbButton","Save Table"),
                                htmlOutput('mzquery'),
                                textInput("mzspace", label = "Select elements:", value = "C0-100H0-202N0-15O0-20"),
                                numericInput("mzcharge", "Charge", 0, step=1),
                                numericInput("mzppm", "ppm", 5, step=0.1),
                                actionButton("mzButton", "Sum formulas"),
                                rHandsontableOutput('hot1')
                            ), #endsidebarpanel
                            
                            mainPanel(
                                verticalLayout( 
                                    tabsetPanel(
                                        
                                        tabPanel(
                                            "Table"
                                        ),#end tab
                                        tabPanel(
                                            "EICplot",
                                            plotOutput('plot1',
                                                       height = "1000px"
                                            )
                                        ),#end tab
                                        tabPanel(
                                            "Heatmap",
                                            actionButton("goButton", "Update heatmap"),
                                            plotlyOutput('plot2',
                                                         height = "1000px")
                                        ),#end tab
                                        tabPanel(
                                            "Explorer",
                                            titlePanel("Viewer GUI"),
                                            htmlOutput('EICfiles'),
                                            plotlyOutput('chromly'),
                                            verbatimTextOutput('specinfo'),
                                            plotOutput('spec', height = 500,
                                                       click = "spec_click",
                                                       hover = "spec_hover",
                                                       dblclick = "spec_dblclick",
                                                       brush = brushOpts(
                                                           id = "spec_brush",
                                                           direction = "x",
                                                           resetOnNew = TRUE))
                                            
                                        ),
                                        tabPanel(
                                            "MS2 Explorer",
                                            titlePanel("MS2 search"),
                                            #div(DT::dataTableOutput('parenttab', height = "auto"), style = "font-size: 75%"),
                                            rHandsontableOutput('fragtab2', height = "300px"),
                                            actionButton("sFrags", "Save Fragments"),
                                            plotOutput("molplot",height = "300px", width = "300px"),
                                            verbatimTextOutput('specinfo2'),
                                            plotOutput('spec2', height = 500,
                                                       click = "spec2_click",
                                                       hover = "spec2_hover",
                                                       dblclick = "spec2_dblclick",
                                                       brush = brushOpts(
                                                           id = "spec2_brush",
                                                           direction = "x",
                                                           resetOnNew = TRUE)),
                                            rHandsontableOutput('parenttab', height = "300px"),
                                            verbatimTextOutput('txt')
                                        )#end tab
                                        
                                    ),#end tabset
                                    # tags$head(
                                    #  tags$style(HTML("
                                    #        .dataTables_wrapper { overflow-x: scroll; }
                                    #       " ))),
                                    actionButton("fButton", "Filter"),
                                    actionButton("fButton2", "Reload Filter"),
                                    rHandsontableOutput('filtable2'),
                                    #rHandsontableOutput('hot1'),
                                    div(DT::dataTableOutput('hmtable', height = "auto"), style = "font-size: 75%")
                                    #                                    div(DT::dataTableOutput('hmtable2', height = "auto"), style = "font-size: 75%")
                                    
                                    
                                )# end verticalLayout
                            )#end tabset and mainpanel
                        )# end sidebarlayout
               )# end tabPanel
               
               
    ) #end navbarPage
)#end fluidpage
