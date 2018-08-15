
#Project options
source(file.path("modules_nonformal", "project_options_server.R"), local = TRUE)$value 


###Column Selection
ColSel <- callModule(columnSelModule, 'ColSelMod',
                     reactives = reactive({(list())}),
                     values = list(featureTables = featureTables),
                     static = list(),
                     load = reactive({list()}))


observeEvent(c(input$mainSelGroup),
               {featureTables$tables[[featureTables$active]]$selectedGroup <- input$mainSelGroup })


##Filter and sort
source(file.path("modules_nonformal", "mainSort_server.R"), local = TRUE)$value 

##Plots
source(file.path("modules_nonformal", "mainPlots_server.R"), local = TRUE)$value 


##The Main Data Table
source(file.path("modules_nonformal", "mainTable_server.R"), local = TRUE)$value 

###PCA viewer
PcaViewFeatures <- callModule(PcaViewModule, "pcaviewfeatures",
                              values = reactiveValues(featureTables = featureTables)
)