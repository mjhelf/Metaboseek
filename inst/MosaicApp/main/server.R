function(input, output, session) {
  
  MosaicMinimalServer(diagnostics = .MosaicOptions$develMode, data = .MosaicOptions$loadExampleData, tables = .MosaicOptions$loadExampleTable)
  
    selectedTabs <- reactiveValues(FeatureTable = "View Table")

    output$activeTable <- renderUI({
        selectizeInput('activeTable', 'Active Table', selected = featureTables$active, choices = featureTables$index, multiple = FALSE)
    })  
    
    observeEvent(input$activeTable, { 
        
        if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
            if(featureTables$tables[[featureTables$active]]$editable){
                featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
            }else{
                featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"] 
            }
        }
        
        featureTables$active <- input$activeTable})
    
     callModule(updaterModule, 'update', tag = 'update', set =list(package = "Mosaic",
                                                                  refs = c("master", "devel", "devel_raw"),
                                                                  active = !.MosaicOptions$serverMode))
    

    source(file.path("modules_nonformal", "loadtables_server.R"), local = TRUE)$value
     
     FTGrouper <- callModule(ChangeFTGroupingModule, "ftgrouper",
                        reactives = reactive({list()}),
                        values = reactiveValues(fileGrouping = NULL,
                                                featureTables = featureTables,
                                                MSData = MSData,
                                                projectData = projectData),
                        static = list()
     )
     
     
     
    source(file.path("modules_nonformal", "loadMSdata_server.R"), local = TRUE)$value
    
    
    tAnalysis <- callModule(TableAnalysisModule, "TabAnalysis",
                             reactives = reactive({list()}),
                             values = reactiveValues(fileGrouping = NULL,
                                                     featureTables = featureTables,
                                                     MSData= MSData))

    source(file.path("modules_nonformal", "exploreData_main_server.R"), local = TRUE)$value 
    
xcmsOut <- callModule(xcmsModule, "xcmsMod",
                      values = list(MSData = MSData),
                      static = list(servermode = .MosaicOptions$serverMode,
                                    activateXCMS = .MosaicOptions$activateXCMS,
                                    rootpath = .MosaicOptions$filePaths,
                                    filePattern = .MosaicOptions$filePattern)
    )
    

    
}