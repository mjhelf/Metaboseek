function(input, output, session) {
  
  MosaicMinimalServer(diagnostics = devel__mode, exampleData = F)
  
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
    
   # source(file.path("modules_nonformal", "bookmarking_server.R"), local = TRUE)$value 
    #source(file.path("modules_nonformal", "logo_server.R"), local = TRUE)$value 
    
    #source(file.path("modules_nonformal", "diagnostics_server.R"), local = TRUE)$value    

    callModule(updaterModule, 'update', tag = 'update', set =list(package = "Mosaic",
                                                                  refs = c("master", "devel", "devel_raw"),
                                                                  active = !servermode))
    
    #source(file.path("modules_nonformal", "help_server.R"), local = TRUE)$value 
    
    source(file.path("modules_nonformal", "loadtables_server.R"), local = TRUE)$value
    source(file.path("modules_nonformal", "loadMSdata_server.R"), local = TRUE)$value
    
    source(file.path("modules_nonformal", "processTableData_server.R"), local = TRUE)$value
    
    source(file.path("modules_nonformal", "exploreData_main_server.R"), local = TRUE)$value 
    
xcmsOut <- callModule(xcmsModule, "xcmsMod",
                      values = list(MSData = MSData),
                      static = list(servermode = servermode,
                                    activateXCMS = activateXCMS,
                                    rootpath = rootpath)
    )
    

    
}