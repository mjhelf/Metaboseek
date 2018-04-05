function(input, output, session) {
    options(shiny.maxRequestSize=1024*1024^2)
    #   session$onSessionEnded(stopApp)
    
    #initialize feature tables
    featureTables <- reactiveValues(tables = list(table0 = constructFeatureTable()),
                                    index = c("Custom Table" = "table0"),
                                    active = "table0"
    )
    
    selectedTabs <- reactiveValues(FeatureTable = "View Table"    )
    
    MSData <- reactiveValues(layouts = NULL, #List of rawfile paths (unsorted)
                             rawgrouptable = NULL,
                             index = NULL,
                             rootfolder = rootpath,
                             localfolders = character(0),
                             RTcorr = NULL,
                             active = NULL,
                             filelist = NULL,
                             data = NULL) #rawfs
    
    projectData <- reactiveValues(filegroupfiles =NULL,
                                  csvfiles = NULL,
                                  filegroups = NULL,
                                  projectName = paste0("MOSAiC_session_",timeStamp))
    
    
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
    
    source(file.path("modules_nonformal", "bookmarking_server.R"), local = TRUE)$value 
    
    
    source(file.path("modules_nonformal", "logo_server.R"), local = TRUE)$value 
    source(file.path("modules_nonformal", "diagnostics_server.R"), local = TRUE)$value    
    source(file.path("modules_nonformal", "background_server.R"), local = TRUE)$value
    
    source(file.path("modules_nonformal", "help_server.R"), local = TRUE)$value 
    
    source(file.path("modules_nonformal", "loadtables_server.R"), local = TRUE)$value
    source(file.path("modules_nonformal", "loadMSdata_server.R"), local = TRUE)$value
    
    source(file.path("modules_nonformal", "processTableData_server.R"), local = TRUE)$value
    
    source(file.path("modules_nonformal", "exploreData_main_server.R"), local = TRUE)$value 
    
    source(system.file("MosaicApp", "xcmsRunner","modules_nonformal", "xcms_light_server.R",package = "Mosaic"), local = TRUE)$value
    
    
}