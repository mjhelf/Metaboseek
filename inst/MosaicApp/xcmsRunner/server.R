function(input, output, session) {
    options(shiny.maxRequestSize=1024*1024^2)
    #   session$onSessionEnded(stopApp)
    
    #initialize feature tables
 #   featureTables <- reactiveValues(tables = list(table0 = constructFeatureTable()),
#                                    index = c("Custom Table" = "table0"),
 #                                   active = "table0"
  #  )
    
    MSData <- reactiveValues(layouts = list(), #List of rawfile paths (unsorted)
                             rawgrouptable = NULL,
                             index = NULL,
                             active = NULL,
                             filelist = NULL,
                             data = NULL) #rawfs
    
    
    
    source(file.path("modules_nonformal", "xcms_light_server.R"), local = TRUE)$value 
    
    
    
}