
###Column Selection

#output$selnormdata <- renderUI({checkboxInput('selnormdata', 'Use normalized data', value = FALSE)})
output$mainSelGroup <- renderUI({selectizeInput('mainSelGroup', 'Group of interest',
                                                choices = featureTables$tables[[featureTables$active]]$gNames,
                                                selected = featureTables$tables[[featureTables$active]]$selectedGroup,
                                                multiple = F)})

output$mainSelgProps <- renderUI({selectizeInput('mainSelgProps', 'Group properties', 
                                                 choices = featureTables$tables[[featureTables$active]]$gProps,
                                                 selected = featureTables$tables[[featureTables$active]]$gProps[[featureTables$tables[[featureTables$active]]$selectedGroup]],
                                                 multiple = T)
})

output$mainSelsProps <- renderUI({selectizeInput('mainSelsProps', 'Sample properties', 
                                                 choices = featureTables$tables[[featureTables$active]]$sProps,
                                                 selected = featureTables$tables[[featureTables$active]]$sProps[[featureTables$tables[[featureTables$active]]$selectedGroup]], multiple = T)})

output$mainSelIntensities <- renderUI({selectizeInput('mainSelIntensities', 'Sample intensities', 
                                                 choices = list(Intensities = featureTables$tables[[featureTables$active]]$anagroupnames,
                                                                "Normalized Intensities" = featureTables$tables[[featureTables$active]]$anagroupnames_norm),
                                                 selected = featureTables$tables[[featureTables$active]]$anagroupnames[[featureTables$tables[[featureTables$active]]$selectedGroup]], multiple = T)})

output$mainSelOthers <- renderUI({selectizeInput('mainSelOthers', 'other columns', 
                                                      choices = list("Basic Stats" = featureTables$tables[[featureTables$active]]$summaryStats,
                                                                     "Others" = featureTables$tables[[featureTables$active]]$others),
                                                     selected = featureTables$tables[[featureTables$active]]$summaryStats, multiple = T)})


selectedCols <- reactive({unique(unname(c(#input$mainSelGroup,
                                       featureTables$tables[[featureTables$active]]$core,
                                       featureTables$tables[[featureTables$active]]$comments,
                                         input$mainSelgProps,
                                         input$mainSelsProps,
                                        input$mainSelIntensities,
                                       input$mainSelOthers)))
    })

observeEvent(c(input$mainSelGroup
 #              input$mainSelgProps,
  #             input$mainSelsProps,
   #            input$mainSelIntensities,
    #           input$mainSelOthers
               ),
               {featureTables$tables[[featureTables$active]]$selectedGroup <- input$mainSelGroup
       #        featureTables$tables[[featureTables$active]]$selectedCols$gProps <- input$mainSelgProps
        #       featureTables$tables[[featureTables$active]]$selectedCols$sProps <- input$mainSelsProps
         #      featureTables$tables[[featureTables$active]]$selectedCols$intensities <- input$mainSelIntensities
          #     featureTables$tables[[featureTables$active]]$selectedCols$others <- input$mainSelOthers
               })


##Filter and sort
source(file.path("modules_nonformal", "mainSort_server.R"), local = TRUE)$value 

##Plots
source(file.path("modules_nonformal", "mainPlots_server.R"), local = TRUE)$value 


##The Main Data Table
source(file.path("modules_nonformal", "mainTable_server.R"), local = TRUE)$value 