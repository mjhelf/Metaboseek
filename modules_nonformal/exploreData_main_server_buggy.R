
###Column Selection

#output$selnormdata <- renderUI({checkboxInput('selnormdata', 'Use normalized data', value = FALSE)})
output$mainSelGroup <- renderUI({selectizeInput('mainSelGroup', 'Group of interest',
                                                choices = featureTables$tables[[featureTables$active]]$gNames,
                                                selected = featureTables$tables[[featureTables$active]]$selectedGroup,
                                                multiple = F)})
observeEvent(input$mainSelGroup,{featureTables$tables[[featureTables$active]]$selectedGroup <- input$mainSelGroup
updateSelectizeInput(session, 'mainSelgProps', selected = featureTables$tables[[featureTables$active]]$gProps[[featureTables$tables[[featureTables$active]]$selectedGroup]])


})

output$mainSelgProps <- renderUI({selectizeInput('mainSelgProps', 'Group properties', 
                                                 choices = featureTables$tables[[featureTables$active]]$gProps,
                                                 selected = featureTables$tables[[featureTables$active]]$gProps[[featureTables$tables[[featureTables$active]]$selectedGroup]],
                                                 multiple = T)
})
observeEvent(
         input$mainSelgProps,{featureTables$tables[[featureTables$active]]$selectedCols$gProps <- input$mainSelgProps
         updateSelectizeInput(session, 'mainSelgProps', selected = featureTables$tables[[featureTables$active]]$selectedCols$gProps)
         print("changing mainSelProps")
})


output$mainSelsProps <- renderUI({selectizeInput('mainSelsProps', 'Sample properties', 
                                                 choices = featureTables$tables[[featureTables$active]]$sProps,
                                                 selected = featureTables$tables[[featureTables$active]]$sProps[[featureTables$tables[[featureTables$active]]$selectedGroup]], multiple = T)})
observeEvent(input$mainSelsProps,{featureTables$tables[[featureTables$active]]$selectedCols$sProps <- input$mainSelsProps
})

output$mainSelIntensities <- renderUI({selectizeInput('mainSelIntensities', 'Sample intensities', 
                                                 choices = list(Intensities = featureTables$tables[[featureTables$active]]$anagroupnames,
                                                                "Normalized Intensities" = featureTables$tables[[featureTables$active]]$anagroupnames_norm),
                                                 selected = featureTables$tables[[featureTables$active]]$anagroupnames[[input$mainSelGroup]], multiple = T)})
observeEvent(input$mainSelIntensities,{featureTables$tables[[featureTables$active]]$selectedCols$intensities <- input$mainSelIntensities
                   updateSelectizeInput(session, 'mainSelIntensities', selected = featureTables$tables[[featureTables$active]]$selectedCols$intensities)

})

output$mainSelOthers <- renderUI({selectizeInput('mainSelOthers', 'other columns', 
                                                      choices = list("Basic Stats" = featureTables$tables[[featureTables$active]]$summaryStats,
                                                                     "Others" = featureTables$tables[[featureTables$active]]$others),
                                                     selected = featureTables$tables[[featureTables$active]]$selectedCols$others, multiple = T)})
observeEvent(input$mainSelOthers,{featureTables$tables[[featureTables$active]]$selectedCols$others <- input$mainSelOthers
print("changing mainSelOthers")
})



##Filter and sort



source(file.path("modules_nonformal", "mainTable_server.R"), local = TRUE)$value 