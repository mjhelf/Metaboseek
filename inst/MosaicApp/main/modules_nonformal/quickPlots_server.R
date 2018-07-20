callModule(featurePlotModule, "quickplots",
           FT = reactive({featureTables$tables[[featureTables$active]]}),
           rname = reactive({row.names(hot_to_r(input$maintable))[maintabsel()$rrng[1]]})
             )