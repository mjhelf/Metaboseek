#featureTables$tables[[featureTables$active]]$intensities_normalized <- callModule(NormalizeModule, "intensityNormalizer",
 #                                                                                 mx = reactive({featureTables$tables[[featureTables$active]]$intensities}))

callModule(densplotModule, "nonNormalizedPlot",
                            mx = reactive({as.matrix(featureTables$tables[[featureTables$active]]$df[,featureTables$tables[[featureTables$active]]$intensities])}),
                            heading = "Input Data")
callModule(densplotModule, "NormalizedPlot",
           mx = reactive({as.matrix(featureTables$tables[[featureTables$active]]$df[,featureTables$tables[[featureTables$active]]$intensities_norm])}),
           heading = "Normalized Data")






observeEvent(input$normbutton,{
    #normalize data and save it in matrix
    mx <- as.matrix(featureTables$tables[[featureTables$active]]$df[,featureTables$tables[[featureTables$active]]$intensities])
    mx <- featureTableNormalize(mx,
                                raiseZeros =  min(mx[which(!mx==0, arr.ind=T)]))
   # mx <- featureTableNormalize(mx, log =  "log10")
    mx <- featureTableNormalize(mx, normalize = "colMeans")
    #make copy of normalized intensities in active table df
    mx <- as.data.frame(mx)
    colnames(mx) <- paste0(colnames(mx),"__norm")
    featureTables$tables[[featureTables$active]] <- updateFeatureTable(featureTables$tables[[featureTables$active]],mx)
    })


output$usenormdata <- renderUI({checkboxInput('usenormdata', 'Use normalized data', value = FALSE)})

output$selctrl <- renderUI({selectizeInput('selctrl', 'Select control group(s)',
                                           choices = c(featureTables$tables[[featureTables$active]]$gNames),
                                           selected = featureTables$tables[[featureTables$active]]$ctrlGroups,
                                           multiple = T)})
observeEvent(input$selctrl,{featureTables$tables[[featureTables$active]]$ctrlGroups <- input$selctrl
    
})

output$selAna <- renderUI({selectizeInput('selAna', 'Select analyses',
                                           choices = c("Basic analysis", "p-values", "Peak shapes"),
                                           selected = "Basic analysis",
                                           multiple = T)})

observeEvent(input$analyzebutton,{
    if("Peak shapes" %in% input$selAna){
            inp <- bestgauss(
                rawdata= MSData$data,
                mz = data.frame(mzmin = featureTables$tables[[featureTables$active]]$df$mzmin, mzmax=featureTables$tables[[featureTables$active]]$df$mzmax),
                rt = data.frame(rtmin = featureTables$tables[[featureTables$active]]$df$rt-5, rtmax=featureTables$tables[[featureTables$active]]$df$rt+5),
                rnames = row.names(featureTables$tables[[featureTables$active]]$df),
                byFile = T,
                getgauss = T
            )

        featureTables$tables[[featureTables$active]] <- updateFeatureTable(featureTables$tables[[featureTables$active]],inp)
    }
    if("Basic analysis" %in% input$selAna){
        featureTables$tables[[featureTables$active]] <- updateFeatureTable(featureTables$tables[[featureTables$active]],featureCalcs(featureTables$tables[[featureTables$active]]$df))
        if(input$usenormdata){
        inp <- foldChange(as.matrix(featureTables$tables[[featureTables$active]]$df
                                                      [,featureTables$tables[[featureTables$active]]$intensities_norm]
                                    
                                                      ),
                                            featureTables$tables[[featureTables$active]]$anagroupnames_norm, ctrl = input$selctrl)
    }else{
        inp <- foldChange(as.matrix(featureTables$tables[[featureTables$active]]$df
                                     [,featureTables$tables[[featureTables$active]]$intensities]),
                                    
                                     #),
                           featureTables$tables[[featureTables$active]]$anagroupnames,ctrl = input$selctrl)}
    featureTables$tables[[featureTables$active]] <- updateFeatureTable(featureTables$tables[[featureTables$active]],inp)}
    if("p-values" %in% input$selAna){
        if(input$usenormdata){
            inp <- multittest(pl = featureTables$tables[[featureTables$active]]$df
                              [,featureTables$tables[[featureTables$active]]$intensities_norm],
                              groups = featureTables$tables[[featureTables$active]]$anagroupnames_norm)
        }else{
            inp <- multittest(pl = featureTables$tables[[featureTables$active]]$df
                                        [,featureTables$tables[[featureTables$active]]$intensities],
            groups = featureTables$tables[[featureTables$active]]$anagroupnames)}
        featureTables$tables[[featureTables$active]] <- updateFeatureTable(featureTables$tables[[featureTables$active]],inp)
        
    }
})