
output$mainSort <- renderUI({selectizeInput('mainSort', 'Sort by',
                                            choices = selectedCols(),
                                            selected = featureTables$tables[[featureTables$active]]$sortBy,
                                            multiple = F)})
observeEvent(input$mainSort,{
  
  #Transfer data from visible maintable into the df (save changes prior to subsequent rerendering of rhandsontable)
  if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
    if(featureTables$tables[[featureTables$active]]$editable){
      featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
    }else{
      featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"] 
    }
  }

             cached <- featureTables$tables[[featureTables$active]]$filters$filters#F1out()
             featureTables$tables[[featureTables$active]]$sortBy <- input$mainSort
             featureTables$tables[[featureTables$active]]$filters$filters <- cached
             })

output$mainSortDecreasing <- renderUI({checkboxInput('mainSortDecreasing', 'Decreasing', value = featureTables$tables[[featureTables$active]]$sortByDecreasing)})
output$mainSortToggle <- renderUI({checkboxInput('mainSortToggle', 'activate', value = F)})


observeEvent(c(input$mainSortDecreasing, input$mainSortToggle),{
  
  if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
    if(featureTables$tables[[featureTables$active]]$editable){
      featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
    }else{
      featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"] 
    }
  }
  
    featureTables$tables[[featureTables$active]]$filters$filters <- featureTables$tables[[featureTables$active]]$filters$filters #F1out()
             featureTables$tables[[featureTables$active]]$sortByDecreasing <- if(input$mainSortToggle){input$mainSortDecreasing}else{NULL}
             })


observeEvent(input$addFilter,{
    numberofFilters <- max(as.integer(gsub("Filter","",names(featureTables$tables[[featureTables$active]]$filters$filters))))
    
    featureTables$tables[[featureTables$active]]$filters$filters[[paste0("Filter",numberofFilters+1)]] <- list(selected = 1:nrow(featureTables$tables[[featureTables$active]]$df),
                                                                              column = colnames(featureTables$tables[[featureTables$active]]$df)[1],
                                                                              minSel = NULL,
                                                                              maxSel = NULL,
                                                                              modeSel = NULL,
                                                                              txtSel = "",
                                                                              active = F
    )
    #solely to trigger drawing of the UI elements for the added filter
            F1out()
            })

#observeEvent(input$multiFilterToggle,{

#  for( i in 1:length(featureTables$tables[[featureTables$active]]$filters$filters)){
 #   featureTables$tables[[featureTables$active]]$filters$filters[["Filter1"]]$active <- input$multiFilterToggle
    
 # }
  #solely to trigger drawing of the UI elements for the added filter
  #F1out()
#})



#this reactive element does not update if not called, maybe because no reactive elements called inside (only reactiveValues)
F1out <-    reactive({
    out <- lapply(names(featureTables$tables[[featureTables$active]]$filters$filters), function (i){
 callModule(FilterModule,i, tag = i,
                    df = reactive({featureTables$tables[[featureTables$active]]$df}),
                    presets = reactive({featureTables$tables[[featureTables$active]]$filters$filters[[i]]}))()

     }
)
    names(out) <- names(featureTables$tables[[featureTables$active]]$filters$filters)
    featureTables$tables[[featureTables$active]]$filters$filters <- out
    
    actives <- sapply(out,"[[","active")
    selections <- lapply(out,"[[","selected")
    
    if(length(selections) == 1){sele <- selections[[1]]} 
    else if(length(selections) > 1){
      sele <- selections[[1]]
      for(i in 2:length(actives)){
        sele <- sele[which(sele %in% selections[[i]])]
      }
      
    }
    else{sele <- row.names(featureTables$tables[[featureTables$active]]$df)}
    featureTables$tables[[featureTables$active]]$filters$sele <- sele 
    
    
    return(sele)
})

output$FilterUI <- renderUI({
#tagList(
    FilterModuleUI("Filter1")
#) 
    #   output_list <- lapply(names(featureTables$tables[[featureTables$active]]$filters$filters), function (i){
  #      FilterModuleUI(i)
   # })
    #return(do.call(tagList, output_list))
})


#observeEvent(input$updateFilter,{


#})
 
allFilters <- eventReactive(c(input$mainSort,input$updateFilter),{#1:nrow(featureTables$tables[[featureTables$active]]$df)
  
  if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
    if(featureTables$tables[[featureTables$active]]$editable){
      featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
    }else{
      featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"] 
    }
  }
  
  return(F1out())#$Filter1$selected
    #actives <- lapply(featureTables$tables[[featureTables$active]]$filters$filters,"[[","active")
  #featureTables$tables[[featureTables$active]]$filters$sele
  
  })