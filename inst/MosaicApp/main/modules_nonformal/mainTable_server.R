
precombino <- reactive({
    
    
  #  if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
   #     if(featureTables$tables[[featureTables$active]]$editable){
    #        featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
     #   }else{
      #      featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"] 
  #      }
   # }
    
    selrows <- if(!is.null(allFilters())){featureTables$tables[[featureTables$active]]$df[allFilters(),]}else{featureTables$tables[[featureTables$active]]$df}
    
    if(length(input$mainSortToggle) !=0 && input$mainSortToggle){
    combinosort <- order(selrows[,input$mainSort], decreasing = input$mainSortDecreasing)
    
    combino <- selrows[combinosort,]
    }else{
        combino <- selrows
    }
})

combino <- reactive({

    combino <- precombino()[,selectedCols()]

    return(combino)
    
            })


pagination <- reactive({
    
    
    
})

output$tablePage <- renderUI({
    numericInput("tablePage","Page:", value = 1,#featureTables$tables[[featureTables$active]]$filters$page,#1, #coupling here creates mismatch crashes
                 min = 1,
                 max = NA#if(featureTables$tables[[featureTables$active]]$editable){1}else{20#ceiling(nrow(combino())/50)# coupling here creates issue with not being able to increase page size
        #             }
    )
})

observeEvent(input$tablePage,{

    #update internal tables with new values from mainTable UI when tablepage is changed.
    #Problem: Timing - tablePage changes when activeTable is changed, and then there is mismatch.
    if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
    if(featureTables$tables[[featureTables$active]]$editable){
        featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
    }else{
        featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"] 
    }
    }
  #Handle exception: NULL returned when user deletes page number before reentering
  if(is.na(input$tablePage)){
    featureTables$tables[[featureTables$active]]$filters$page <- 1
  }else{
  
    if(input$tablePage <= if(featureTables$tables[[featureTables$active]]$editable){1}else{ceiling(nrow(combino())/50)}){
    featureTables$tables[[featureTables$active]]$filters$page <- input$tablePage
    }else{
        featureTables$tables[[featureTables$active]]$filters$page <- if(featureTables$tables[[featureTables$active]]$editable){1}else{ceiling(nrow(combino())/50)}
    }
  }
})


output$tableInfo <- renderText({paste0(nrow(combino()),
                                             " items (",
                                             if(featureTables$tables[[featureTables$active]]$editable){"1 page, editable)"}
                                             else{paste0(ceiling(nrow(combino())/50),"page(s))")}
                                        )
})

#output$tableInfo <- renderUI(
 #                             {textOutput("tableInfo",
  #                                        paste0(nrow(combino()),
   #                                        " items (",
    #                                      if(featureTables$tables[[featureTables$active]]$editable){"1 page, editable)"}
     #                                    else{paste0(ceiling(nrow(combino())/50),"page(s))")}))
#})

output$tbButton <- downloadHandler(filename= function(){paste0(input$projectName,"_",featureTables$active,"_table.csv")}, 
                                   content = function(file){
                                     
                                     if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
                                       if(featureTables$tables[[featureTables$active]]$editable){
                                         featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
                                       }else{
                                         featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"] 
                                       }
                                     }
                                     
                                     temp <- precombino()
                                  # temp$Fragments <- tablestuff$fragments[sele()]
                                   return(write.csv(temp, file, row.names = F))
                                   },
                                   contentType = "text/csv"
)

observeEvent(input$newTable, {
    shinyjs::toggle(id = "tableSaver", anim = T)
})

observeEvent(input$cancelTable, {
    shinyjs::hide(id = "tableSaver", anim = T)
})

output$tableSaver <- renderUI({
    
        fluidRow(htmlOutput("tableNaming"),
        actionButton("makeTable","Confirm Name"),
        actionButton("cancelTable","Cancel"))
    
    })
output$tableNaming <- renderUI({
    textInput("tableNaming",label = "Choose a name for the new table object", value = paste0(featureTables$active,"__",length(featureTables$index)+1))
})

observeEvent(input$makeTable,{
  
  if(!is.null(featureTables$tables[[featureTables$active]]$editable) & !is.null(input$maintable)){
    if(featureTables$tables[[featureTables$active]]$editable){
      featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),c(colnames(hot_to_r(input$maintable)))]
    }else{
      featureTables$tables[[featureTables$active]]$df[c(row.names(hot_to_r(input$maintable))),"comments"] <- hot_to_r(input$maintable)[c(row.names(hot_to_r(input$maintable))),"comments"] 
    }
  }
  
    tabid <- paste0("table",length(featureTables$tables))
    featureTables$tables[[tabid]] <- featureTables$tables[[featureTables$active]]
    featureTables$tables[[tabid]]$df <- precombino()
    featureTables$tables[[tabid]]$tablename <- input$tableNaming
    featureTables$index <- updateFTIndex(featureTables$tables)
    featureTables$active <- tabid
    shinyjs::hide(id = "tableSaver", anim = T)
    })

# which rows to show on the current page
observeEvent(input$tablePage,{
                      if(is.na(input$tablePage)){
                       pagenum <- 1 
                      }else{
                        pagenum <- input$tablePage
                      }
    featureTables$tables[[featureTables$active]]$filters$inpage       <-     if(featureTables$tables[[featureTables$active]]$editable){c(1:nrow(combino()))}
                           else if(pagenum == ceiling(nrow(combino())/50)){c((featureTables$tables[[featureTables$active]]$filters$page*50-49):(nrow(combino())))}#-((input$tablePage-1)*50)))}
                           else{c((pagenum*50-49):(pagenum*50))}
                               
                               })

#render the active table in the main Table box
output$maintable <- renderRHandsontable({if(!is.null(combino())){
  
    inpage <- reactive({
              if(featureTables$tables[[featureTables$active]]$editable){c(1:nrow(combino()))}
              else if(featureTables$tables[[featureTables$active]]$filters$page == ceiling(nrow(combino())/50)){c((featureTables$tables[[featureTables$active]]$filters$page*50-49):(nrow(combino())))}#c((featureTables$tables[[featureTables$active]]$filters$page*50-49):(nrow(combino())-((featureTables$tables[[featureTables$active]]$filters$page-1)*50)))}
              else{c((featureTables$tables[[featureTables$active]]$filters$page*50-49):(featureTables$tables[[featureTables$active]]$filters$page*50))}
    })
    
  rheight <- if(nrow(combino()[inpage(),])<40){NULL}else{500}
  
    rhandsontable(combino()[inpage(),],
                  readOnly = !featureTables$tables[[featureTables$active]]$editable,
                  contextMenu = featureTables$tables[[featureTables$active]]$editable,
                  selectCallback = TRUE,
                  height = rheight,
                 # width = 1000,
                  digits=8,
                 highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_col("comments", readOnly = FALSE)%>%
        hot_cols(columnSorting = FALSE,format="0.000000")%>%
      hot_cols(fixedColumnsLeft = 3)%>%
    #  hot_cols(columnSorting = TRUE)%>%
        #hot_col("em",format="0.000000")%>%
        hot_cols(renderer = "
                 function(instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.TextCell.renderer.apply(this, arguments);
                 td.style.color = 'black';
                 }")
    
    
    
    
    
    }
})

maintabsel <- reactive({ 
  if(!is.null(input$maintable_select$select)){
    return(list(c1 = as.integer(input$maintable_select$select$c),
         c2 = as.integer(input$maintable_select$select$c2),
         r1 = as.integer(input$maintable_select$select$r),
         r2 = as.integer(input$maintable_select$select$r2),
         crng = as.integer(input$maintable_select$select$c):as.integer(input$maintable_select$select$c2),
         rrng = as.integer(input$maintable_select$select$r):as.integer(input$maintable_select$select$r2))
    )}
  else{return(NULL)}
        })