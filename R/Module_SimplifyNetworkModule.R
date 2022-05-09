#' SimplifyNetworkModule
#' 
#' A module to simplify molecular networks to aid visualization
#' 
#' @describeIn SimplifyNetworkModule server logic
#' 
#' @inherit MseekModules
#' 
#' @return Returns its internalValues
#' 
#' @export 
SimplifyNetworkModule <- function(input,output, session,
                                  values = reactiveValues(Networks = NULL,
                                                          GlobalOpts = NULL),
                                  reactives = reactive({list(activeNetwork = NULL)})){
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(numNetworks = 0,
                                   edgecolnames = NULL,
                                   colSelected1 = "cosine",
                                   colSelected2 = "deltamz")

  observe({
    
    if(length(names(values$Networks)[names(values$Networks) != "numNetworks"]) >0 
       & !is.null(reactives()$activeNetwork)){
     
      internalValues$edgecolnames <- edge_attr_names(values$Networks[[reactives()$activeNetwork]]$graph)
      
    }
    
    
    
  })
  
  # output$edgeInfo <- renderUI({
  #   
  #   if(!is.null(input$col1min)
  #      && !is.null(is.null(values$featureTables$tables[[values$featureTables$active]]$edges))){
  #    # print(values$Networks[[reactives()$activeNetwork]]$tables$edges)
  #   #  print(internalValues$colSelected1)
  #     p(paste0("With the current filter setting, you would remove at least ", 
  #              sum(values$Networks[[reactives()$activeNetwork]]$tables$edges[[internalValues$colSelected1]] >= input$col1min
  #                  & values$Networks[[reactives()$activeNetwork]]$tables$edges[[internalValues$colSelected1]] <= input$col1max),
  #              " edges."))
  #     
  #   }
  #   
  # })
    
  
  
  output$colSel1 <- renderUI({
          if(!is.null(reactives()$activeNetwork)){

    tooltip <- tryCatch({
        
        paste0("Numeric column, range:",
               round(min(as.numeric(values$Networks[[reactives()$activeNetwork]]$tables$edges[,internalValues$colSelected1])),3),
               " - ", 
               round(max(as.numeric(values$Networks[[reactives()$activeNetwork]]$tables$edges[,internalValues$colSelected1])),3),
               ", mean: ",
               round(mean(as.numeric(values$Networks[[reactives()$activeNetwork]]$tables$edges[,internalValues$colSelected1])),3),
               ", median: ",
               round(median(as.numeric(values$Networks[[reactives()$activeNetwork]]$tables$edges[,internalValues$colSelected1])),3)
        )},
        warning = function(w){paste("Error in calculation")},
        error = function(e){paste("Error in calculation")})

    div(title = tooltip,
        selectizeInput(ns('colsel1'), "Filter 1",
                       choices = internalValues$edgecolnames,
                       selected = if(is.null(internalValues$colSelected1) || !internalValues$colSelected1 %in% internalValues$edgecolnames){NULL}else{internalValues$colSelected1},
                       multiple = F)
    )
    }
  }) 
  
  observeEvent(input$colsel1,{
    
    internalValues$colSelected1 <- input$colsel1

  })
  
  output$colSel2 <- renderUI({
    if(!is.null(reactives()$activeNetwork)){
    tooltip <- tryCatch({
      paste0("Numeric column, range:",
             round(min(as.numeric(values$Networks[[reactives()$activeNetwork]]$tables$edges[,internalValues$colSelected2])),3),
             " - ", 
             round(max(as.numeric(values$Networks[[reactives()$activeNetwork]]$tables$edges[,internalValues$colSelected2])),3),
             ", mean: ",
             round(mean(as.numeric(values$Networks[[reactives()$activeNetwork]]$tables$edges[,internalValues$colSelected2])),3),
             ", median: ",
             round(median(as.numeric(values$Networks[[reactives()$activeNetwork]]$tables$edges[,internalValues$colSelected2])),3)
      )},
      warning = function(w){paste("Error in calculation")},
      error = function(e){paste("Error in calculation")})
    
    div(title = tooltip,
        selectizeInput(ns('colsel2'), "Filter 2",
                       choices = internalValues$edgecolnames,
                       selected = if(is.null(internalValues$colSelected2) || !internalValues$colSelected2 %in% internalValues$edgecolnames){NULL}else{internalValues$colSelected2},
                       multiple = F)
    )
    }
  }) 
  
  observeEvent(input$colsel2,{
    
    internalValues$colSelected2 <- input$colsel2
    
  })
  
  output$edgeInfo1 <- renderUI({
    
    if(!is.null(input$cosThresh)
       && !is.null(is.null(values$featureTables$tables[[values$featureTables$active]]$edges))){
      
      p(paste0("With the current cutoff at ", input$cosThresh,", a network would contain ", 
               sum(na.omit(E(values$Networks[[reactives()$activeNetwork]]$graph)$cosine) >= input$cosThresh),
               " edges."))
      
    }
    
  })
  
  
  observeEvent(input$simplifyButton,{
    
    if(length(names(values$Networks)[names(values$Networks) != "numNetworks"]) >0 
       & !is.null(reactives()$activeNetwork)){
      
      showModal(
        modalDialog(
          fluidPage(
            fluidRow(
              p(strong("Generate an edge table for the current feature table")),
              p("You can simplify the current network in different ways:")
            ),
            hr(),
            fluidRow(
              div( title = "Remove edges with cosine below this threshold: ", 
              checkboxInput(ns("cosThreshCheck"),"Remove edges below threshold", value = F)),
           
              numericInput(ns("cosThresh"), "Cosine threshold", value = round(min(na.omit(E(values$Networks[[reactives()$activeNetwork]]$graph)$cosine)),3)-0.001)
              
            ),
            fluidRow(
              htmlOutput((ns("edgeInfo1")))
            ),
            # hr(),
            # 
            # fluidRow(
            #   p("Merge Nodes functionality temporarily disabled until bugs are fixed. Selections will be ignored."),
            #   column(3, checkboxInput(ns("mergeCheck"),"Merge Nodes", value = F)),
            #   column(4, htmlOutput(ns("colSel1"))),
            #   column(2, div(title = "Minimum value for edges to merge",
            #                 numericInput(ns("col1min"),"min", value = 0.95))),
            #   column(2, div( title = "Maximum value for edges to merge", 
            #                  numericInput(ns("col1max"), "max", value = 1)))),
            # fluidRow(
            #   column(3, checkboxInput(ns("mergeCheck2"),"Use second merge condition", value = F)),
            #   column(4, htmlOutput(ns("colSel2"))),
            #   column(2, div(title = "Minimum value for edges to merge",
            #                 numericInput(ns("col2min"),"min", value = -0.001))),
            #   column(2, div( title = "Maximum value for edges to merge", 
            #                  numericInput(ns("col2max"), "max", value = 0.001)))
            # ),
            # fluidRow(
            #   htmlOutput(ns("edgeInfo"))
            # ),
                hr(),
    
            fluidRow(
              
              column(3, div(title = "Only keep the top k edges of each node",
                            checkboxInput(ns("maxKcheck"),"Restrict edges per node", value = F))),
              column(4, 
                     selectizeInput(ns('maxKcol'), "Rank by",
                             choices = internalValues$edgecolnames,
                             selected = if(!"cosine" %in% internalValues$edgecolnames){NULL}else{"cosine"},
                             multiple = F)
              ),
              column(3, div( title = "", 
                             numericInput(ns("maxK"), "Maximum number of edges per node (k)", value = 10)))
              
              ), hr(),
            
            fluidRow(
              
              column(3, div(title = "Remove edges from large clusters until they collapse in size",
                            checkboxInput(ns("maxConnectionsCheck"),"Restrict nodes per cluster", value = F))),
              column(4, 
                     numericInput(ns('maxConnections'), "Maximum Cluster size",
                                    value = 100)
              ),
              column(3, div(title = "Remove 1% lowest value edges from large cluster each iteration (slower, but less chance of breaking up edges unnecessarily).",
                            checkboxInput(ns("percentileCheck"),"Remove by percentile", value = TRUE)))
              ),
              hr(),
            fluidRow(
              div( title = "Name of the simplified network:", 
                   textInput(ns('NetName2'), "Network name", value = paste0("simplified_", reactives()$activeNetwork )))
          ),
            hr(),
            fluidRow(
              div( title = "Use these parameters to simplify the network", 
                             mActionButton(ns("simplifyNow"), "Go", red = T)))
              
            ),
          title = "Simplify network",
          easyClose = T,
          fade = F,
          size = "l",
          footer = modalButton("Cancel") 
        ))
      
    }
    
    else{
      
      showNotification(paste("You have to load or create a network first!"), type = "error", duration = 10)
      
      
    }
  })
  
  
  observeEvent(input$simplifyNow,{
    if(input$maxKcheck || input$cosThreshCheck || input$maxConnectionsCheck){
        tryCatch({  
          withProgress(message = 'Please wait!', detail = "Simplifying Network", value = 0.5, {
            
          tempgraph <- values$Networks[[reactives()$activeNetwork]]
          if(input$maxKcheck || input$cosThreshCheck){
            tempgraph <- simplify(tempgraph,
                                                                     rankBy = if(input$maxKcheck){input$maxKcol}else{NULL},
                                                                     maxK = if(input$maxKcheck){input$maxK}else{NULL},
                                                                     cosineThreshold = if(input$cosThreshCheck){input$cosThresh}else{NULL},
                                  layoutFunction = values$GlobalOpts$graph.layouts.selected
                                  )
            if(hasError(previousStep(tempgraph))){
              showNotification(paste("An error occured: ",
                                     unlist(error(previousStep(tempgraph)))),
                               duration = 0, type = "error")
            }else{
              
              values$Networks[[gsub("\\.[^.]*$","",input$NetName2)]] <- tempgraph
              removeModal()
              values$Networks$numNetworks <- values$Networks$numNetworks + 1
              showNotification(paste("Simplified Network"), duration = 10)
              
            }
}
          
             
          if(input$maxConnectionsCheck){
            tempgraph <- limitComponents(tempgraph,
                                  rankBy = "cosine",
                                  n = input$maxConnections,
                                  layoutFunction = values$GlobalOpts$graph.layouts.selected,
                                  percentile = input$percentileCheck
            )
            
            if(hasError(previousStep(tempgraph))){
              showNotification(paste("An error occured: ",
                                     unlist(error(previousStep(tempgraph)))),
                               duration = 0, type = "error")
            }else{
              
              values$Networks[[gsub("\\.[^.]*$","",input$NetName2)]] <- tempgraph
              removeModal()
              values$Networks$numNetworks <- values$Networks$numNetworks + 1
              showNotification(paste("Simplified Network"), duration = 10)
              
            }
          }
          })
          showNotification(paste("Network simplified!"), duration = 10)
          
    },
    error = function(e){
      #print("graph not loaded")
      #print(e)
      showModal(
        modalDialog(title = "An error has occured",
                    "Load a node and edge table belonging to the same network, and make sure you selected the correct Table Options (tab or comma separated?) for your input table.",
                    hr(),
                    p(strong("Error:")),
                    p(paste(e, collapse = "\n")),
                    easyClose = T
        )
      )
    }
    
        )
      
      }
      else{
          showNotification(paste("You have to select a simplificatioin method to continue"), 
                           type = "warning", duration = 10)
          
          }
    
    
  })
  
  
  return(internalValues)
}
#' @describeIn SimplifyNetworkModule UI elements
#' @export
SimplifyNetworkModuleUI <-  function(id){
  ns <- NS(id)
  actionButton(ns("simplifyButton"), label = "Simplify network", icon = icon("cog", lib = "glyphicon"))
  
}