#' LoadNetworkModule
#' 
#' 
#' server module for loading Tables
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param tag id to be used in ns()
#' @param set Import data from the shiny session
#' 
#' @export 
LoadNetworkModule <- function(input,output, session, values = reactiveValues(featureTables = featureTables,
                                                                             MainTable = MainTable,
                                                                             MSData = MSData),
                              reactives = reactive({list(active = NetMod$active)})){
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(numNetworks = 0)
  
  
  loadNodeTab <- callModule(UploadTableModule,'loadNodeTab',
                            static = list(title =  "Load node table",
                                          filetypes = NULL,
                                          format = list(header = T,
                                                        sep = NULL,#"\t",
                                                        quote = '"',
                                                        stringsAsFactors = T))
  )
  
  loadEdgeTab <- callModule(UploadTableModule,'loadEdgeTab', 
                            static = list(title =  "Load edge table",
                                          filetypes = NULL,
                                          format = list(header = T,
                                                        sep = NULL,#"\t",
                                                        quote = '"',
                                                        stringsAsFactors = T))
  )
  
  #load and reformat a  network from a file
  observeEvent(input$networkFileLoad$datapath,{
    
    res <- list(tables = list(nodes = NULL,
                              edges = NULL),
                graph = read_graph(input$networkFileLoad$datapath, "graphml"))
    
    res$tables$nodes <- type.convert(as_data_frame(res$graph, "vertices"), as.is = T)
    
    res$tables$edges <- type.convert(as_data_frame(res$graph, "edges"), as.is = T)
    # 
    # g1 <- graph_from_data_frame(d=res$tables$edges, vertices=res$tables$nodes, directed=F) 
    # V(g1)$label <- V(g1)$parent.mass
    # V(g1)$fixed__id <- seq(vcount(g1))
    # 
    # # Removing loops from the graph:
    # g1 <- simplify(g1, remove.multiple = F, remove.loops = T) 
    # 
    # #important for overview mode!
    # V(g1)$subcl <-  clusters(g1)$membership
    # 
    # res$graph <- g1
    
    internalValues[[gsub("\\.[^.]*$","",input$networkFileLoad$name)]] <- res
    removeModal()
    #print("loadcomplete")
    
  })
  
  #load and reformat a network from edge and node table
  observeEvent(input$loadNetwork,{
    res <- list(tables = list(nodes = NULL,
                              edges = NULL),
                graph = NULL)
    
    res$tables$nodes <- loadNodeTab$df
    res$tables$edges <- loadEdgeTab$df
    
    if(! "fixed__id" %in% colnames(res$tables$nodes)){
    res$tables$nodes$fixed__id <- res$tables$nodes[,1]
  }
    
    tryCatch({  
      g1 <- graph_from_data_frame(d=res$tables$edges, vertices=res$tables$nodes, directed=F) 
      
      
      #V(g1)$label <- V(g1)$parent.mass
      V(g1)$id <- seq(vcount(g1))
      
      # Removing loops from the graph:
      g1 <- simplify(g1, remove.multiple = F, remove.loops = T) 
      
      #important for overview mode!
      V(g1)$subcl <-  clusters(g1)$membership
      
      res$graph <- g1
      
      internalValues[[gsub("\\.[^.]*$","",input$NetName)]] <- res
      internalValues$numNetworks <- internalValues$numNetworks + 1
      
      removeModal()
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
  })
  
  FindMS2 <- callModule(FindMS2ScansModule, "findms2network",
                        values = reactiveValues(featureTables = values$featureTables,
                                                MSData = values$MSData,
                                                MainTable = values$MainTable),
                        static = list(tooltip = "Make a new MS2 network from the current feature table",
                                      label = "New MS2 network")
  )
  
  SaveNetworks <- callModule(SaveNetworkModule, "savenetworks",
                             reactives = reactive({list(graphname = reactives()$active,
                                                        filename = paste0("networks/",reactives()$active,".graphml"))}),
                             values = reactiveValues(Networks = internalValues),
                             static = list(tooltip = "Save Network as a graphml file",
                                           label = "",
                                           format = c("graphml"))
  )
  
  observeEvent(FindMS2$done,{
    
    if(FindMS2$done){
      
      
      
      showModal(
        modalDialog(
          fluidPage(
            fluidRow(
              p(strong("Generate an edge table for the current feature table")),
              p("A network will be constructed using the available MS2 scans identified in the previous step.")
            ),
            hr(),
            
            if(!is.null(values$featureTables$tables[[values$featureTables$active]]$edges)){
              tagList(
                p(strong("An edge table exists already for this feature table. do you want to keep using it?")),
                div(title = "If not selected, a new edge table will be generated using the settings below (potentially time consuming).",
                    checkboxInput(ns("useOldEdges"), "Use previous edge list", value = T)),
                hr()
              )
            }else{p("")},
            
            
            fluidRow(
              column(3, div(title = "Use parent m/z information for networking",
                            checkboxInput(ns("useparentmasses"),"Use parent masses", value = T))),
              column(3, div(title = "Define m/z tolerance for MS2 fragment peak matching",
                            numericInput(ns("mzdiff"),"m/z window for peak matching", value = 0.002))),
              column(3, div( title = "Remove noise (peaks below this threshold", 
                             numericInput(ns("noise"), "Noise level in %", value = 2))),
              column(3, div( title = "Search MS2 scans", 
                             METABOseek:::mActionButton(ns("makeNetwork"), "Proceed", red = T)))
            )),
          title = "Make edges for network",
          easyClose = T,
          fade = F,
          size = "l",
          footer = modalButton("Cancel") 
        ))
      
      FindMS2$done <- F
    }
  })
  
  output$edgeInfo <- renderUI({
    
    if(!is.null(input$cosThresh)
       && !is.null(is.null(values$featureTables$tables[[values$featureTables$active]]$edges))){
      
      p(paste0("With the current cutoff at ", input$cosThresh,", a network would contain ", 
               sum(values$featureTables$tables[[values$featureTables$active]]$edges$cosine >= input$cosThresh),
               " edges."))
      
    }
    
  })
  
  observeEvent(input$makeNetwork,{
    
    
    tryCatch({  
      

      if(is.null(values$featureTables$tables[[values$featureTables$active]]$edges) 
         || (!is.null(input$useOldEdges) && !input$useOldEdges)){
    
        withProgress(message = 'Please wait!', detail = "Saving changes to Feature Table", value = 0, {
          
        METABOseek:::TableUpdateChunk()
        values$featureTables$tables[[values$featureTables$active]] <- updateFeatureTable(values$featureTables$tables[[values$featureTables$active]],data.frame(fixed__id = seq(nrow(values$featureTables$tables[[values$featureTables$active]]$df))))
        
        incProgress(0.1, detail = "Extracting MS2 scans")
        
        AllSpecLists <- lapply(makeScanlist2(values$featureTables$tables[[values$featureTables$active]]$df$MS2scans), getAllScans, values$MSData$data, removeNoise = input$noise*0.01)
        
        incProgress(0.2, detail = "Merging MS2 scans for each feature in Feature Table")
        
        
        MergedSpecs <- lapply(AllSpecLists, quickMergeMS, ppm = 0, mzdiff = input$mzdiff, removeNoise = input$noise*0.01)
        
        tempn <- length(MergedSpecs)
        
        incProgress(0.2, detail = paste0("Calculating similarity between ", tempn, " features (",(tempn*(tempn-1))/2," comparisons)."  ))
        
        
        values$featureTables$tables[[values$featureTables$active]]$edges <- makeEdges(speclist = MergedSpecs,
                                                                                      mztol = input$mzdiff,
                                                                                      parentmasses = if(input$useparentmasses){values$featureTables$tables[[values$featureTables$active]]$df$mz}else{NULL},
                                                                                      minpeaks = 6)
        
        #let's remove edges with cosine ~0 
        values$featureTables$tables[[values$featureTables$active]]$edges <- values$featureTables$tables[[values$featureTables$active]]$edges[values$featureTables$tables[[values$featureTables$active]]$edges$cosine > 0.001,]
        
        
      })
      }
      removeModal()
      
      showModal(
        modalDialog(
          fluidPage(
            fluidRow(
              p(strong("Finish Network assembly")),
              p("Give a name to the new network:"),
              textInput(ns('NetName2'), "Network name", value = paste0("Custom_Network_", length(names(internalValues)) )),
              if(length(names(internalValues)) > 1){
              p(strong("Note:"),paste0("Network names that already exist (will be overriden if you use the same name): ", paste(names(internalValues)[names(internalValues) != "numNetworks"], collapse = ", ")))
              }else{p("")}
                
                ),
            hr(),
            fluidRow(
              p("Hint: You can remove edges from this network later."),
              div( title = "Remove edges with cosine below this threshold: ", 
                   numericInput(ns("cosThresh"), "Cosine threshold", value = 0.8))
              
            ),
            fluidRow(
              htmlOutput((ns("edgeInfo")))
            ),
            fluidRow(
              div( title = "Search MS2 scans", 
                   actionButton(ns("makeNetwork2"), "Make Network"))
            )
        ),
        title = "Search MS2 scans",
        easyClose = T,
        fade = F,
        size = "l",
        footer = modalButton("Cancel") 
      ))
    
    
    },
    error = function(e){
      #print("graph not loaded")
      #print(e)
      showModal(
        modalDialog(title = "An error has occured",
                    "Make sure MS2 data for the current feature table is loaded into Mseek",
                    hr(),
                    p(strong("Error:")),
                    p(paste(e, collapse = "\n")),
                    easyClose = T
        )
      )
    }
    
  )
      
   
  })

observeEvent(input$makeNetwork2,{
  
  tryCatch({  
    #print("Making Network")
    #originally preferred: Node ids are indices in feature table
  #  tempnodes <- values$featureTables$tables[[values$featureTables$active]]$df[ values$featureTables$tables[[values$featureTables$active]]$df$MS2scans != "", colnames(values$featureTables$tables[[values$featureTables$active]]$df)!= "id"]
   # internalValues[[gsub("\\.[^.]*$","",input$NetName2)]]$tables$nodes <- cbind(data.frame(id = which(values$featureTables$tables[[values$featureTables$active]]$df$MS2scans != ""), tempnodes))
    #internalValues[[gsub("\\.[^.]*$","",input$NetName2)]]$tables$edges <- values$featureTables$tables[[values$featureTables$active]]$edges[values$featureTables$tables[[values$featureTables$active]]$edges$cosine >= input$cosThresh,]
    
    #now renumbering node IDs and edge from/ to so that they start from 1:
    tempnodes <- values$featureTables$tables[[values$featureTables$active]]$df[ values$featureTables$tables[[values$featureTables$active]]$df$MS2scans != "", colnames(values$featureTables$tables[[values$featureTables$active]]$df)!= "id" ]
   # print(nrow(tempnodes))
   #print(input$NetName2)
   internalValues[[gsub("\\.[^.]*$","",input$NetName2)]]$tables$nodes <- data.frame(id = seq(nrow(tempnodes)), tempnodes)
   # print("got here")
    
    tempedges <- values$featureTables$tables[[values$featureTables$active]]$edges[values$featureTables$tables[[values$featureTables$active]]$edges$cosine >= input$cosThresh,]
    
    tempedges$from <- sapply(tempedges$from, match, tempnodes$fixed__id)
    tempedges$to <- sapply(tempedges$to, match, tempnodes$fixed__id)
    
    
    internalValues[[gsub("\\.[^.]*$","",input$NetName2)]]$tables$edges <- na.omit(tempedges)
    
    
    
    g1 <- graph_from_data_frame(d=internalValues[[gsub("\\.[^.]*$","",input$NetName2)]]$tables$edges, vertices=internalValues[[gsub("\\.[^.]*$","",input$NetName2)]]$tables$nodes, directed=F) 
    
    
    #V(g1)$label <- V(g1)$parent.mass
    
    #Weird that this is not automatically taken from the original df like all other columns:
    V(g1)$id <- internalValues[[gsub("\\.[^.]*$","",input$NetName2)]]$tables$nodes$id
    
    # Removing loops from the graph:
    g1 <- simplify(g1, remove.multiple = F, remove.loops = T) 
    
    #important for overview mode!
    V(g1)$subcl <-  clusters(g1)$membership
    internalValues[[gsub("\\.[^.]*$","",input$NetName2)]]$graph <- g1
    
    internalValues$numNetworks <- internalValues$numNetworks + 1
   # print(internalValues$numNetworks) 
    removeModal()
  },
  error = function(e){
    #print("graph not loaded")
    print(e)
    removeModal()
    
    showModal(
      modalDialog(title = "An error has occured",
                  "Make sure MS2 data for the current feature table is loaded into Mseek",
                  hr(),
                  p(strong("Error:")),
                  p(paste(e, collapse = "\n")),
                  easyClose = T
      )
    )
  }
  
  )
})

observeEvent(input$loadNetworkModal,{
  
  showModal(
    modalDialog(
  fluidPage(
    fluidRow(
      column(4,
             UploadTableModuleUI(ns('loadNodeTab'))),
      column(4,
             UploadTableModuleUI(ns('loadEdgeTab'))),
      column(4,
             style = "margin-top: 45px;",
             # p("Load with automatic presets"),
             tags$div(title = "Load a network file.",
                      fileInput(ns('networkFileLoad'),"Load  network (.graphml)", accept = NULL)))),
    fluidRow(
      column(8,
             textInput(ns('NetName'), "Network name", value = "Custom_Network_1")),
      column(4,
             style = "margin-top: 25px;",
             tags$div(title = "Load network from custom tables. Requires both a node and an edge table to be loaded.",
                      actionButton(ns("loadNetwork"), "Load Network"))
      )
    )
    ),
  title = "Load a network",
  easyClose = T,
  fade = F,
  size = "l",
  footer = modalButton("Cancel") 
    ))
  
  
  
})



observe({
  shinyjs::toggleState(id = "loadNetwork",condition = (!is.null(loadNodeTab$df) && !is.null(loadEdgeTab$df)))
})

return(internalValues)
}
#' LoadNetworkModuleUI
#' 
#' UI module for load networking module
#' 
#' 
#' @export
LoadNetworkModuleUI <-  function(id){
  ns <- NS(id)
    fluidRow(
      column(2,
             div(title = "Load a network",
             actionButton(ns("loadNetworkModal"), "", icon = icon("folder-open", lib = "font-awesome"))
             )
             ),
      column(2,
             SaveNetworkModuleUI(ns("savenetworks"))
             ),
      column(8,
             FindMS2ScansModuleUI(ns('findms2network')))
    )
    }