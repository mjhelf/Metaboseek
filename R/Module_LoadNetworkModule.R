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
LoadNetworkModule <- function(input,output, session, tag, set = list(allowGNPS =  T)){
  
  ns <- NS(tag)
  
  dataSets <- reactiveValues(
    graphs = list()
  )
  
  
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
  
  #load and reformat a gnps network from the zip file
  observeEvent(input$GNPS_Load$datapath,{
    
    exfolder = file.path(dirname(input$GNPS_Load$datapath), gsub("\\.[^.]*$","",input$GNPS_Load$name))
    unzip(input$GNPS_Load$datapath, exdir = exfolder )
    
    newfilesTSV <- list.files(exfolder, pattern=".tsv", recursive = TRUE, full.names=T)
    newfilesPairsinfo <- list.files(exfolder, pattern=".pairsinfo", recursive = TRUE, full.names=T)
    
    res <- list(tables = list(nodes = NULL,
                              edges = NULL),
                graph = NULL)
    
    res$tables$nodes <- read.delim(grep("withcomponentID",newfilesTSV, value = T),
                                   header= T,quote = "",  sep= '\t', dec=".", fill= T, skip=0,  stringsAsFactors = T)
    res$tables$edges <- read.delim(grep("networkedges_selfloop",newfilesPairsinfo, value = T),
                                   header= T,quote = "",  sep= '\t', dec=".", fill= T, skip=0,  stringsAsFactors = T)
    
    g1 <- graph_from_data_frame(d=res$tables$edges, vertices=res$tables$nodes, directed=F) 
    V(g1)$label <- V(g1)$parent.mass
    V(g1)$id <- seq(vcount(g1))
    
    # Removing loops from the graph:
    g1 <- simplify(g1, remove.multiple = F, remove.loops = T) 
    
    #important for overview mode!
    V(g1)$subcl <-  clusters(g1)$membership
    
    res$graph <- g1
    
    dataSets$graphs[[gsub("\\.[^.]*$","",input$GNPS_Load$name)]] <- res
    
  })
  
  #load and reformat a network from edge and node table
  observeEvent(input$loadNetwork,{
    res <- list(tables = list(nodes = NULL,
                              edges = NULL),
                graph = NULL)
    
    res$tables$nodes <- loadNodeTab$df
    res$tables$edges <- loadEdgeTab$df
    
    tryCatch({  
      g1 <- graph_from_data_frame(d=res$tables$edges, vertices=res$tables$nodes, directed=F) 
      
      
      #V(g1)$label <- V(g1)$parent.mass
      V(g1)$id <- seq(vcount(g1))
      
      # Removing loops from the graph:
      g1 <- simplify(g1, remove.multiple = F, remove.loops = T) 
      
      #important for overview mode!
      V(g1)$subcl <-  clusters(g1)$membership
      
      res$graph <- g1
      
      dataSets$graphs[[gsub("\\.[^.]*$","",input$NetName)]] <- res
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
  
  observe({
    shinyjs::toggleState(id = "loadNetwork",condition = (!is.null(loadNodeTab$df) && !is.null(loadEdgeTab$df)))
  })
  
  return(reactive({dataSets}))
}
#' LoadNetworkModuleUI
#' 
#' UI module for load networking module
#' 
#' 
#' @export
LoadNetworkModuleUI <-  function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4,
             UploadTableModuleUI(ns('loadNodeTab'))),
      column(4,
             UploadTableModuleUI(ns('loadEdgeTab'))),
      column(4,
             style = "margin-top: 45px;",
             # p("Load with automatic presets"),
             tags$div(title = "Load a GNPS networking output file. Will automatically select a node and an edge file from the .zip file.",
                      fileInput(ns('GNPS_Load'),"Load GNPS network (.zip)", accept = "application/zip")))
    ),
    fluidRow(
      column(4,
             textInput(ns('NetName'), "Network name", value = "Custom_Network_1")),
      column(4,
             style = "margin-top: 25px;",
             tags$div(title = "Load network from custom tables. Requires both a node and an edge table to be loaded.",
                      actionButton(ns("loadNetwork"), "Load Network"))
      )
    )
    
  )}