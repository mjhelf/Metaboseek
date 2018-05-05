#' MS2BrowserModule
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
MS2BrowserModule <- function(input,output, session, tag, set = list(MSData =  xcmsRaws,
                                                                    query = list(mz = NULL,
                                                                                 rt = NULL)),
                             keys = reactive({keys()})){
  
  ns <- NS(tag)
  
  
  dataSets <- reactiveValues(
    graphs = list(),
    activeGraph = NULL,
    
    spectab = NULL
  )
  
  
  Graphloader <- callModule(LoadNetworkModule, 'Graphloader', tag = ns('Graphloader'), set = reactive({ list(allowGNPS =  T)}))
  
  
  observe({
    toggle(id = 'network', condition = input$showNet, anim = T)
    toggle(id = 'LoadNet', condition = input$showLoadNet, anim = T)
    
  })
  
  output$showNet <- renderUI({
    fluidRow(
      column(3,
             h4("Parent m/z search options:")),
      column(3,
             numericInput(ns('ppmSearch'), "m/z tolerance (ppm)", value = 5)),
      column(3,
             numericInput(ns('rtSearch'), "RT tolerance (seconds)", value = 60)),
      column(3,
             checkboxInput(ns('showNet'), "Show network options", value = F)
      )
    )
  })
  
  
  output$network <- renderUI({
    fluidPage(
      fluidRow(
        NetworkModuleUI(ns('net1'))),
      fluidRow(
        column(3,
               #style = "margin-top: 25px;",
               checkboxInput(ns('showLoadNet'), "Load network menu", value = T)),
        column(9,
               htmlOutput(ns('selectNetwork'))
        )),
      
      htmlOutput(ns('LoadNet'))
    )
  })
  
  
  output$LoadNet <- renderUI({
    fluidRow(
      LoadNetworkModuleUI(ns('Graphloader')))
    
  })
  
  output$selectNetwork <- renderUI({
    if(length(names(Graphloader()$graphs)) >0 ){
      selectizeInput(ns("selectNetwork"), "Select Network",
                     choices = names(Graphloader()$graphs),
                     selected = dataSets$activeGraph,
                     width = '100%')
    }
  })
  
  observeEvent(input$selectNetwork,{
    
    dataSets$activeGraph <- input$selectNetwork
    
  })
  
  
  netout <- callModule(NetworkModule, 'net1', tag = ns('net1'), set = reactive({list(net = list(xrange = NULL,
                                                                                                yrange = NULL,
                                                                                                maxxrange = NULL,
                                                                                                maxyrange = NULL,
                                                                                                sel = NULL,
                                                                                                mz = NULL,
                                                                                                data = if(length(dataSets$activeGraph)>0){Graphloader()$graphs[[dataSets$activeGraph]]$graph}else{NULL},
                                                                                                tables = if(length(dataSets$activeGraph)>0){Graphloader()$graphs[[dataSets$activeGraph]]$tables}else{NULL}
  ),
  layout = list(lw = 1,
                cex = 1,
                controls = F,
                ppm = 5,
                active = T))}),
  keys = reactive({keys()})
  )
  
  
  
  selectScan <- callModule(TableModule,'scantab', tag = ns('scantab'), set = reactive({list(df =  dataSets$spectab,
                                                                                            update = NULL,
                                                                                            layout = list(
                                                                                              perpage = NULL,
                                                                                              height = 350,
                                                                                              readOnly = T,
                                                                                              contextMenu = F,
                                                                                              fixedColumnsLeft = 0,
                                                                                              format = list(col = c("parentMz"),
                                                                                                            format = "0.00000"),
                                                                                              invertReadOnly = NULL
                                                                                            ))})
  )
  
  observeEvent(netout()$net$marker$vertex,{ 
    if(!is.null(netout()$net$marker$vertex)){
      dataSets$spectab <- makeScanlist(vertex_attr(netout()$net$activelayout$graph,"AllFiles", netout()$net$marker$vertex), MSData = set()$MSData)  
    }
  })
  
  observeEvent(c(input$ppmSearch,input$rtSearch,set()$query),{ 
    if(length(set()$query$mz) > 0 ){
      dataSets$spectab <- Parentsearch(set()$MSData, mz = set()$query$mz, rt = set()$query$rt, ppm = input$ppmSearch, rtw = input$rtSearch)
      
    }
  })
  
  
  iSpec2 <- callModule(MultiSpecmodule,"Spec2", tag = ns("Spec2"),
                       set = reactive({list(spec = list(xrange = NULL,
                                                        yrange = NULL,
                                                        maxxrange = NULL,
                                                        maxyrange = NULL,
                                                        sel = if(length(selectScan()$props$selected_rows) == 0 && !is.null(dataSets$spectab$file)){
                                                          #print(dataSets$spectab)
                                                          list(File = dataSets$spectab$file,
                                                               scan = dataSets$spectab$scan,
                                                               rt = dataSets$spectab$rt)
                                                        }else{
                                                          list(File = dataSets$spectab$file[selectScan()$props$selected_rows],
                                                               scan = dataSets$spectab$scan[selectScan()$props$selected_rows],
                                                               rt = dataSets$spectab$rt[selectScan()$props$selected_rows])
                                                        },
                                                        data = NULL,
                                                        mz =  if(length(selectScan()$props$selected_rows) == 0 && !is.null(dataSets$spectab$file)){
                                                          mean(dataSets$spectab$parentMz)}else{
                                                            dataSets$spectab$parentMz[selectScan()$props$selected_rows[1]]},
                                                        MS2 = T),
                                            layout = list(lw = 1,
                                                          cex = 1.5,
                                                          controls = F,
                                                          ppm = 5,
                                                          active = if(!is.null(dataSets$spectab) 
                                                                      #&& !is.null(selectScan()$props$selected_rows)
                                                          ){T}else{F},
                                                          highlights = NULL,
                                                          height = 350),
                                            msdata = set()$MSData)
                       }),
                       keys = reactive({keys()}),
                       static = list(title = "MS2 spectra")
  )
  
  
  
  
  
}

#' MS2BrowserModule
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
MS2BrowserModuleUI <-  function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
             htmlOutput(ns('showNet')),
             fluidRow(
               
               TableModuleUI(ns('scantab'))
             ),
             fluidRow(
               htmlOutput(ns("network")))
      ),
      column(6,
             fluidRow(
               MultiSpecmoduleUI(ns('Spec2'))
             )
      )
    )
  )
}
