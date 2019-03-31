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
MS2BrowserModule <- function(input,output, session, 
                             reactives = reactive({list(query = list(mz = NULL,
                                                                     rt = NULL))}),
                             values = reactiveValues(featureTables = featureTables,
                                                     MainTable = MainTable,
                                                     MSData = MSData,
                                                     GlobalOpts = GlobalOpts),
                             keys = reactive({keys()})){
  
  ns <- NS(session$ns(NULL))
  
  # 
  # dataSets <- reactiveValues(
  #   graphs = list(),
  # 
  #   spectab = NULL
  # )
  
  
  SimplifyMod <- callModule(SimplifyNetworkModule, 'simplify', values = reactiveValues(Networks = Networks),
                            reactives = reactive({list(activeNetwork = NetMod$active)}))
  
  
  observe({
    # toggle(id = 'network', condition = input$showNet, anim = T)
    #toggle(id = 'LoadNet', condition = input$showLoadNet, anim = T)
    
  })
  
  output$searchcontrol <- renderUI({
    fluidRow(
      column(3,
             div(title = "m/z tolerance for MS2 scan search (ppm)",
                 numericInput(ns('ppmSearch'), "ppm", value = 5))),
      column(3,
             div(title = "RT tolerance for MS2 scan search (seconds)",
                 
                 numericInput(ns('rtSearch'), "seconds", value = 60))
      ),
      column(3,
             ShowSiriusModuleUI(ns("showSirius"))
      ),
      column(3,
             GetSiriusModuleUI(ns("getSirius"))
      )
    )
  })
  
  output$pdfButton <- downloadHandler(filename= function(){
    
    return(paste0(NetMod$active,".pdf"))}, 
    content = function(file){
      
      pdf(file,
          14,14
      )
      
      if(!is.null(NetMod$recordedPlot)){
        replayPlot(NetMod$recordedPlot)
      }
      
      dev.off()
      
    },
    contentType = "application/pdf")
  
  
  output$network <- renderUI({
    tagList(
      box(width = 12, status= "primary",
          fluidPage(
            fluidRow(
              column(4,
                     LoadNetworkModuleUI(ns('loadnetworks'))),
              column(5,
                     fluidRow(
                       htmlOutput(ns("selectNetwork"))
                     )
              ),
              column(2,
                     fluidRow(
                       SimplifyNetworkModuleUI(ns("simplify"))                   )
              ),
              column(1,
                     if(!is.null(NetMod$recordedPlot)){
                       div(title = "Save current Network view as pdf",
                           downloadButton(ns('pdfButton'), ""))
                       
                     }else{p()}
              )
            ),
            fluidRow(
              NetworkModuleUI(ns('shownetworks')))
          )
      )
    )
  })
  
  
  
  
  output$selectNetwork <- renderUI({
    if(length(names(Networks)[names(Networks) != "numNetworks"]) >0 ){
      selectizeInput(ns('activeNetwork'), 'Active Network', 
                     selected = NetMod$active, 
                     choices = names(Networks)[names(Networks) != "numNetworks"],
                     multiple = FALSE)
    }
  })
  
  observeEvent(input$activeNetwork,{
    
    NetMod$active <- input$activeNetwork
    
  })
  
  Networks <- callModule(LoadNetworkModule, "loadnetworks", values = reactiveValues(featureTables = values$featureTables,
                                                                                    MainTable = values$MainTable,
                                                                                    MSData = values$MSData),
                         reactives = reactive({list(active = NetMod$active)}))
  
  NetMod <- callModule(NetworkModule, "shownetworks", values = reactiveValues(Networks = Networks),
                       reactives = reactive({list(active = T)}),
                       static = list(noSelection = T),
                       keys = reactive({keys()}))
  
  SaveNetworks <- callModule(SaveNetworkModule, "savenetworks",
                             reactives = reactive({list(graphname = NetMod$active,
                                                        filename = paste0("networks/",NetMod$active,".graphml"))}),
                             values = reactiveValues(Networks = Networks),
                             static = list(tooltip = "Save Network as a graphml file",
                                           label = "Save Network",
                                           format = c("graphml"))
  )
  
  
  Sirius <- callModule(SiriusModule,"sirius",
                       values = reactiveValues(
                         GlobalOpts = values$GlobalOpts))
  
  FReport <- callModule(FeatureReportModule, "freport",values = reactiveValues(MSData = values$MSData,
                                                                               MainTable = values$MainTable,
                                                                               featureTables = values$featureTables,
                                                                               GlobalOpts = values$GlobalOpts),
                        keys = reactive({keyin$keyd}))
  
  
  
  
  splashsource <- reactive({
    res <- list()
    if(!is.null(internalValues$spectab)){
      if(length(selectScan()$props$selected_rows) == 0 && !is.null(internalValues$spectab$file)){
        #print(internalValues$spectab)
        res$stab <- internalValues$spectab
        
      }else{
        res$stab <- internalValues$spectab[selectScan()$props$selected_rows,]
      }
      
      AllSpecLists <- lapply(list(res$stab), getAllScans, values$MSData$data, removeNoise = 0)
      
      res$MergedSpecs <- lapply(AllSpecLists, quickMergeMS, ppm = 0, mzdiff = 0.005, removeNoise = 0)
      
      res$splashtag <- sapply(res$MergedSpecs, getSplash)
      
    }
    else{
      res$splashtag <- NULL
    }
    return(res)
  })
  
  
  
  callModule(GetSiriusModule, "getSirius",
             values = reactiveValues(MSData = values$MSData,
                                     GlobalOpts = values$GlobalOpts),
             reactives = reactive({
               if(!is.null(splashsource()$stab) && !is.null(values$GlobalOpts$siriusFolder)){
                 list(outfolder =  file.path(values$GlobalOpts$siriusFolder,"METABOseek"),
                      ms2 = splashsource()$MergedSpecs,
                      instrument = values$GlobalOpts$SiriusSelInstrument,
                      parentmz = mean(splashsource()$stab$parentMz),
                      rt = mean(splashsource()$stab$rt),
                      comments = "",
                      ion = values$GlobalOpts$SiriusSelIon,
                      charge= if(length(grep("-$",values$GlobalOpts$SiriusSelIon))){-1}else{1},
                      fingerid = values$GlobalOpts$SiriusCheckFinger,
                      scanindices = saveScanlist(splashsource()$stab),
                      
                      sirpath = list.files(values$GlobalOpts$siriusFolder,
                                           pattern = if(Sys.info()['sysname'] == "Windows"){
                                             "^sirius-console"}else{"^sirius$"},
                                           full.names = T,
                                           recursive = T)[1],
                      
                      moreOpts = paste0("-c 50 --fingerid-db bio -e ", values$GlobalOpts$SiriusElements))
               }else{
                 
                 NULL
               }
               
             }))
  
  callModule(ShowSiriusModule, "showSirius",
             values = reactiveValues(SiriusModule = Sirius,
                                     GlobalOpts = values$GlobalOpts),
             reactives = reactive({
               list(splash = splashsource()$splashtag,
                    mz = mean(splashsource()$stab$parentMz))
               
             })
  )
  
  
  selectScan <- callModule(TableModule,'scantab', tag = ns('scantab'), set = reactive({list(df =  internalValues$spectab,
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
  
  observeEvent(NetMod$marker$vertex,{ 
    if(!is.null(NetMod$marker$vertex)){
      internalValues$spectab <- makeScanlist2(vertex_attr(NetMod$activelayout$graph,"MS2scans", NetMod$marker$vertex), MSData = values$MSData$data)[[1]]  
    }
  })
  
  observeEvent(c(input$ppmSearch,input$rtSearch,reactives()$query),{ 
    if(length(reactives()$query$mz) > 0 ){
      internalValues$spectab <- Parentsearch(values$MSData$data, mz = reactives()$query$mz, rt = reactives()$query$rt, ppm = input$ppmSearch, rtw = input$rtSearch)
      
    }
  })
  
  
  iSpec2 <- callModule(MultiSpecmodule,"Spec2", tag = ns("Spec2"),
                       set = reactive({list(spec = list(xrange = NULL,
                                                        yrange = NULL,
                                                        maxxrange = NULL,
                                                        maxyrange = NULL,
                                                        sel = if(length(selectScan()$props$selected_rows) == 0 && !is.null(internalValues$spectab$file)){
                                                          #print(internalValues$spectab)
                                                          list(File = internalValues$spectab$file,
                                                               scan = internalValues$spectab$scan,
                                                               rt = internalValues$spectab$rt)
                                                        }else{
                                                          list(File = internalValues$spectab$file[selectScan()$props$selected_rows],
                                                               scan = internalValues$spectab$scan[selectScan()$props$selected_rows],
                                                               rt = internalValues$spectab$rt[selectScan()$props$selected_rows])
                                                        },
                                                        data = NULL,
                                                        mz =  if(length(selectScan()$props$selected_rows) == 0 && !is.null(internalValues$spectab$file)){
                                                          mean(internalValues$spectab$parentMz)}else{
                                                            internalValues$spectab$parentMz[selectScan()$props$selected_rows[1]]},
                                                        MS2 = T),
                                            layout = list(lw = 1,
                                                          cex = 1.5,
                                                          controls = F,
                                                          ppm = 5,
                                                          active = if(!is.null(internalValues$spectab) 
                                                                      #&& !is.null(selectScan()$props$selected_rows)
                                                          ){T}else{F},
                                                          highlights = NULL,
                                                          height = 350),
                                            msdata = values$MSData$data)
                       }),
                       keys = reactive({keys()}),
                       static = list(title = "MS2 spectra")
  )
  
  internalValues <- reactiveValues(iSpec2 = iSpec2,
                                   selectScan = selectScan,
                                   NetMod = NetMod,
                                   Networks = Networks,
                                   spectab = NULL,
                                   SimplifyMod = SimplifyMod)
  
  #control highlights in network here
  observeEvent(values$MainTable$selected_rows,{
    #print("i see")
    tryCatch({
      if(!is.null(NetMod$hoverActive) 
         && NetMod$hoverActive 
         && !is.null(values$MainTable$selected_rows)
         && !is.null(NetMod$activelayout$graph)){
        #print("A")       
        # print(vertex_attr(NetMod$activelayout$graph,"fixed__id"))
        
        if(!is.null(values$featureTables$tables[[values$featureTables$active]]$df$fixed__id)){
          
          
          if(is.numeric(vertex_attr(NetMod$activelayout$graph,"fixed__id"))){
            sel <- which(vertex_attr(NetMod$activelayout$graph,"fixed__id") == values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$MainTable$liveView[values$MainTable$selected_rows[1],]), "fixed__id"])
            
          }else{
            
            # note: \\b looks for word boundaries which can be whitespace or beginning/end of strings. Useful!
            
            sel <- grep(paste0("\\b",values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$MainTable$liveView[values$MainTable$selected_rows[1],]), "fixed__id"],"\\b"), vertex_attr(NetMod$activelayout$graph,"fixed__id"))
            
          }
          
          
        }else{
          
          vmzs <- vertex_attr(NetMod$activelayout$graph,"mz")
          vrts <- vertex_attr(NetMod$activelayout$graph,"rt")
          
          
          sel <- which(vmzs <= values$MainTable$liveView[values$MainTable$selected_rows[1],"mz"]+1e-6*values$MainTable$liveView[values$MainTable$selected_rows[1],"mz"]*values$GlobalOpts$PPMwindow
                       & vmzs >= values$MainTable$liveView[values$MainTable$selected_rows[1],"mz"]-1e-6*values$MainTable$liveView[values$MainTable$selected_rows[1],"mz"]*values$GlobalOpts$PPMwindow
                       & vrts <= values$MainTable$liveView[values$MainTable$selected_rows[1],"rt"] + values$GlobalOpts$RTwindow
                       & vrts >= values$MainTable$liveView[values$MainTable$selected_rows[1],"rt"]- values$GlobalOpts$RTwindow                                 )
          
          #  print("selected by mz+rt")
          
        }
        
        NetMod$highlights <- sel
        
        
      }
    },
    error = function(e){print(e)})
    
  })
  
  
  #control actual selection of nodes in network here
  
  
  return(internalValues)
  
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
      tabBox(title = "MS2 Browser",
             id = ns("MS2Tabs"), width = 12, side = "left", selected = "Feature Report",
             # tabsetPanel(id = ns("MS2Tabs"),
             tabPanel("_"),
             tabPanel("Feature Report",
                      fluidPage(
                        fluidRow(
                          FeatureReportModuleUI(ns("freport")) 
                        ),
                        fluidRow(
                          SiriusModuleUI(ns("sirius"))
                        ))
             ),
             tabPanel("Compare MS2",
                      fluidPage(
                        fluidRow(
                          column(7,
                                 htmlOutput(ns("network"))),
                          column(5,
                                 box(width = 12, status= "primary",
                                     MultiSpecmoduleUI(ns('Spec2'))))
                        ))
             )
             
      )),
    
    
    fluidRow(
      box(width = 12, status= "primary",
          
          fluidPage(
            
            htmlOutput(ns("searchcontrol")),
            fluidRow(
              TableModuleUI(ns('scantab'))
            )
          )
          
          
      )
    )
  )
}
