#' MS2BrowserModule
#' 
#' Module for browsing and comparing MS2 data
#' 
#' @inherit MseekModules
#' @param keys \code{reactive({})} that reports the current key press
#' 
#' @describeIn MS2BrowserModule Server logic
#' 
#' @export 
MS2BrowserModule <- function(input,output, session, 
                             values,
                             keys = reactive({"NO"})){
  
  ns <- NS(session$ns(NULL))

  ###TODO This module needs to be rearranged, submodules to bundle most of sirius and network functionality
  
  
  ###core functionality
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
  observeEvent(c(input$ppmSearch,input$rtSearch,values$featureTables$Maintable$selected_rows),{ 
      if(length(values$featureTables$Maintable$selected_rows) > 0 ){
          internalValues$spectab <- Parentsearch(values$MSData$data,
                                                 mz = values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"],
                                                 rt = values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"rt"],
                                                 ppm = input$ppmSearch,
                                                 rtw = input$rtSearch)
          
      }
  }, ignoreInit = T)
  
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
             GetSiriusWidgetUI(ns("getSirius"))
      )
    )
  })
  
  ###MAKE SPECTRUM FROM selected scans, to be used multiple times, for SIRIUS as well as plotting
  specEngine <- reactive({
      
      moreArgs <- list(k = 20)
      
      sel <- if(length(selectScan()$props$selected_rows) == 0 && !is.null(internalValues$spectab$file)){
          #print(internalValues$spectab)
          list(File = internalValues$spectab$file,
               scan = internalValues$spectab$scan,
               rt = internalValues$spectab$rt)
      }else if (!is.null(internalValues$spectab$file)){
          list(File = internalValues$spectab$file[selectScan()$props$selected_rows],
               scan = internalValues$spectab$scan[selectScan()$props$selected_rows],
               rt = internalValues$spectab$rt[selectScan()$props$selected_rows])
      }else{
          NULL
      }
      
      
      if(length(Sirius$activeMF)>0 
         && !is.null(Sirius$activeMF[["trees_json"]]) 
         && length(Sirius$activeMF[["trees_json"]]$fragments)> 0){
          
          fragments <- Sirius$activeMF[["trees_json"]]
          
          inttemp <- sapply(fragments$fragments,function(x){x$relativeIntensity})
          mztemp <- sapply(fragments$fragments,function(x){x$mz})
          labs <- paste0(format(round(mztemp,5),nsmall = 5, scientific = F), " (", sapply(fragments$fragments,function(x){x$molecularFormula}), ")")
          
          
          if(any(inttemp>0)){
              moreArgs$labels <- data.frame(x = mztemp[inttemp>=0.02],
                                            y = inttemp[inttemp>=0.02]*100,
                                            label = labs[inttemp>=0.02],
                                            stringsAsFactors = F)
          }
          
      }
      
      list(spec = list(xrange = NULL,
                       yrange = NULL,
                       maxxrange = NULL,
                       maxyrange = NULL,
                       sel = sel,
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
           msdata = values$MSData$data,
           moreArgs = moreArgs)
      
      
  })
  
  iSpec2 <- callModule(MultiSpecmodule,"Spec2", tag = ns("Spec2"),
                       set = specEngine,
                       keys = reactive({keys()}),
                       static = list(title = "MS2 spectra")
  )
  
  
  SpecView1 <- callModule(SpecModule2, "specview1",
                          reactives = reactive({
                              if(is.null(specEngine()$spec$sel)
                                 || !length(specEngine()$spec$sel$scan)){NULL}else{
                                     return(list(scantable = data.frame(file = specEngine()$spec$sel$File,
                                                                        scan = specEngine()$spec$sel$scan,
                                                                        stringsAsFactors = F),
                                                 type = "ms2"))
                                 }
                          }),
                          values = values)
  
  
  
  FReport <- callModule(FeatureReportModule, "freport",values = reactiveValues(MSData = values$MSData,
                                                                               featureTables = values$featureTables,
                                                                               GlobalOpts = values$GlobalOpts),
                        MS2feed = specEngine,
                        tree = reactive({if(length(Sirius$activeMF)>0){Sirius$activeMF[["trees_dot"]]}else{NULL}}),
                        fragments = reactive({if(length(Sirius$activeMF)>0){Sirius$activeMF[["trees_json"]]}else{NULL}}))
  
  
  ####NETWORK RELATED
  Networks <- callModule(LoadNetworkModule, "loadnetworks", values = reactiveValues(featureTables = values$featureTables,
                                                                                    MSData = values$MSData,
                                                                                    projectData = values$projectData),
                         reactives = reactive({list(active = NetMod$active)}))
  
  NetMod <- callModule(NetworkModule, "shownetworks", values = reactiveValues(Networks = Networks),
                       reactives = reactive({list(active = T)}),
                       static = list(noSelection = T),
                       keys = reactive({keys()}))
  
  
  SimplifyMod <- callModule(SimplifyNetworkModule, 'simplify', values = reactiveValues(Networks = Networks),
                            reactives = reactive({list(activeNetwork = NetMod$active)}))
  
  
  SaveNetworks <- callModule(SaveNetworkModule, "savenetworks",
                             reactives = reactive({list(graphname = NetMod$active,
                                                        filename = paste0("networks/",NetMod$active,".graphml"))}),
                             values = reactiveValues(Networks = Networks),
                             static = list(tooltip = "Save Network as a graphml file",
                                           label = "Save Network",
                                           format = c("graphml"))
  )
  
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
  
  observeEvent(NetMod$marker$vertex,{ 
      if(!is.null(NetMod$marker$vertex)){
          internalValues$spectab <- makeScanlist2(vertex_attr(NetMod$activelayout$graph,"MS2scans", NetMod$marker$vertex), MSData = values$MSData$data)[[1]]  
      }
  })
  
  #control highlights in network here
  observeEvent(values$featureTables$Maintable$selected_rows,{
      tryCatch({
          if(!is.null(NetMod$hoverActive) 
             && NetMod$hoverActive 
             && !is.null(values$featureTables$Maintable$selected_rows)
             && !is.null(NetMod$activelayout$graph)){
              
              if(!is.null(values$featureTables$tables[[values$featureTables$active]]$df$fixed__id)){
                  
                  
                  if(is.numeric(vertex_attr(NetMod$activelayout$graph,"fixed__id"))){
                      sel <- which(vertex_attr(NetMod$activelayout$graph,"fixed__id") == values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],]), "fixed__id"])
                      
                  }else{
                      
                      # note: \\b looks for word boundaries which can be whitespace or beginning/end of strings. Useful!
                      
                      sel <- grep(paste0("\\b",values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],]), "fixed__id"],"\\b"), vertex_attr(NetMod$activelayout$graph,"fixed__id"))
                      
                  }
                  
                  
              }else{
                  
                  vmzs <- vertex_attr(NetMod$activelayout$graph,"mz")
                  vrts <- vertex_attr(NetMod$activelayout$graph,"rt")
                  
                  
                  sel <- which(vmzs <= values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]+1e-6*values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]*values$GlobalOpts$PPMwindow
                               & vmzs >= values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]-1e-6*values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]*values$GlobalOpts$PPMwindow
                               & vrts <= values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"rt"] + values$GlobalOpts$RTwindow
                               & vrts >= values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"rt"]- values$GlobalOpts$RTwindow                                 )
                  
                  #  print("selected by mz+rt")
                  
              }
              
              NetMod$highlights <- sel
              
              
          }
      },
      error = function(e){print(e)})
      
  })
  
######SIRIUS RELATED
    
  Sirius <- callModule(SiriusModule,"sirius",
                       values = reactiveValues(
                         GlobalOpts = values$GlobalOpts))
  
  splashsource <- reactive({
    res <- list()
    if(!is.null(internalValues$spectab)){
      if(length(selectScan()$props$selected_rows) == 0 && !is.null(internalValues$spectab$file)){
        #print(internalValues$spectab)
        res$stab <- internalValues$spectab
        
      }else{
        res$stab <- internalValues$spectab[selectScan()$props$selected_rows,]
      }
      
      res$AllSpecLists <- lapply(list(res$stab), getAllScans, values$MSData$data, removeNoise = 0)
      
      names(res$AllSpecLists[[1]]) <- paste0("collision",
                                        getScanInfo(basename(res$stab$file),
                                                    res$stab$scan,
                                                    data = values$MSData$MSnExp,
                                                    type = "ms2")$collisionEnergy)
      
     # MergedSpecs <- lapply(AllSpecLists, quickMergeMS, ppm = 0, mzdiff = 0.005, removeNoise = 0)
      
      res$splashtag <- digest(res$AllSpecLists[[1]], algo = "xxhash64")
      
      #print(paste("spechash:",res$splashtag))
      #res$splashtag <- sapply(res$MergedSpecs, getSplash)
      
    }
    else{
      res$splashtag <- NULL
    }
    return(res)
  })
  
  
  observeEvent(specEngine(),{
      #get ONE corresponding MS1 scan 
      
      if(!is.null(specEngine()$spec) 
         && values$GlobalOpts$SiriusUseMS1 
         && !is.null(specEngine()$spec$sel) 
         && length(specEngine()$spec$sel$File) > 0
         &&!is.null(values$MSData$data)){
          targets <- specEngine()$spec$sel
          
          
          ms1targets <- list(File = targets$File)
          
          ms1targets$scan <- sapply(seq(length(targets$File)),function(n){ which.min(abs(values$MSData$data[[which(basename(names( values$MSData$data)) == targets$File[n])]]@scantime - targets$rt[n]))})
          
          
          internalValues$ms1 <- getScan(values$MSData$data[[which(basename(names(values$MSData$data)) == ms1targets$File[1])[1]]],
                         ms1targets$scan[1],
                         mzrange = range(c(mean(splashsource()$stab$parentMz)-3,mean(splashsource()$stab$parentMz)+7)))
          
          internalValues$ms1splash <- digest(internalValues$ms1,algo = "xxhash64")
          
      }else{
          internalValues$ms1splash <- ""
          internalValues$ms1 <- NULL
          
      }
      
      }, ignoreNULL = FALSE)
  
  
  callModule(GetSiriusWidget, "getSirius",
             reactives = reactive({
               
               
               
               
               if(!is.null(splashsource()$stab) 
                  && !is.null(values$GlobalOpts$siriusFolder)){
                 

                 
                 list(outfolder =  file.path(values$GlobalOpts$siriusFolder,"Metaboseek"),
                      ms2 = splashsource()$AllSpecLists,
                      ms1 = list(internalValues$ms1),
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
                      
                      moreOpts = paste0("-c 50 ",
                                        if(!is.null(values$GlobalOpts$SiriusCheckFinger) 
                                            && values$GlobalOpts$SiriusCheckFinger){paste0("--fingerid-db ", values$GlobalOpts$SiriusDBselected," -e ")}else{"-e "},
                                        values$GlobalOpts$SiriusElements))
               }else{
                 
                 NULL
               }
               
             }))
  
  callModule(ShowSiriusModule, "showSirius",
             values = reactiveValues(SiriusModule = Sirius,
                                     GlobalOpts = values$GlobalOpts),
             reactives = reactive({
               list(splash = splashsource()$splashtag,
                    ms1splash = internalValues$ms1splash,
                    mz = mean(splashsource()$stab$parentMz))
               
             })
  )
  
  

 ####TODO: remove return and simplify internalValues
  internalValues <- reactiveValues(iSpec2 = iSpec2,
                                   selectScan = selectScan,
                                   NetMod = NetMod,
                                   Networks = Networks,
                                   spectab = NULL,
                                   SimplifyMod = SimplifyMod)
  
  return(internalValues)
  
}

#' @describeIn MS2BrowserModule UI elements
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
                        )
                        
                        
                        )
             )
             
      )),
   
    fluidRow(
        SiriusModuleUI(ns("sirius"))
    ),
    
    fluidRow(
      box(width = 12, status= "primary",
          
          fluidPage(
            
            
            fluidRow(
              column(6,
                htmlOutput(ns("searchcontrol")),
              TableModuleUI(ns('scantab'))),
              column(6,
                     SpecModule2UI(ns("specview1"))                
)
            )
          )
          
          
      )
    )
  )
}
