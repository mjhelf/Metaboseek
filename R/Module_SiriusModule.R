#' SiriusModule
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' 
#' @importFrom shinyBS bsCollapse bsCollapsePanel updateCollapse
#' @importFrom data.table fread
#' 
#' @export 
SiriusModule <- function(input,output, session, 
                          values = reactiveValues(
                                                  GlobalOpts = GlobalOpts)){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(siriusIndex = NULL,
                                   activeSirius = NULL,
                                   activeMF = NULL,
                                   activeStructure = NULL,
                                   
                                   selCharge = 1,
                                   selIon = "",
                                   checkFinger = T,
                                   selInstrument = "orbitrap",
                                   elements = "CHNOP[5]S",
                                   
                                   grabSettingsTrigger = T,
                                   
                                   siriusJob = NULL,
                                   quickLookup = character(0),
                                   depictor = if(.MseekOptions$rcdk.installed){rcdk::get.depictor(width = 550, height = 550, zoom = 1.3, style = "cow", 
                                                                                                annotate = "off", abbr = "on", suppressh = TRUE, 
                                                                                                showTitle = FALSE, smaLimit = 100, sma = NULL) }else{NULL}
                                   )
  
  
  observe({
    if(!is.null(values$GlobalOpts$siriusFolder)){
    internalValues$siriusIndex <- tryCatch({
      as.data.frame(reactiveFileReader(1500,
                                    NULL,
                                    file.path(values$GlobalOpts$siriusFolder,"METABOseek", "index.csv"),
                                    fread,
                                    stringsAsFactors = F)(), stringsAsFactors = F)
      },
                                    error = function(e){
                                      return(NULL)
                                      })
}
  })
  
  # observeEvent(internalValues$siriusIndex,{
  #   if(is.data.frame(internalValues$siriusIndex)  && nrow(internalValues$siriusIndex) > 0){
  #     
  #     tryCatch({
  #   internalValues$quickLookup <- paste(round(internalValues$siriusIndex$mz,4),
  #      apply(internalValues$siriusIndex[,c("splash", "ion", "fingerid", "moreOpts", "METABOseek_sirius_revision")],
  #                                       1, paste, collapse = "//"), sep = "//")
  #     },
  #   error = function(e){print(e); return(character(0))})
  #   
  #   }
  #   else{
  #     character(0)
  #   }
  #   
  # })
 
  SirBrowser <- callModule(TableModule2, "sirbrowser",
                           reactives = reactive({list(df = if(is.null(internalValues$siriusIndex)){NULL}else{internalValues$siriusIndex},
                                                      rowFilters = NULL,
                                                      colFilters = NULL)}),
                           values = reactiveValues(),
                           static = list(perpage = NULL,
                                         height = 200,
                                         sort = F,
                                         readOnly = T,
                                         contextMenu = F,
                                         fixedColumnsLeft = 2,
                                         invertReadOnly = NULL,
                                         format = list(col = NULL,
                                                       format = NULL))
  )
  
  observeEvent(SirBrowser$selected_rows,{

    if(!is.null(SirBrowser$selected_rows) && !is.null(SirBrowser$liveView)){

      tryCatch({
              internalValues$activeSirius <- getSirius(file.path(values$GlobalOpts$siriusFolder, "METABOseek"),
                                                       splash = SirBrowser$liveView[SirBrowser$selected_rows[1],"splash"],
                                                       ts = SirBrowser$liveView[SirBrowser$selected_rows[1],"timestamp"])

      },
      error =  function(e){

        internalValues$activeSirius <- NULL

      })
      internalValues$activeMF <- NULL
      internalValues$activeStructure <- NULL
      }

  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  MFbrowser <- callModule(TableModule2, "mfbrowser",
                          reactives = reactive({


                            list(df =  if(!is.null(internalValues$activeSirius)){internalValues$activeSirius$summary_expanded}else{NULL},
                                                     rowFilters = NULL,
                                                     colFilters = NULL)


                            }),
                          values = reactiveValues(),
                          static = list(perpage = NULL,
                                        height = 300,
                                        sort = F,
                                        readOnly = T,
                                        contextMenu = F,
                                        fixedColumnsLeft = 1,
                                        invertReadOnly = NULL,
                                        format = list(col = NULL,
                                                      format = NULL))
  )

  observeEvent(MFbrowser$selected_rows,{

    if(!is.null(MFbrowser$selected_rows) && !is.null(MFbrowser$liveView)){

      internalValues$activeMF <- getSiriusTree(internalValues$activeSirius,MFbrowser$liveView[MFbrowser$selected_rows,"formula"])
    }

  })



  StructureBrowser <- callModule(TableModule2, "structurebrowser",
                          reactives = reactive({


                            list(df =  if(!is.null(internalValues$activeSirius)){internalValues$activeSirius$summary_fingerid_expanded[,c("rank", "molecularFormula", "score", "name", "pubchemids", "smiles")]}else{NULL},
                                 rowFilters = NULL,
                                 colFilters = NULL)


                          }),
                          values = reactiveValues(),
                          static = list(perpage = NULL,
                                        height = 300,
                                        sort = F,
                                        readOnly = T,
                                        contextMenu = F,
                                        fixedColumnsLeft = 1,
                                        invertReadOnly = NULL,
                                        format = list(col = NULL,
                                                      format = NULL))
  )

  observeEvent(StructureBrowser$selected_rows,{

    if(!is.null(StructureBrowser$selected_rows) && !is.null(StructureBrowser$liveView)){

      internalValues$activeStructure <- internalValues$activeSirius$summary_fingerid_expanded[row.names(StructureBrowser$liveView)[StructureBrowser$selected_rows][1],]

    }

  })

  output$siriusTreePlot <-  DiagrammeR::renderGrViz({
      
      if(!is.null(internalValues$activeMF) && !is.null(internalValues$activeMF[["trees_dot"]]) ){
      
      internalValues$activeMF[["trees_dot"]]
      
  }  })



#   output$fingerIDwindow <-  renderUI({
#     if(!is.null(internalValues$activeStructure)){
# 
#       pcid <- internalValues$activeStructure$pubchemids
#       if(length(grep(";", pcid))>0){
# 
#        pcid <-  strsplit(pcid,";")[[1]][1]
# 
#       }
# 
#     HTML('
# <iframe id="inlineFramePubchem"
#          title="webpage"
#          style="border:none;width:100%;height:500px;" ',
#          paste0('src="https://pubchem.ncbi.nlm.nih.gov/compound/', pcid,'">'),
#          '</iframe>
#          ')
#     }
#   })
  
  
  output$selectSirius <- renderUI({
    
    bsCollapse(id = ns("collapseSiriusSearch"), open = NULL,
               bsCollapsePanel("Browse SIRIUS searches",
                               if(!is.null(internalValues$siriusIndex)){
                                 
                                 
                                 TableModule2UI(ns("sirbrowser"))
                                 
                                 
                               }else{
                                 
                                 
                                 p("No SIRIUS results available")
                                 
                                 
                               })
                               )
    
    
    
  })
  
  output$tablewrap <- renderUI({fluidRow(
      column(6,
             TableModule2UI(ns("mfbrowser"))),
      column(6,
             TableModule2UI(ns("structurebrowser")))
  )})
  
  output$selectResults <- renderUI({
    
    if(!is.null(internalValues$activeSirius)){
      
      selitem <- internalValues$siriusIndex[internalValues$siriusIndex$splash == internalValues$activeSirius$splash 
                                            & internalValues$siriusIndex$timestamp == internalValues$activeSirius$timestamp,]
      
      tagList(
        bsCollapse(id = ns("collapseSiriusMF"), open = "main",
                   bsCollapsePanel(paste0("SIRIUS results for m/z ", selitem$mz, " @ ", selitem$rt, "sec"),
                                   fluidPage(
                                     htmlOutput(ns("tablewrap"))
                                   ),
                                   style = "success",value = "main") 
                   
                   
        )
        
        #        column(8,
        #        DiagrammeR::grVizOutput(ns("siriusTreePlot"), width = "100%", height = "500px"))
        # )
        # 
        # column(8,
        #        
        #        fluidRow(htmlOutput(ns("fingerIDwindow")))
        #        ))
      )
    }else{
      bsCollapse(id = ns("collapseSiriusMF"), open = NULL,
                 bsCollapsePanel("No SIRIUS results selected.", style = "danger",
                                 
                                 p("No Siriusspectrum selected, or no SIRIUS results available for selected spectrum, or calculation is still in progress.")
                                 
                 ))
    }
    
    
    
  })
  
  output$smileplot <- renderPlot({
      if(!is.null(internalValues$activeStructure)
                  && !is.null(internalValues$activeStructure$smiles)){
          

          # 
          #       pcid <- internalValues$activeStructure$pubchemids
      if(values$GlobalOpts$rcdk.installed){
      plotSMILE(internalValues$activeStructure$smiles, depictor = internalValues$depictor)
          
      }else{
          plot(numeric(0),
               numeric(0),
               ylim = c(0,1),
               xlim = c(0,1),
               type = "n", ann = FALSE, bty = "n", axes = F, asp = 1)
          
          text(0.5,0.5, labels = "Please install package rcdk to plot molecular structures", adj = 0.5)    
          }
      }
      
      })
  
  output$treeout <- renderUI({if(!is.null(internalValues$activeMF) && !is.null(internalValues$activeMF[["trees_dot"]]) ){
      DiagrammeR::grVizOutput(ns("siriusTreePlot"), width = "100%",
                                                      height = "500px")
      }
      })
  
  output$smileout <- renderUI({if(!is.null(internalValues$activeStructure) && !is.null(internalValues$activeStructure$smiles)){
      plotOutput(ns("smileplot"), width = "100%",
                 height = "500px")
  }
  })
 
  output$allSirius <- renderUI({
         #print(internalValues$activeSirius)
fluidPage(
   fluidRow(
     column(8,
            htmlOutput(ns("treeout"))
     ),
     column(4,
            htmlOutput(ns("smileout"))
            )
     ),
  fluidRow(
                  htmlOutput(ns("selectResults"))
                  
  ),
  fluidRow(
    htmlOutput(ns("selectSirius"))
  )
    )
  })
 
  
  return(internalValues)
}

#' SiriusModuleUI
#' 
#' 
#' UI module for interactive SIRIUS interface
#' 
#' @param id id to be used in ns()
#' 
#' @export
SiriusModuleUI <- function(id){
  ns <- NS(id)
  htmlOutput(ns("allSirius"))
  
}