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
#' @importFrom splashR getSplash
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
                                   quickLookup = character(0)
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
  
  observeEvent(internalValues$siriusIndex,{
    if(is.data.frame(internalValues$siriusIndex)  && nrow(internalValues$siriusIndex) > 0){
      
      tryCatch({
    internalValues$quickLookup <- paste(round(internalValues$siriusIndex$mz,4),
       apply(internalValues$siriusIndex[,c("splash", "ion", "charge", "fingerid", "moreOpts", "METABOseek_sirius_revision")],
                                        1, paste, collapse = "//"), sep = "//")
      },
    error = function(e){print(e); return(character(0))})
    
    }
    else{
      character(0)
    }
    
  })
 
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
              internalValues$activeSirius <- getSirius(file.path(values$GlobalOpts$siriusFolder, "METABOseek"), splash = SirBrowser$liveView[SirBrowser$selected_rows[1],"splash"], ts = SirBrowser$liveView[SirBrowser$selected_rows[1],"timestamp"])

      },
      error =  function(e){

        internalValues$activeSirius <- NULL

      })
      internalValues$activeMF <- NULL
      internalValues$activeStructure <- NULL
      }

  })
  
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



  output$siriusTreePlot <-  DiagrammeR::renderGrViz({ if(length(internalValues$activeMF)>0){

                           internalValues$activeMF[["trees_dot"]]

  }  })

  output$fingerIDwindow <-  renderUI({
    if(!is.null(internalValues$activeStructure)){

      pcid <- internalValues$activeStructure$pubchemids
      if(length(grep(";", pcid))>0){

       pcid <-  strsplit(pcid,";")[[1]][1]

      }

    HTML('
<iframe id="inlineFramePubchem"
         title="webpage"
         style="border:none;width:100%;height:500px;" ',
         paste0('src="https://pubchem.ncbi.nlm.nih.gov/compound/', pcid,'">'),
         '</iframe>
         ')
    }
  })
  
  output$controlSirius <- renderUI({
    fluidRow(
      column(4,
             fluidRow(
               column(3,
                      selectizeInput(ns("selCharge"), "Charge:", choices = list("+1" = 1, "-1" = -1), selected = internalValues$selCharge)),
               column(3,
                      selectizeInput(ns("selIon"), "Ion", choices = list("?" = if(internalValues$selCharge ==1){"[M+?]+"}else{"[M+?]-"},
                                                                         "[M+H]+" = "[M+H]+",
                                                                         "[M+Na]+" = "[M+Na]+",
                                                                         "[M-H}-" = "[M-H]-",
                                                                         "[M+Cl]-" = "[M+Cl]-"), selected = internalValues$selIon)),
               column(3,
                      checkboxInput(ns("checkFinger"), "Get FingerID", value = internalValues$checkFinger))
             ),
             fluidRow(
               column(6,
                      selectizeInput(ns("selInstrument"), "Instrument", choices = list("Orbitrap" = "orbitrap", "QTOF" = "qtof", "FT-ICR" = "fticr"), selected = internalValues$selIon)),
               column(6, 
                      textInput(ns("elements"), "Allow elements:", value = internalValues$elements)))
      ),
      column(8,
             if(!is.null(internalValues$siriusIndex)){
               TableModule2UI(ns("sirbrowser"))
             }else{
               p("No SIRIUS results available")
             })
    )
  })
  
  output$selectSirius <- renderUI({
    
    bsCollapse(id = ns("collapseSiriusSearch"), open = "SIRIUS search settings",
               bsCollapsePanel("SIRIUS search settings",
                              htmlOutput(ns("controlSirius"))
                               )
    )
    
    
  })
  
  observeEvent(input$selCharge,{
    
    internalValues$selCharge <- input$selCharge
    
  })
  observeEvent(input$selIon,{
    
    internalValues$selIon <- input$selIon
    
  })
  observeEvent(input$checkFinger,{
    
    internalValues$checkFinger <- input$checkFinger
    
  })
  observeEvent(input$selInstrument,{
    
    internalValues$selInstrument <- input$selInstrument
    
  })
  observeEvent(input$elements,{
    
    internalValues$elements <- input$elements
    
  })
  
  output$SiriusUIWrap <- renderUI({
    fluidPage(
      bsCollapse(id = ns("collapseAllSirius"), open = NULL,
                 bsCollapsePanel("SIRIUS Module (click to open/close)",
      htmlOutput(ns("allSirius"))
    )))
    
  })
  
  
  output$allSirius <- renderUI({
    fluidPage(
    fluidRow(
      htmlOutput(ns("selectSirius"))
    ),
      if(!is.null(internalValues$activeSirius)){
        
        selitem <- internalValues$siriusIndex[internalValues$siriusIndex$splash == internalValues$activeSirius$splash 
                                              & internalValues$siriusIndex$timestamp == internalValues$activeSirius$timestamp,]
        
        tagList(
          bsCollapse(id = ns("collapseSiriusMF"), open = NULL,
                     bsCollapsePanel(paste0("SIRIUS results for m/z ", selitem$mz, " @ ", selitem$rt, "sec"), style = "success")),
          
          bsCollapse(id = ns("collapseSiriusMF"), open = "SIRIUS MF prediction",
                     bsCollapsePanel("SIRIUS MF prediction",
        fluidRow(
          column(4,
               TableModule2UI(ns("mfbrowser"))),
               column(8,
               DiagrammeR::grVizOutput(ns("siriusTreePlot"), width = "100%", height = "500px"))
        )
        )),
        bsCollapse(id =  ns("collapseFingerIDs"), open = "FingerID structure matches",
                   bsCollapsePanel("FingerID structure matches",
                                   fluidRow(
                                     column(4,
        fluidRow(TableModule2UI(ns("structurebrowser")))),
        column(8,
               
               fluidRow(htmlOutput(ns("fingerIDwindow")))
               ))))
  )
      }else{
        bsCollapse(id = ns("collapseSiriusMF"), open = NULL,
                   bsCollapsePanel("No SIRIUS results available (yet) for selected spectrum", style = "danger",
                   
                  p("No SIRIUS results available for selected spectrum, or calculation is still in progress.")
         
                   ))
      }
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
  htmlOutput(ns("SiriusUIWrap"))
  
}