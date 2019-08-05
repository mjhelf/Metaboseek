#' FeatureReportModule
#' 
#' Module for visualization of MS1 and MS2 spectra along with EICs, plus option 
#' to plot them together with SIRIUS results in a pdf
#' 
#' @inherit MseekModules
#' 
#' 
#' @param MS2feed  temporary solution to feed current MS2 scan in from MS2BrowserModule
#' @param tree feed SIRIUS fragmentation tree in from MS2BrowserModule
#' @param fragments feed SIRIUS fragmentation annotation for the MS2 scan
#' 
#' @return 
#' Returns its internalValues
#' 
#' @describeIn FeatureReportModule server logic
#' 
#' @export 
FeatureReportModule <- function(input,output, session,
                                   values,
                                MS2feed = NULL,  #TODO: simplify inputs
                                tree = reactive({}),
                                fragments = NULL){
  
  ns <- NS(session$ns(NULL))
  
  EICcache <- reactiveValues()
  
  
  
  output$pdfButton <- downloadHandler(filename= function(){
    titleout <- "featureReport"
    
    return(paste0(titleout,".pdf"))}, 
    content = function(file){
      
      # if(!is.null(fragments)){
      #   print(fragments()$fragments)}
      
      grp <- if(!is.null(values$GlobalOpts$groupBy)
                && values$GlobalOpts$groupBy %in% c("grouping", "grouping2")){values$GlobalOpts$groupBy}else{"grouping"}
      
      featureReport(pdf_settings = list(file = file, width = NULL, height = NULL),
                    layout_settings = min(values$GlobalOpts$plotCols,length(values$MSData$layouts[[values$MSData$active]][[grp]])),
                    EICplots = plotEngine(),
                    MS1 = iSpec1()$plotArgs,
                    MS2 = MS2spec()$plotArgs,
                    tree = list(tree = tree(),
                                resolution = 5000),
                    fragments = if(!is.null(fragments)){
                      fragments()
                      }else{NULL},
                    
                    selectMS2 = if(is.null(MS2feed()$spec$sel)){NULL}
                                else{names(values$MSData$data)[basename(names(values$MSData$data)) %in% basename(MS2feed()$spec$sel$File)]}
                      )
      
    },
    
    
    
    contentType = "application/pdf")
  
  output$pdfTree <- downloadHandler(filename= function(){
    titleout <- "TreeOnly"
    
    return(paste0(titleout,".pdf"))}, 
    content = function(file){
      plotTree(tree = tree(),
                                resolution = 5000,
                                filename = file)
      
    },
    
    
    
    contentType = "application/pdf")
  
  
 plotEngine <- reactive({

    if(!is.null(values$MSData$data) && !is.null(values$MSData$layouts[[values$MSData$active]]$grouping)){
      rtmid <- if(is.null(values$featureTables$Maintable$selected_rows)){NULL}else{values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"rt"]}
      mzmid <- if(is.null(values$featureTables$Maintable$selected_rows)){NULL}else{values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]}
      RTall <- values$GlobalOpts$RTtoggle
      adducts <- if(is.null(values$MSData$massShifts$shifts)){0}else{values$MSData$massShifts$shifts}
      RTcorrect <- if(is.null(input$RtCorrActive) || !input$RtCorrActive){NULL}else{values$MSData$RTcorr}
      
      # mzx <- data.frame(mzmin = mzmid - mzmid*values$GlobalOpts$PPMwindow*1e-6,
      #                  mzmax = mzmid + mzmid*values$GlobalOpts$PPMwindow*1e-6)
      
      if(any(RTall, is.null(rtmid))){ # any can handle NULL, seems more flexible than ||
        rtmid <- NULL
        rtx <- NULL
      }else{
        rtx <- data.frame(rtmin = rtmid - values$GlobalOpts$RTwindow,
                          rtmax = rtmid + values$GlobalOpts$RTwindow)
      }
      
      #generate mz boundary df
      if(any(values$GlobalOpts$TICtoggle, is.null(mzmid)) ){
        mzmid <- if(!is.null(rtmid)){rep(100,length(rtmid))}else{100}
        mzx <- data.frame(mzmin = mzmid-1,
                          mzmax = mzmid+1)
        
      }else{
        mzx <- data.frame(mzmin = mzmid - mzmid*values$GlobalOpts$PPMwindow*1e-6,
                          mzmax = mzmid + mzmid*values$GlobalOpts$PPMwindow*1e-6)
      }
      
      
      EICcache[[values$MSData$active]] <- multiEICplus(rawdata= values$MSData$data[basename(names(values$MSData$data)) %in% basename(values$MSData$layouts[[values$MSData$active]]$filelist)  ],
                                                               mz = mzx,
                                                               rt = if(is.null(RTcorrect)){rtx}else{NULL},
                                                               rnames = row.names(mzmid), #major item names
                                                               byFile = F, #if true, table will be sorted by rawfile, otherwise by feature
                                                               adducts,
                                                               RTcorr = RTcorrect
      )
      
      #make subtitles
      
      subtitles <- NULL
      
      if(length(internalValues$subtitleColumns) > 0  && internalValues$subtitleColumns != ""){
        
        subtitles <- paste0(internalValues$subtitleColumns[1], ": ",
                       values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],internalValues$subtitleColumns[1]])
        
        if(length(internalValues$subtitleColumns) > 1){
        for( i in 2:length(internalValues$subtitleColumns)){
          
          subtitles <- paste0(subtitles, " ", internalValues$subtitleColumns[i], ": ",
                              values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],internalValues$subtitleColumns[i]])
          
        }
        }
        

      }
      
      if(!is.null(values$GlobalOpts$colorBy) 
         && values$GlobalOpts$colorBy %in% c("grouping", "grouping2")
         &&!is.null(values$GlobalOpts$groupBy)
         && values$GlobalOpts$groupBy %in% c("grouping", "grouping2")){
        
      cr <-  makeColorscheme(maingroup = values$MSData$layouts[[values$MSData$active]][[values$GlobalOpts$groupBy]],
                                    colorgroup =values$MSData$layouts[[values$MSData$active]][[values$GlobalOpts$colorBy]],
                                    colrange = values$GlobalOpts$colorscheme,
                                    transparency = values$GlobalOpts$plotTransparency)
        
      }else{
        cr <- values$GlobalOpts$colorscheme
      }

      grp <- if(!is.null(values$GlobalOpts$groupBy)
                && values$GlobalOpts$groupBy %in% c("grouping", "grouping2")){values$GlobalOpts$groupBy}else{"grouping"}
      list(rtmid = rtmid,
           mzmid = mzmid,
           glist = values$MSData$layouts[[values$MSData$active]][[grp]],
           cols = min(values$GlobalOpts$plotCols,length(values$MSData$layouts[[values$MSData$active]][[grp]])),
           colrange = cr,
           transparency = values$GlobalOpts$plotTransparency,
           RTall = values$GlobalOpts$RTtoggle,
           TICall = values$GlobalOpts$TICtoggle || is.null(values$featureTables$Maintable$selected_rows),
           rtw = values$GlobalOpts$RTwindow,
           ppm = values$GlobalOpts$PPMwindow,
           rdata = values$MSData$data[basename(names(values$MSData$data)) %in% basename(values$MSData$layouts[[values$MSData$active]]$filelist)  ],
           pdfFile = NULL,
           leadingTIC = F,
           lw = values$GlobalOpts$plotLw,
           adducts = if(is.null(values$MSData$massShifts$shifts)){0}else{values$MSData$massShifts$shifts},
           cx = values$GlobalOpts$plotCx,
           midline = values$GlobalOpts$MLtoggle,
           yzoom = values$GlobalOpts$plotYzoom,
           RTcorrect = if(is.null(input$RtCorrActive) || !input$RtCorrActive){NULL}else{values$MSData$RTcorr},
           importEIC = EICcache[[values$MSData$active]],
           globalYmax = internalValues$reltoCheck,
           subtitles = subtitles,
           relPlot = values$GlobalOpts$relPlotToggle,
           raise = values$GlobalOpts$raiseToggle,
           RescaleExclude = "MS2"
      )
      
    }else{
      
      NULL
      }
    
  })
  
  output$mainPlotEICsPre <- renderPlot({
    if(!is.null(plotEngine())){
    
      
      do.call(EICgeneral, plotEngine())
      
    }
    
  }, bg = "white", execOnResize = T)
 
  
  mainPlotHeight <- reactive({if(!is.null(values$MSData$active) && values$MSData$active != ""){
    sizeFactor <- if(!is.null(input$miniPlots) && input$miniPlots){0.4}else{1}

    paste0(sizeFactor*(ceiling(length(values$MSData$layouts[[values$MSData$active]]$grouping)/min(values$GlobalOpts$plotCols,
                                                                                                  length(values$MSData$layouts[[values$MSData$active]]$grouping)))*400+100),"px")
  }
    else{"auto"}
  })
  
  output$mainPlotEICs <- renderUI({
    if(!is.null(values$MSData$data)){
      plotOutput(ns("mainPlotEICsPre"),
                 height = mainPlotHeight()
      )
    }
  })
  
  
  
  
  observe({if(!is.null(values$MSData$active) && !is.null(EICcache[[values$MSData$active]])) {
    
    maxI <- which.max(EICcache[[values$MSData$active]][[1]][,"intmax"])
    maxsc <- which.max(EICcache[[values$MSData$active]][[1]][maxI,"intensity"][[1]])
    
    
    EICcache$iSpec1_feed <- list(File = row.names(EICcache[[values$MSData$active]][[1]])[maxI],
                scan = EICcache[[values$MSData$active]][[1]][maxI,"scan"][[1]][maxsc],
                rt = EICcache[[values$MSData$active]][[1]][maxI,"rt"][[1]][maxsc]
                )
    
  }else{
    EICcache$iSpec1_feed <- NULL
  }
  })
  
  
  # iSpec1_feed <- eventReactive(EICcache[[values$MSData$active]],{
  #   
  #   if(!is.null(EICcache[[values$MSData$active]])){
  #     maxI <- which.max(EICcache[[values$MSData$active]][[1]][,"intmax"])
  #     maxsc <- which.max(EICcache[[values$MSData$active]][[1]][maxI,"intensity"][[1]])
  #     return(list(File = row.names(EICcache[[values$MSData$active]][[1]])[maxI],
  #                 scan = EICcache[[values$MSData$active]][[1]][maxI,"scan"][[1]][maxsc],
  #                 rt = EICcache[[values$MSData$active]][[1]][maxI,"rt"][[1]][maxsc]
  #                 
  #     ))
  #   }
  # })
  
  # output$siriusTreePlot <-  DiagrammeR::renderGrViz({ if(!is.null(tree) && !is.null(tree()) ){
  # 
  #                          tree()
  # 
  # }  })
  
  
  iSpec1 <- callModule(Specmodule,"Spec1", tag = ns("Spec1"), 
                       set = reactive({
                         
                         
                         
                         if(is.null(MS2spec()$plotArgs) || is.null(MS2feed()) || is.null(MS2feed()$spec$sel) || length(MS2feed()$spec$sel$File) <1){
                         
                         
                         list(spec = list(xrange = if(is.null(values$featureTables$Maintable$selected_rows)){NULL}else{c(values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]-10,
                                                                                                           values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]+10)},
                                          yrange = NULL,
                                          maxxrange = NULL,
                                          maxyrange = NULL,
                                          sel = if(!is.null(EICcache$iSpec1_feed)){list(File = EICcache$iSpec1_feed$File[[1]],
                                                                                 scan = EICcache$iSpec1_feed$scan[[1]],
                                                                                 rt = EICcache$iSpec1_feed$rt[[1]])}else{NULL},
                                          data = NULL,
                                          mz = if(is.null(values$featureTables$Maintable$selected_rows)){NULL}else{values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]}),
                              layout = list(lw = 1,
                                            cex = 1.5,
                                            controls = F,
                                            ppm = values$GlobalOpts$PPMwindow,
                                            active = input$ShowSpec,
                                            highlights = NULL,
                                            height = 350),
                              msdata = values$MSData$data)
                         
                         }else{
                           tryCatch({
                           targets <- MS2feed()$spec$sel
                           
                         
                           ms1targets <- list(File = targets$File)
                           
                           ms1targets$scan <- sapply(seq(length(targets$File)),function(n){ which.min(abs(values$MSData$data[[which(basename(names( values$MSData$data)) == targets$File[n])]]@scantime - targets$rt[n]))})
                           
                           ms1targets$rt <- sapply(seq(length(targets$File)),function(n){values$MSData$data[[which(basename(names( values$MSData$data)) == targets$File[n])]]@scantime[ms1targets$scan[n]]})
                           
                           
                           list(spec = list(xrange = if(is.null(values$featureTables$Maintable$selected_rows)){NULL}else{c(values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]-3,
                                                                                                             values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]+7)},
                                            yrange = NULL,
                                            maxxrange = NULL,
                                            maxyrange = NULL,
                                            sel = ms1targets,
                                            data = NULL,
                                            mz = if(is.null(values$featureTables$Maintable$selected_rows)){NULL}else{values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]}),
                                layout = list(lw = 1,
                                              cex = 1.5,
                                              controls = F,
                                              ppm = values$GlobalOpts$PPMwindow,
                                              active = T, #input$ShowSpec,
                                              highlights = NULL,
                                              height = 350),
                                msdata = values$MSData$data,
                                moreArgs = list(k = 10))
                           },
                           error = function(e){
                             print("ERROR occurred")
                             print(targets)
                             
                             list(spec = list(xrange = NULL,
                                              yrange = NULL,
                                              maxxrange = NULL,
                                              maxyrange = NULL,
                                              sel = NULL,
                                              data = NULL,
                                              mz = NULL),
                                  layout = list(lw = 1,
                                                cex = 1.5,
                                                controls = F,
                                                ppm = values$GlobalOpts$PPMwindow,
                                                active = T, #input$ShowSpec,
                                                highlights = NULL,
                                                height = 350),
                                  msdata = values$MSData$data,
                                  moreArgs = list(k = 10))
                             
                           })
                           
                           
                           }
                         
                       }), 
                       keys = reactive({values$GlobalOpts$keyinput.keydown})#,
                      # static = list(title = "MS1 spectrum")
  )
  
  MS2spec <- callModule(Specmodule,"ms2spec", tag = ns("ms2spec"), 
                       set = reactive({MS2feed()}), 
                       keys = reactive({values$GlobalOpts$keyinput.keydown})
  )
  
  SelectMSGrouping <- callModule(SelectMSGroupingModule, "selectLayout",
                                 values = reactiveValues(MSData = values$MSData),
                                 static = list(editOnly = F)
  )
  
  internalValues <- reactiveValues(iSpec1 = iSpec1,
                                   yaxmax = NULL,
                                   reltoCheck = F,
                                   subtitleColumns = "comments")
  
  output$reltocheck <- renderUI({
    div(title = "Plot EICs for all groups to the same scale (the highest intensity value in all EICs for a feature).",
           checkboxInput(ns("reltoCheck"), "Rescale", value = internalValues$reltoCheck)
    )
    
  })
  
  output$subtitleSelect <- renderUI({
    div(title = "Select columns to use for the plot subtitle. All columns currently selected for the main table can be used.",
           selectizeInput(ns("subtitleselect"), "Subtitle content", 
                          selected = internalValues$subtitleColumns, 
                          choices = colnames(values$featureTables$Maintable$liveView),
                          multiple = TRUE)
)
  })



  observeEvent(input$subtitleselect,{
    internalValues$subtitleColumns <- input$subtitleselect
  })
  
  observeEvent(input$reltoCheck,{
    internalValues$reltoCheck <- input$reltoCheck
  })
  
  return(internalValues)
  
}


#' @describeIn FeatureReportModule UI elements
#' @export 
FeatureReportModuleUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      #tags$head(
      # tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
      #),
      
      
      column(1,
             checkboxInput(ns("RtCorrActive"), "RT correction", value = F)
      ),
      
      column(1,
             checkboxInput(ns("ShowSpec"), "Show spectrum", value = F)
      ),
      
      column(1,
             div(title = "Make EIC plots smaller to gain vertical space",  checkboxInput(ns("miniPlots"), "small EICs", value = F))
      ),
      
      column(1,
             htmlOutput(ns("reltocheck"))
             ),
       column(2,
              htmlOutput(ns("subtitleSelect"))
       ),
      column(1,
             downloadButton(ns("pdfButton"), "Save Plot")
      ),
      column(1,
             downloadButton(ns("pdfTree"), "Save Tree")
      ),
      
      column(4,
             SelectMSGroupingModuleUI(ns("selectLayout"))
      )
    ),
    
    
    fluidRow(
      #imageOutput("mainPlotPlaceholder"),
      htmlOutput(ns("mainPlotEICs"))),
    
    fluidRow(
      column(6,
      SpecmoduleUI(ns("Spec1"))
    ),
    column(6,
           SpecmoduleUI(ns("ms2spec"))
    ))#,
  #  fluidRow(
 #     DiagrammeR::grVizOutput(ns("siriusTreePlot"), width = "100%", height = "500px")
 # )
  )  
}