#' SpecModule2
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input,output,session arguments necessary for use with \link[shiny]{callModule}()
#' @param reactive a \link[shiny]{reactive} object with settings for this module. See \code{details}.
#' @param values a \link[shiny]{reactiveValues} object that in effect gives read and write access to external objects
#' @param id id to be used to define a namespace via  \link[shiny]{NS}() (must match \code{id} in \link[shiny]{callModule} for the server Module)
#' 
#' @decribeIn SpecModule2 Server module, to be called with \link[shiny]{callModule}()
#' 
#' @details 
#' \subsection{reactives}{
#' Can either be a list with entries \code{scanTable} and \code{type}, 
#' specifying inputs for the \link[METABOseek]{getScanInfo}, OR
#' a list with entries \code{spectrum} and \code{specinfo}. 
#' 
#' \describe{
#' \item{scanTable}{}
#' \item{type}{}
#' \item{spectrum}{}
#' \item{specinfo}{}
#' }
#' }
#' 
#' 
#' @importFrom xcms getMsnScan
#' @importFrom xcms getScan
#' @importFrom MassTools mergeMS calcMF
#' @export 
SpecModule2 <- function(input,output, session,
                        values = reactiveValues(MSData = MSData,
                                                GlobalOpts = GlobalOpts),
                        reactives = reactive({
                          list(scantable = NULL,
                               type = "ms2",
                               spectrum = NULL,
                               specinfo = NULL,
                               xrange = NULL,
                               markermz = NULL)
                          
                        })
){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(plotArgs = NULL,
                                   maxxrange = NULL, #maximum x axis range
                                   maxyrange = NULL, #maximum y axis range
                                   
                                   marker = NULL, #selected peak with $mz and $intensity
                                   highlights = NULL, #peaks to be highlighted with $mz and $intensity
                                   hover = NULL, #peak hovered over with $mz and $intensity
                                   
                                   data = NULL,
                                   ymax = 100)
  
  # observeEvent(set(),{sc()})
  observeEvent(reactives(),{
    # observe({
    if(!is.null(reactives())){
      
      if(!is.null(reactives()$spectrum)){
        
        plotArgs <- reactives()[!names(reactives()) %in% c("specinfo","scantable","type")]
        
        if(!is.null(reactives()$specinfo)){
          internalValues$specinfo <- reactives()$specinfo
          plotArgs$fileName <- parseTitle(internalValues$specinfo)
        }else{
          internalValues$specinfo <- NULL
        }
        
        internalValues$plotInitArgs <- plotArgs
        
        
      }else if(!is.null(reactives()$scantable) && !is.null(values$MSData$data) ){
        
        plotArgs <- reactives()[!names(reactives()) %in% c("specinfo","scantable","type")]
        
        plotArgs$spectrum <- mergeMS(getAllScans(reactives()$scantable, MSData = values$MSData$data))
        
        internalValues$specinfo <- getScanInfo(reactives()$scantable$file,
                                               reactives()$scantable$scan,
                                               values$MSData$MSnExp,
                                               type = if(!is.null(reactives()$type)){reactives()$type}else{"acquisition"}) 
        
        
        plotArgs$fileName <- parseTitle(internalValues$specinfo)
        
        
        internalValues$plotInitArgs <- plotArgs
        
        
        
      }else{
        internalValues$plotInitArgs <- NULL
        internalValues$specinfo <- NULL
      }
    }else{
      internalValues$plotInitArgs <- NULL
      internalValues$specinfo <- NULL
    }
    
  })
  
  
  
  output$specinfo <- renderUI({
    if(!is.null(internalValues$plotInitArgs)
       && (!is.null(Mspec$marker)
           ||!is.null(Mspec$hover)
       )){
      
      
      
      
      
      fluidRow(
        p(paste0("Marker on: ", round(as.numeric(Mspec$marker$mz[1]), 5),
                 ", Cursor on: ", round(as.numeric(Mspec$hover$mz[1]),5),
                 if(length(Mspec$hover$mz) !=0 && length(Mspec$marker$mz) != 0){
                   paste0(" (",
                          if(as.numeric(Mspec$hover$mz) > as.numeric(Mspec$marker$mz[1])){"+"}else{""},
                          round(as.numeric(Mspec$hover$mz[1]) - as.numeric(Mspec$marker$mz[1]),5), ")")}else{""}))
        
      )
      
    }
    
    
  })
  
  
  # output$predicitonSelectors <- renderUI({
  #   
  #   tagList(
  #     checkboxInput(ns("predictselection"),"predict MF for selection", value = F)
  #   )
  # })
  # 
  # 
  output$specinfo <- renderUI({
    if(!is.null(internalValues$plotInitArgs)
       && (!is.null(Mspec$marker)
           ||!is.null(Mspec$hover)
       )){
      
      if(length(Mspec$marker$mz) != 0){
        
        
       
        
        mfmark <-  calcMF(as.numeric(Mspec$marker$mz[1]), 
                          summarize = T,  
                          top = 3,
                          z = values$GlobalOpts$mzquery.mzcharge,
                          ppm =  values$GlobalOpts$mzquery.mzppm,
                          elements = values$GlobalOpts$mzquery.InitializedElements,
                          BPPARAM = NULL,#bpparam(),
                          Filters = list(DBErange = c(values$GlobalOpts$mzquery.minUnsat,values$GlobalOpts$mzquery.maxUnsat),
                                         minElements = values$GlobalOpts$mzquery.minElements,
                                         maxElements = values$GlobalOpts$mzquery.maxElements,
                                         parity = if(values$GlobalOpts$mzquery.parity == "either"){NULL}else{values$GlobalOpts$mzquery.parity},
                                         maxCounts = values$GlobalOpts$mzquery.maxcounts,
                                         SENIOR3 = if(values$GlobalOpts$mzquery.valencefilter){0}else{NULL},
                                         HCratio = values$GlobalOpts$mzquery.hcratio,
                                         moreRatios = values$GlobalOpts$mzquery.moreratios,
                                         elementHeuristic = values$GlobalOpts$mzquery.elementheuristic))
      }else{
        mfmark <- ""
        }
        
        if(length(Mspec$hover$mz) !=0 
           && length(Mspec$marker$mz) != 0){
        mfdist <- calcMF(as.numeric(Mspec$marker$mz[1]), 
                         summarize = T,  
                         top = 3,
                         z = 0,
                         ppm =  values$GlobalOpts$mzquery.mzppm,
                         elements = values$GlobalOpts$mzquery.InitializedElements,
                         BPPARAM = NULL,#bpparam(),
                         Filters = list(DBErange = c(values$GlobalOpts$mzquery.minUnsat,values$GlobalOpts$mzquery.maxUnsat),
                                        minElements = "C0",
                                        maxElements = "C999",
                                        parity = NULL,
                                        maxCounts = values$GlobalOpts$mzquery.maxcounts,
                                        SENIOR3 = if(values$GlobalOpts$mzquery.valencefilter){0}else{NULL},
                                        HCratio = values$GlobalOpts$mzquery.hcratio,
                                        moreRatios = values$GlobalOpts$mzquery.moreratios,
                                        elementHeuristic = values$GlobalOpts$mzquery.elementheuristic))
      }
      
      
      fluidRow(
        p(paste0("Marker on: ", round(as.numeric(Mspec$marker$mz[1]), 5),"[",mfmark,"]",
                 ", Cursor on: ", round(as.numeric(Mspec$hover$mz[1]),5),
                 if(length(Mspec$hover$mz) !=0 && length(Mspec$marker$mz) != 0){
                   paste0(" (",
                          if(as.numeric(Mspec$hover$mz) > as.numeric(Mspec$marker$mz[1])){"+"}else{""},
                          round(as.numeric(Mspec$hover$mz[1]) - as.numeric(Mspec$marker$mz[1]),5), ")", mfdist)}else{""}))
        
      )
      
    }
    
    
  })
  
  
  output$specAll <- renderUI({
    if(!is.null(internalValues$plotInitArgs)){
      
      
      fluidPage(
        fluidRow(downloadButton(ns('pdfButton'), "Save as pdf"),
                 downloadButton(ns('peaklistButton'), "Save as table"),
                 htmlOutput(ns("predictionselection"))
                 
        ),
        fluidRow(SpecplotWidgetUI(ns("specp"))),
        htmlOutput(ns("specinfo"))
        
        
      )
    }
  })
  
  # elementButton <- callModule(SwitchButton,"tbutton",
  #                             states = list(on = list(label = "",
  #                                                     title = "Click to turn Elemental composition calculation OFF",
  #                                                     icon = icon("react"),
  #                                                     style = "background-color: #1BBF1E;"),
  #                                           off = list(label = "",
  #                                                      icon = icon("react"),
  #                                                      title = "Click to turn Elemental composition calculation ON")),
  #                             type = "actionButton")
  
  
  Mspec <- callModule(SpecplotWidget, "specp",
                      reactives = reactive({internalValues$plotInitArgs}),
                      
                      layout = reactive({list(active = T,
                                              highlights = NULL,
                                              height = 550,
                                              tooltip = "TEST",
                                              selectCallback = T)}),
                      keys = reactive({values$GlobalOpts$keyinput.keydown}))
  
  
  
  
  observeEvent(Mspec$marker$mz,{
    
    if(!is.null(Mspec$marker) && is.numeric(Mspec$marker$mz)){
     
      values$GlobalOpts$mzquery.SpectrumMz <- Mspec$marker$mz
       
    }
    
  })
  
  
  
  output$pdfButton <- downloadHandler(filename= function(){
    titleout <- if(length(set()$spec$sel$File) == 1){
      filesel <- match(basename(set()$spec$sel$File), basename(names(set()$msdata)))
      
      paste0(basename(set()$spec$sel$File), "_", 
             if(!is.null(set()$spec$MS2) && set()$spec$MS2){
               set()$msdata[[filesel]]@msnAcquisitionNum[set()$spec$sel$scan]
             }
             else{set()$msdata[[filesel]]@acquisitionNum[set()$spec$sel$scan]})}
    else{"averagedSpectra"}
    
    return(paste0(titleout,".pdf"))}, 
    content = function(file){
      
      pdf(file,
          14,6
      )
      
      if(!is.null(selections$plots$spec$fullplot)){
        replayPlot(selections$plots$spec$fullplot)
      }
      
      #replayPlot(selections$plots$spec$fullplot)
      dev.off()
      
    },
    contentType = "application/pdf")
  
  output$peaklistButton <- downloadHandler(filename= function(){
    titleout <- if(length(set()$spec$sel$File) == 1){
      filesel <- match(basename(set()$spec$sel$File), basename(names(set()$msdata)))
      
      paste0(basename(set()$spec$sel$File), "_", 
             if(!is.null(set()$spec$MS2) && set()$spec$MS2){
               set()$msdata[[filesel]]@msnAcquisitionNum[set()$spec$sel$scan]
             }
             else{set()$msdata[[filesel]]@acquisitionNum[set()$spec$sel$scan]})}
    else if(!is.null(selections$plots$spec$data)){"averagedSpectra"}
    else{"ERROR"}
    
    return(paste0(titleout,".tsv"))}, 
    content = function(file){
      if(!is.null(selections$plots$spec$data)){
        data.table::fwrite(data.frame(mz = selections$plots$spec$data[,1],
                                      intensity = selections$plots$spec$data[,2]), file, sep = "\t")
      }else{
        data.table::fwrite(data.frame(mz = 0,
                                      intensity = 0), file, sep = "\t")
      }
    },
    contentType = "text/tab-separated-values")
  
  return(internalValues)
}


#' @describeIn SpecModule2 UI function for SpecModule2
#' @export
SpecModule2UI <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("specAll"))
  
  
}