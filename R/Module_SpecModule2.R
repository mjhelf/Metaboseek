#' SpecModule2
#' 
#' Module for interactive mass spectrum view
#' 
#' @inherit MseekModules
#' 
#' @describeIn SpecModule2 Server module, to be called with \link[shiny]{callModule}()
#' 
#' @details 
#' \subsection{reactives}{
#' Can either be a list with entries \code{scanTable} and \code{type}, 
#' specifying inputs for the \link[Metaboseek]{getScanInfo}, OR
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
#' @examples 
#' \dontrun{
#' library(shiny)
#'
#' ui <- SpecModule2UI("examplemodule")
#' 
#' server <- function(input, output) {
#'     MseekMinimalServer(diagnostics = F, data = F, tables = F)
#'     
#'     ExampleModule <- callModule(SpecModule2, "examplemodule", values,
#'                                 reactives = reactive({
#'                                     list(spectrum = data.frame(mz = c(100,200,250,300,400),
#'                                                                intensity = c(1000,2000,3000,1000,3000)))
#'                                     
#'                                 }))
#' }
#' 
#' # Create Shiny app ----
#' shinyApp(ui, server)
#' }
#' 
#' @importFrom xcms getMsnScan
#' @importFrom xcms getScan
#' @importFrom MassTools mergeMS calcMF
#' @export 
SpecModule2 <- function(input,output, session,
                        values,
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
    internalValues <- reactiveValues(plotInitArgs = NULL,
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
                
                plotArgs <- reactives()[!names(reactives()) %in% c("specinfo","scantable","type", "spectra")]
                
                if(!is.null(reactives()$specinfo)){
                    internalValues$specinfo <- reactives()$specinfo
                    plotArgs$fileName <- parseTitle(internalValues$specinfo)
                }else{
                    internalValues$specinfo <- NULL
                }
                
                internalValues$plotInitArgs <- plotArgs
                
                
            }else if(!is.null(reactives()$scantable) && !is.null(values$MSData$data) ){
                
                plotArgs <- reactives()[!names(reactives()) %in% c("specinfo","scantable","type", "spectra")]
                
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
            
            if(length(Mspec$marker$mz) != 0
               && elementButton$state == "on"){
                
                
                
                
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
               && length(Mspec$marker$mz) != 0
               && elementButton$state == "on"){
                mfdist <- calcMF(abs(as.numeric(Mspec$hover$mz[1]) - as.numeric(Mspec$marker$mz[1])), 
                                 summarize = T,  
                                 top = 3,
                                 z = 0,
                                 moreRatios = FALSE,
                                 HCratio = FALSE,
                                 elementHeuristic = FALSE,
                                 SeniorRule = FALSE,
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
            }else{
             mfdist <- ""   
            }
            
            
            fluidRow(
                p(paste0("Marker on: ", round(as.numeric(Mspec$marker$mz[1]), 5),"[",mfmark,"]",
                         ", Cursor on: ", round(as.numeric(Mspec$hover$mz[1]),5),
                         if(length(Mspec$hover$mz) !=0 && length(Mspec$marker$mz) != 0){
                             paste0(" (",
                                    if(as.numeric(Mspec$hover$mz[1]) > as.numeric(Mspec$marker$mz[1])){"+"}else{""},
                                    round(as.numeric(Mspec$hover$mz[1]) - as.numeric(Mspec$marker$mz[1]),5), ")[", mfdist, "]")}else{""}))
                
            )
            
        }
        
        
    })
    
    output$buttons <- renderUI({
        
        div(class = "column",
            div(title = "Save as pdf",
            mDownloadButton(ns('pdfButton'), "", icon = icon("file-pdf"))),
            #div(title = "Save as text file",
            mDownloadButton(ns('peaklistButton'), "", icon = icon("list-alt"),
                            title = "Save as text file"),
            #),
        SwitchButtonUI(ns("tbutton"))
        )
    })
    
    
    output$specAll <- renderUI({
        if(!is.null(internalValues$plotInitArgs)){
            
            div(class = "flex-container",
                style = "display: flex; flex-direction: row",
                div(#class = "column",
                    #style = "width:55px;float:left",
                             htmlOutput(ns("buttons"))),
                div(#class = "column",
                    style = "width:100%",
                    SpecplotWidgetUI(ns("specp")),
                             htmlOutput(ns("specinfo")))
                            )
            
        }
    })
    
    elementButton <- callModule(SwitchButton,"tbutton",
                                states = list(off = list(label = "",
                                                         icon = icon("react"),
                                                         title = "Click to turn Elemental composition calculation ON"),
                                              on = list(label = "",
                                                        title = "Click to turn Elemental composition calculation OFF",
                                                        icon = icon("react"),
                                                        style = "background-color: #1BBF1E;")),
                                type = "actionButton")
    
    
    Mspec <- callModule(SpecplotWidget, "specp",
                        reactives = reactive({internalValues$plotInitArgs}),
                        
                        layout = reactive({list(active = T,
                                                highlights = NULL,
                                                height = 450,
                                                tooltip = "TEST",
                                                selectCallback = T)}),
                        keys = reactive({values$GlobalOpts$keyinput.keydown}))
    
    
    
    
    observeEvent(Mspec$marker$mz,{
        
        if(!is.null(Mspec$marker) && is.numeric(Mspec$marker$mz)){
            
            values$GlobalOpts$mzquery.SpectrumMz <- Mspec$marker$mz
            
        }
        
    })
    
    
    
    output$pdfButton <- downloadHandler(filename= function(){
        titleout <- gsub(":","_",gsub("/","",Mspec$plotArgs$fileName))
        
        return(paste0(titleout,".pdf"))}, 
        content = function(file){
            
            pdf(file,
                14,6
            )
            
            if(!is.null(Mspec$plotArgs)){
                do.call(specplot2,Mspec$plotArgs)
            }
            
            #replayPlot(selections$plots$spec$fullplot)
            dev.off()
            
        },
        contentType = "application/pdf")
    
    output$peaklistButton <- downloadHandler(filename= function(){
        titleout <-  gsub(":","_",gsub("/","",Mspec$plotArgs$fileName))
        
        return(paste0(titleout,".tsv"))}, 
        content = function(file){
            if(!is.null(Mspec$plotArgs) && !is.null(Mspec$plotArgs$spectrum)){
                data.table::fwrite(data.frame(mz = Mspec$plotArgs$spectrum[,1],
                                              intensity = Mspec$plotArgs$spectrum[,2]),
                                   file, sep = " ", col.names = FALSE)
            }else{
                data.table::fwrite(data.frame(mz = 0,
                                              intensity = 0),
                                   file, sep = " ",
                                   col.names = FALSE)
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