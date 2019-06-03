#' SelectMS2Module
#' 
#' 
#' Module for selection of MS2 spectra. Will search for MS2 spectra matching the
#'  current selection in the Feature table within specified ppm and RT boundaries,
#'  and will make the selection available app-wide.
#'  
#'  @details Does not return anything, but will modify values:
#'  \itemize{
#'  \item{values$MSData$MS2.selectTable:} data.frame listing metadata for all MS2 spectra matching criteria
#'  \item{values$MSData$MS2.selected:} list with three items: 
#'  \itemize{
#'  \item{spectra:} list of selected MS2 spectra as matrices
#'  \item{mergedSpectrum:} result of merging all selected spectra as a matrix
#'  \item{specInfo:} data.frame listing metadata for all selected MS2 spectra
#' }}
#' 
#' @inherit MseekModules
#' 
#' @describeIn SelectMS2Module server logic for SelectMS2Module
#' 
#' @export 
SelectMS2Module <- function(input,output, session, 
                            values){
    
    ns <- NS(session$ns(NULL))
    
    
    ###core functionality
    selectScan <- callModule(TableModule2,'scantab', reactives = reactive({list(df =  if(is.data.frame(values$MSData$MS2.selectTable)
                                                                                         #&& nrow(values$MSData$MS2.selectTable)>0
    ){
        values$MSData$MS2.selectTable[,c("precursorMZ",
                                         "retentionTime",
                                         "precursorCharge",
                                         "totIonCurrent",
                                         "precursorIntensity", 
                                         "filename",
                                         "acquisitionNum")] }else{NULL},
    layout = list(
        perpage = NULL,
        height = 350,
        readOnly = T,
        contextMenu = F,
        fixedColumnsLeft = 0,
        format = list(col = c("precursorMZ"),
                      format = "0.00000"),
        invertReadOnly = NULL
    ))})
    )
    observeEvent(c(input$ppmSearch,
                   input$rtSearch,
                   input$rtcheck,
                   FTselection(values)),{ 
                       if(nrow(FTselection(values)) > 0 && !is.null(values$MSData$MSnExp)){
                           values$MSData$MS2.selectTable <- findMSnScans(values,
                                                                         mz = FTselection(values)$mz,
                                                                         rt = FTselection(values)$rt,
                                                                         ppm = input$ppmSearch,
                                                                         rtw = if(!input$rtcheck){
                                                                             input$rtSearch
                                                                         }else{999999}
                           )
                           
                       }
                   }, ignoreInit = T)
    
    output$searchcontrol <- renderUI({
        fluidRow(
            column(3,
                   div(title = "m/z tolerance for MS2 scan search (ppm)",
                       numericInput(ns('ppmSearch'), "ppm", value = 5))),
            column(3,
                   div(title = "RT tolerance for MS2 scan search (seconds)",
                       
                       numericInput(ns('rtSearch'), "seconds", value = 15))
            ),
            column(3,
                   div(title = "search in entire RT range",
                       
                       checkboxInput(ns('rtcheck'), "entire RT range", value = T))
            )#,
            # column(3,
            #        ShowSiriusModuleUI(ns("showSirius"))
            # ),
            # column(3,
            #        GetSiriusModuleUI(ns("getSirius"))
            # )
        )
    })
    
    ###MAKE SPECTRUM FROM selected scans, to be used multiple times, for SIRIUS as well as plotting
    observe({
        #shiny::req(selectScan$liveView)
        res <- list()
        tryCatch({
            if(!is.null(selectScan$liveView) && nrow(values$MSData$MS2.selectTable) > 0){
                
                if(!is.null(selectScan$selected_rows)){
                    res$specinfo <- values$MSData$MS2.selectTable[row.names(selectScan$liveView)[selectScan$selected_rows],]
                }else{
                    res$specinfo <- values$MSData$MS2.selectTable
                    
                }
                
                res$spectra <- getSpectra(values, index = res$specinfo$spectrum)
                
                res$spectrum <- MassTools::mergeMS(res$spectra)
                
            }
            values$MSData$MS2.selected <- res
            
            
        },
        error = function(e){
            values$MSData$MS2.selected <- list()
        })
        
    })
    
    
    
}


#' @describeIn SelectMS2Module UI elements for SelectMS2Module
#' @export 
SelectMS2ModuleUI <-  function(id){
    ns <- NS(id)
    
    fluidPage(
        htmlOutput(ns("searchcontrol")),
        fluidRow(
            TableModuleUI(ns('scantab')))
    )
    
}

