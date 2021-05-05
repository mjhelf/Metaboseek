#' GetSiriusWidget
#' 
#' Widget that starts a SIRIUS analysis as specified in reactives({}, which will 
#' add an entry to the index file in the SIRIUS folder to be observed by the SiriusModule
#' 
#' @inherit MseekWidgets
#' 
#' @param reactives a reactive() returning a list of \code{\link{runSirius}()}
#'  parameters
#' 
#' @describeIn GetSiriusWidget server logic
#' 
#' @export 
GetSiriusWidget <- function(input,output, session, 
                          reactives = reactive({
                            list(                       sirpath = file.path(.MseekOptions$siriusFolder, "sirius"),
                                                        ms2,
                                                        parentmz,
                                                        outfolder = getwd(),
                                                        ms1 = NULL,
                                                        ion = "[M+?]+",
                                                        comments = "",
                                                        rt = "",
                                                        charge= 1,
                                                        fingerid = T,
                                                        scanindices = "",
                                                        moreOpts = "",
                                                        config = list(IsotopeSettings.filter = TRUE,
                                                                      FormulaSearchDB = c('ALL_BUT_INSILICO','ALL','BIO',
                                                                                          'METACYC','CHEBI','COCONUT',
                                                                                          'ECOCYCMINE','GNPS','HMDB',
                                                                                          'HSDB','KEGG','KEGGMINE',
                                                                                          'KNAPSACK','MACONDA','MESH',
                                                                                          'NORMAN','UNDP','PLANTCYC',
                                                                                          'PUBCHEM','PUBMED','YMDB',
                                                                                          'YMDBMINE','ZINCBIO'),
                                                                      Timeout.secondsPerTree = 0, 
                                                                      FormulaSettings.enforced = "HCNOP[5]S", 
                                                                      Timeout.secondsPerInstance = 0, 
                                                                      AdductSettings.detectable = "[[M+K]+,[M+H3N+H]+,[M+Na]+,[M-H4O2+H]+,[M-H2O+H]+,[M+H]+]", 
                                                                      UseHeuristic.mzToUseHeuristicOnly = 650, 
                                                                      AlgorithmProfile = "orbitrap", #qtof 
                                                                      IsotopeMs2Settings = "IGNORE", 
                                                                      MS2MassDeviation.allowedMassDeviation = '5.0ppm', 
                                                                      NumberOfCandidatesPerIon = 1, 
                                                                      UseHeuristic.mzToUseHeuristic = 300, 
                                                                      FormulaSettings.detectable = ",", 
                                                                      NumberOfCandidates = 10, 
                                                                      StructureSearchDB = c('ALL_BUT_INSILICO','ALL','BIO',
                                                                                            'METACYC','CHEBI','COCONUT',
                                                                                            'ECOCYCMINE','GNPS','HMDB',
                                                                                            'HSDB','KEGG','KEGGMINE',
                                                                                            'KNAPSACK','MACONDA','MESH',
                                                                                            'NORMAN','UNDP','PLANTCYC',
                                                                                            'PUBCHEM','PUBMED','YMDB',
                                                                                            'YMDBMINE','ZINCBIO'),
                                                                      AdductSettings.fallback = "[[M+K]+,[M+Na]+,[M+H]+]", 
                                                                      RecomputeResults = TRUE),
                                                        force = T)
                          })
){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(siriusIndex = NULL)
  
  observeEvent(input$getSirius,{
    if(!is.null(reactives())){
    tryCatch({
      print(reactives())
  do.call(runSirius, reactives())
    }, error = function(e){
      print(e)
      showNotification(paste("A problem occured and SIRIUS search failed"), type = "error", duration = 0)
      
    })
      
      }
      
  })
  output$getsiriusbutton <- renderUI({
    
    actionButton(ns("getSirius"), "Run SIRIUS", title = "Run SIRIUS with current settings for the selected spectrum or averaged spectra.")
    
  })
}

#' @describeIn GetSiriusWidget UI elements
#' @export
GetSiriusWidgetUI <- function(id){
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns("getsiriusbutton"))
  )
  
}