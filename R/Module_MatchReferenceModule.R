#' MatchReferenceModule
#' 
#' One-button module to match MseekFT and MseekGraph objects with each other
#' 
#' @inherit MseekModules
#' 
#' @describeIn MatchReferenceModule server logic
#' 
#' @export 
MatchReferenceModule <- function(input,output, session,
                           values = reactiveValues(featureTables = NULL,
                                                   Networks = NULL)){
  ns <- NS(session$ns(NULL))
  

  dialog <- callModule(ModalWidget, "modbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                         
                           fluidRow(
                             p("Match features between two Feature Tables or a Network and a Feature Table.")),
                           fluidRow(
                             column(6,
                                    div(title = "Match to this reference object",
                                        selectizeInput(ns("selectreference"), "Reference:",
                                                       choices = names(values$Networks)[names(values$Networks) != "numNetworks"])
                                    )),
                             column(6,
                                    div(title = "Match this object to the reference",
                                        selectizeInput(ns("selectquery"), "Query:", choices = values$featureTables$index, selected = activeFT(values))
                                    ))),
                           fluidRow(
                             column(4,
                                    div(title = "Use a retention time window to match features.",
                                        checkboxInput(ns("rtcheck"), "Match retention time", value  = FALSE)
                                    )),
                             column(4,
                                    div(title = "Peak retention time window size (+/- in seconds between features from reference and query data.",
                                        numericInput(ns("rttol"),"RT tolerance (seconds)", value = 5, min = 0)
                                    ))),
                           fluidRow(
                               column(4,
                                      div(title = "Use an m/z tolerance window to match features.",
                                          checkboxInput(ns("mzcheck"), "Match m/z", value  = FALSE)
                                      )),
                               column(4,
                                      div(title = "m/z match toleranze (absolute number); features have to match within either mzdiff or ppm tolerance. Also used as tolerance for peak matching for MS2 similarity calculation.",
                                          numericInput(ns("mzdiff"),"mzdiff", value = 0.001, min = 0)
                                      )),
                               column(4,
                                      div(title = "m/z ppm match tolerance; features have to match within either mzdiff or ppm tolerance. Also used as tolerance for peak matching for MS2 similarity calculation.",
                                          numericInput(ns("ppm"),"ppm", value = 5, min = 0)
                                      ))),
                           fluidRow(
                               column(3,
                                      div(title = "Get MS2 scan similarity for matched features",
                                          checkboxInput(ns("getcosine"), "Calculate MS2 scan similarities", value  = TRUE)
                                      )),
                               column(3,
                                      div(title = "Use MS2 scan similarity cutoff to match features",
                                          checkboxInput(ns("usecosine"), "use Cosine cutoff", value  = TRUE)
                                      )),
                               column(3,
                                      div(title = "MS2 similarity threshold (cosine, range 0 (unrelated) - 1 (identical); minimum cosine value that has to be met to match features.",
                                          numericInput(ns("cosinethreshold"),"Cosine cutoff", value = 0.5, min = 0)
                                      )),
                               column(3,
                                      div(title = "Number of peaks that have to match between reference and query spectra in MS2 similarity matching",
                                          numericInput(ns("peaknum"),"min. matched peaks", value = 4, min = 0)
                                      ))),
                           fluidRow(
                             column(5),
                             column(1,
                                    actionButton(ns("matchReference"), "Go")
                             )
                           )
                           
                         ))      }),
                       static = list(tooltip = "Match a feature table to a network or feature table",
                                     title = "Match to reference", 
                                     label = "",
                                     icon = icon("handshake-o", lib = "font-awesome")))
  
  
  
  
  observeEvent(input$matchReference,{
    tryCatch({
      withProgress(message = 'Please wait!', detail = "Calculating peak intensities", value = 0.5, {
          updateFT(values)
          
          values$Networks[[paste0("matched_",input$selectreference)]] <- matchReference(values$Networks[[input$selectreference]],
                                                                     FeatureTable(values, tableID = input$selectquery), 
                                                                     parent_mztol = if(input$mzcheck){input$mzdiff}else{NULL},
                                                                     parent_ppm = input$ppm, 
                                                                     getCosine = input$getcosine,
                                                                     cosineThreshold = if(input$usecosine){input$cosinethreshold}else{NULL},
                                                                     rttol = if(input$rtcheck){input$rttol}else{NULL},
                                                                     mztol = input$mzdiff, ppm = input$ppm,
                                                                     minpeaks = input$peaknum)
            
                                                                     values$Networks$numNetworks <- values$Networks$numNetworks +1
                                                                                                                              
          })
       
      
          if(hasError(previousStep(values$Networks[[input$selectreference]]))){
              showNotification(paste("An error occured: ",
                                     unlist(error(previousStep(values$Networks[[input$selectreference]])))),
                               duration = 0, type = "error")
              
          }else{
          removeModal()
          showNotification(paste("Matched query to reference."), duration = 0, type = "message")
          }
        }
      ,
      error = function(e){
        
        showNotification(paste("An error occured: ", e), duration = 0, type = "error")
        
        
      })
    
  })
  
  
}

#' @describeIn MatchReferenceModule server logic
#' @export
MatchReferenceModuleUI <- function(id)
{
  ns <- NS(id)
  
  ModalWidgetUI(ns("modbutton"))
  
}