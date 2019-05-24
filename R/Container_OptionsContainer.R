#' OptionsContainer
#' 
#' Container for modules in the Metaboseek Options box
#' 
#' @return The server module for this container returns nothing
#' 
#' @inherit MseekContainers
#' @describeIn OptionsContainer server logic module, to be called with \link[shiny]{callModule}()
#' 
#' @export 
OptionsContainer <- function(input,output, session,
                            values = reactiveValues(projectData = projectData,
                                                    featureTables = featureTables,
                                                    MSData = MSData,
                                                    GlobalOpts = GlobalOpts,
                                                    MainPlotBox = MainPlotBox,
                                                    MainTable = MainTable)
){
  ns <- NS(session$ns(NULL))
  
  
  
  
  callModule(MzqueryModule,"mzquery",
             values = reactiveValues(featureTables = values$featureTables,
                                     GlobalOpts = values$GlobalOpts,
                                     MainTable = values$featureTables$Maintable),
             reactives = reactive({list(mz = list("feature table" = if(is.null(values$featureTables$Maintable$selected_rows)){NULL}else{values$featureTables$Maintable$liveView[values$featureTables$Maintable$selected_rows[1],"mz"]},
                                                          "spectrum" =  internalValues$activeMZ,
                                                  "Spectrum2" = values$GlobalOpts$mzquery.SpectrumMz))})
  )
  
  MassShifts <- callModule(MassShiftsModule, "massshifts",
                                 values = reactiveValues(MSData = values$MSData)
  )
  
  callModule(RtCorrViewModule, "rtcorrviewer",
                             values = reactiveValues(MSData = values$MSData,
                                                     GlobalOpts = values$GlobalOpts)
  )
  
  
  callModule(EICOptionsModule, "eicopts", values = reactiveValues(GlobalOpts = values$GlobalOpts))
  
  callModule(SiriusOptionsModule, "siriusopts", values = reactiveValues(GlobalOpts = values$GlobalOpts))
  
  ###################TODO: REMOVE ALL CODE BELOW (Pending full SpecModule2 implementation)####
  internalValues <- reactiveValues(
    )
  
  observeEvent(values$MainPlotBox$GroupedEICs$iSpec1()$spec$marker$mz,{
    if(!is.null(values$MainPlotBox$GroupedEICs$iSpec1()$spec$marker$mz)){
      
    internalValues$activeMZ <- values$MainPlotBox$GroupedEICs$iSpec1()$spec$marker$mz
    }
  })
  
  observeEvent(values$MainPlotBox$iSpec2$marker$mz,{
   if(!is.null(values$MainPlotBox$iSpec2$marker$mz)){
    internalValues$activeMZ <- values$MainPlotBox$iSpec2$marker$mz
   }
  })
  
  
  observeEvent(values$MainPlotBox$MS2Browser$iSpec2$marker$mz,{
  if(!is.null(values$MainPlotBox$MS2Browser$iSpec2$marker$mz)){
    internalValues$activeMZ <- values$MainPlotBox$MS2Browser$iSpec2$marker$mz
    }
    })
  
 # return(internalValues)
  
}

#' @describeIn OptionsContainer returns the \code{shiny} UI elements for the METABOseek options box, including the surrounding box
#' 
#' @export
OptionsContainerUI <- function(id){
  ns <- NS(id)
 
  tabBox(title = "Options",
         id = "PlotOpts", width = 12, side = "right", selected = "EIC options",
         
         tabPanel("_"),
         # 
         # tabPanel("Load Data",
         #          LoadDataModuleUI(ns("maindataload"))
         # ),
         tabPanel("EIC options",
                  EICOptionsModuleUI(ns("eicopts"))
         ),
         
         tabPanel("Mass shifts",
                  MassShiftsModuleUI(ns("massshifts"))
         ),
         
         tabPanel("RT correction",
                  RtCorrViewModuleUI(ns("rtcorrviewer"))
                  ),
         
         tabPanel("Molecular formula prediction",
                  MzqueryModuleUI(ns("mzquery"))
         ),
         tabPanel("Sirius options",
                  SiriusOptionsModuleUI(ns("siriusopts"))
         )
         
         )
  
  
  
}