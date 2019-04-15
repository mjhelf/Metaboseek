#' OptionsContainer
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values Import data from the shiny session
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
                                     MainTable = values$MainTable),
             reactives = reactive({list(mz = list("feature table" = if(is.null(values$MainTable$selected_rows)){NULL}else{values$MainTable$liveView[values$MainTable$selected_rows[1],"mz"]},
                                                          "spectrum" =  internalValues$activeMZ ))})
  )
  
  MassShifts <- callModule(MassShiftsModule, "massshifts",
                                 values = reactiveValues(MSData = values$MSData)
  )
  
  callModule(RtCorrViewModule, "rtcorrviewer",
                             values = reactiveValues(MSData = values$MSData,
                                                     GlobalOpts = values$GlobalOpts)
  )
  
  # MainDataLoad <- callModule(LoadDataModule, "maindataload",
  #                            values = reactiveValues(projectData = values$projectData,
  #                                                    featureTables = values$featureTables,
  #                                                    MSData = values$MSData,
  #                                                    GlobalOpts = values$GlobalOpts)
  # )
  
  callModule(EICOptionsModule, "eicopts", values = reactiveValues(GlobalOpts = values$GlobalOpts))
  
  callModule(SiriusOptionsModule, "siriusopts", values = reactiveValues(GlobalOpts = values$GlobalOpts))
  
  
  # callModule(GlobalOptionsModule, "globalopts", values = reactiveValues(GlobalOpts = values$GlobalOpts))
  
  internalValues <- reactiveValues(
    #MainDataLoad = MainDataLoad
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
  
  return(internalValues)
  
}

#' OptionsContainerUI
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param id
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