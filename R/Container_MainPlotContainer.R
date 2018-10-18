#' MainPlotContainer
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param values Import data from the shiny session
#' 
#' @export 
MainPlotContainer <- function(input,output, session,
                              values = reactiveValues(projectData = projectData,
                                                      featureTables = featureTables,
                                                      MSData = MSData,
                                                      GlobalOpts = GlobalOpts,
                                                      MainTable = MainTable),
                              keys = reactive({keyin$keyd})
){
  
  ns <- NS(session$ns(NULL))
  
  
  RegroupMS <- callModule(RegroupMSDataModule, "regroupms",
                          values = reactiveValues(MSData = values$MSData,
                                                  projectData = values$projectData))
  
  GroupedEICs <- callModule(GroupedEICModule, "groupedeics",
                            values = reactiveValues(projectData = values$projectData,
                                                    featureTables = values$featureTables,
                                                    MSData = values$MSData,
                                                    MainTable = values$MainTable,
                                                    GlobalOpts = values$GlobalOpts),
                            keys = reactive({keys()})
  )
  
  #### MS2Browsewr #####
  
  MS2Browser <- callModule(MS2BrowserModule, 'MS2B', tag = ns("MS2B"), 
                           set = reactive({list(MSData = values$MSData$data,
                                                query = list(mz = if(is.null(values$MainTable$selected_rows)){NULL}else{values$MainTable$liveView[values$MainTable$selected_rows[1],"mz"]},
                                                             rt = if(is.null(values$MainTable$selected_rows)){NULL}else{values$MainTable$liveView[values$MainTable$selected_rows[1],"rt"]}
                                                ))}),
                           keys = reactive({keys()}))
  
  #### Quickplots #####
  callModule(featurePlotModule, "quickplots",
             FT = reactive({values$featureTables$tables[[values$featureTables$active]]}),
             rname = reactive({row.names(values$MainTable$liveView[values$MainTable$selected_rows[1],])})
  )
  
  #### interactiveView #####
  MultiEICout <- callModule(MultiEICmodule,"MultiE", values = reactiveValues(MSData = values$MSData,
                                                                                                     GlobalOpts = values$GlobalOpts,
                                                                                                     MainTable = values$MainTable),
                            keys = reactive({keys()}))
  
  iSpec2 <- callModule(MultiSpecmodule,"Spec2", tag = ns("Spec2"), 
                       set = reactive({
                         
                         
                         list(spec = list(xrange = if(length(MultiEICout$currentView$controls$mz) < 1 || is.na(MultiEICout$currentView$controls$mz)){
                           NULL}
                           else{c(MultiEICout$currentView$controls$mz-10,MultiEICout$currentView$controls$mz+10)},
                           yrange = NULL,
                           maxxrange = NULL,
                           maxyrange = NULL,
                           sel = if(length(MultiEICout$currentView$controls$marker$file) < 1 || is.na(MultiEICout$currentView$controls$marker$file) ){
                             NULL}
                           else{list(File = MultiEICout$currentView$controls$marker$file,
                                     scan = MultiEICout$currentView$controls$marker$scan,
                                     rt = MultiEICout$currentView$controls$marker$rt*60)},
                           data = NULL,
                           mz = MultiEICout$currentView$controls$mz,
                           MS2 = F),
                           layout = list(lw = 1,
                                         cex = 1.5,
                                         controls = F,
                                         ppm = values$GlobalOpts$PPMwindow,
                                         active =T,
                                         highlights = NULL,
                                         height = 350),
                           msdata = values$MSData$data)
                       }), 
                       keys = reactive({keys()}),
                       static = list(title = "MS spectra")
  )
  
  PcaViewFeatures <- callModule(PcaViewModule, "pcaviewfeatures",
                                values = reactiveValues(featureTables = values$featureTables)
  )
  
  internalValues <- reactiveValues(RegroupMS = RegroupMS,
                                   GroupedEICs = GroupedEICs,
                                   MS2Browser = MS2Browser,
                                   MultiEICout = MultiEICout,
                                   iSpec2 = iSpec2,
                                   PcaViewFeatures = PcaViewFeatures)

  return(internalValues)
  
}

#' MainPlotContainerUI
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param id
#' 
#' @export
MainPlotContainerUI <- function(id){
  ns <- NS(id)
  
  tabBox(title = "Data viewer",
         id = "EICplots", width = 12, side = "right", selected = "Grouped EICs",
         
         tabPanel("_"),
         tabPanel("Regroup MS data",
                  RegroupMSDataModuleUI(ns("regroupms"))
         ),
         tabPanel("Grouped EICs",
                  GroupedEICModuleUI(ns("groupedeics"))
         ),
         tabPanel("MS Browser",
                  fluidPage(
                    MultiEICmoduleUI(ns("MultiE")),
                    MultiSpecmoduleUI(ns("Spec2"))
                  )
         ),
         tabPanel("Quickplots",
                  featurePlotModuleUI(ns("quickplots"))
         ),
         tabPanel("PCA Viewer",
                  PcaViewModuleUI(ns("pcaviewfeatures"))
         ),
         tabPanel("MS2 Browser",
                  MS2BrowserModuleUI(ns('MS2B'))
         )
         
  )
  
  
}