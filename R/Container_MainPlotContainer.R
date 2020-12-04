#' MainPlotContainer
#' 
#' Module that contains most plotting devices in Metaboseek
#' 
#' @inherit MseekContainers
#' @describeIn MainPlotContainer server logic module, to be called with \link[shiny]{callModule}()
#' 
#' @export 
MainPlotContainer <- function(input,output, session,
                              values = reactiveValues(projectData = NULL,
                                                      featureTables = NULL,
                                                      MSData = NULL,
                                                      GlobalOpts = NULL)
                              ){
  
  ns <- NS(session$ns(NULL))
  
  
      callModule(RegroupMSDataModule, "regroupms",
                          values = reactiveValues(MSData = values$MSData,
                                                  projectData = values$projectData))
  
  GroupedEICs <- callModule(GroupedEICModule, "groupedeics",
                            values = reactiveValues(projectData = values$projectData,
                                                    featureTables = values$featureTables,
                                                    MSData = values$MSData,
                                                    GlobalOpts = values$GlobalOpts),
                            keys = reactive({values$GlobalOpts$keyinput.keydown})
  )
  
  
  #### Ratio Plot #####
  callModule(RatioPlotModule, "ratioplots", values)
  
  #### Quickplots #####
  callModule(QuickPlotsModule, "quickplots", values)
  
  
  #### MS2Browsewr #####
  MS2Browser <- callModule(MS2BrowserModule, 'MS2B', 
                           values = reactiveValues(featureTables = values$featureTables,
                                                   MSData = values$MSData,
                                                   GlobalOpts = values$GlobalOpts,
                                                   projectData = values$projectData),
                           keys = reactive({values$GlobalOpts$keyinput.keydown}))
  #### interactiveView #####
  MultiEICout <- callModule(MultiEICmodule,"MultiE", values)
  
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
                       keys = reactive({values$GlobalOpts$keyinput.keydown}),
                       static = list(title = "MS spectra")
  )
  
  callModule(PcaViewModule, "pcaviewfeatures",
                                values = reactiveValues(featureTables = values$featureTables)
  )
  
      callModule(VennDiagramModule, "venndiagrams", values = reactiveValues(featureTables = values$featureTables,
                                                                           GlobalOpts = values$GlobalOpts))
  
  ###TODO: remove return values. currently returns modules with SpecModule for mzquerymodule
  internalValues <- reactiveValues(#RegroupMS = RegroupMS,
                                   GroupedEICs = GroupedEICs,
                                   MS2Browser = MS2Browser,
                                   #MultiEICout = MultiEICout,
                                   iSpec2 = iSpec2#,
                                   #PcaViewFeatures = PcaViewFeatures,
                                   #VennDiagrams = VennDiagrams
                                   )

  return(internalValues)
  
}

#' @describeIn MainPlotContainer returns the \code{shiny} UI elements for the Main plot box ("Data viewer"), including the box
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
         tabPanel("Ratio Plot",
                  RatioPlotModuleUI(ns("ratioplots"))
         ),
         tabPanel("Quickplots",
                  QuickPlotsModuleUI(ns("quickplots"))
         ),
         tabPanel("MS Browser",
                  fluidPage(
                    MultiEICmoduleUI(ns("MultiE")),
                    MultiSpecmoduleUI(ns("Spec2"))
                  )
         ),
         tabPanel("Venn Diagrams",
                  VennDiagramModuleUI(ns("venndiagrams"))
         ),
         tabPanel("PCA Viewer",
                  PcaViewModuleUI(ns("pcaviewfeatures"))
         ),
         tabPanel("MS2 Browser",
                 
                  MS2BrowserModuleUI(ns('MS2B'))
         )
         
  )
  
  
}