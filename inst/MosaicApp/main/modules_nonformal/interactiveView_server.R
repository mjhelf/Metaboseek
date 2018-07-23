
MultiEICout <- callModule(MultiEICmodule,"MultiE", values = reactiveValues(MSData = MSData),
keys = reactive({keyin$keyd}))

iSpec2 <- callModule(MultiSpecmodule,"Spec2", tag = "Spec2", 
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
                                       ppm = MSData$layouts[[MSData$active]]$settings$ppm,
                                       active =T,
                                       highlights = NULL,
                                       height = 350),
                         msdata = MSData$data)
                     }), 
                     keys = reactive({keyin$keyd}),
                     static = list(title = "MS spectra")
)
