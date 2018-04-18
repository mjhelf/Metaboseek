selections <- reactiveValues(plots = list(chrom1 = list(xrange = NULL,
                                                        yrange = NULL,
                                                        maxxrange = NULL,
                                                        maxyrange = NULL,
                                                        marker = NULL,
                                                        mz = NULL,
                                                        tic = T),
                                          spec1 = list(xrange = NULL,
                                                       yrange = NULL,
                                                       maxxrange = NULL,
                                                       maxyrange = NULL,
                                                       marker = NULL,
                                                       data = NULL)
),
index = c("chrom1", "spec1"),
lastChangedEIC = "chrom1"
)

iEIC2 <- callModule(EICmodule,"EIC2", tag = "EIC2", set= reactive({ 
                                                                        list(layouts = MSData$layouts, #List of rawfile paths (unsorted)
                                                                        RTcorr = MSData$RTcorr,
                                                                        activeGroup = MSData$active,
                                                                        filelist = MSData$filelist,
                                                                        data = MSData$data,
                                                                        mz = if(is.null(maintabsel())){NULL}else{hot_to_r(input$maintable)[maintabsel()$rrng[1],"mz"]},
                                                                        rtr = if(is.null(maintabsel())){NULL}else{c((hot_to_r(input$maintable)[maintabsel()$rrng[1],"rt"]-MSData$layouts[[MSData$active]]$settings$rtw)/60,
                                                                                                                    (hot_to_r(input$maintable)[maintabsel()$rrng[1],"rt"]+MSData$layouts[[MSData$active]]$settings$rtw)/60)},
                                                                        active = if(length(MSData$data) ==0){F}else{T}
)}),
keys = reactive({keyin$keyd}))

iSpec2 <- callModule(Specmodule,"Spec2", tag = "Spec2", 
                     set = reactive({
                       
                       list(spec = list(xrange = if(length(iEIC2()$chrom1$mz) < 1 || is.na(iEIC2()$chrom1$mz) || is.null(iEIC2()$chrom1$mz)){
                       NULL}
                       else{c(iEIC2()$chrom1$mz-10,iEIC2()$chrom1$mz+10)},
                                                      yrange = NULL,
                                                      maxxrange = NULL,
                                                      maxyrange = NULL,
                                                      sel = if(length(iEIC2()$chrom1$mz) < 1 || is.na(iEIC2()$chrom1$mz) || is.null(iEIC2()$chrom1$mz)){
                                                        NULL}
                                                        else{list(File = iEIC2()$chrom1$marker$File[1],
                                                                 scan = iEIC2()$chrom1$marker$scan[1],
                                                                 rt = iEIC2()$chrom1$marker$rt[1]*60)},
                                                      data = NULL,
                                                      mz = iEIC2()$chrom1$mz),
                                          layout = list(lw = 1,
                                                        cex = 1.5,
                                                        controls = F,
                                                        ppm = MSData$layouts[[MSData$active]]$settings$ppm,
                                                        active =T),
                                          msdata = MSData$data)
                     }), 
                     keys = reactive({keyin$keyd})
)