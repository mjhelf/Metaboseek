#' PeptideAnnotationModule
#' 
#' This module allows setting fixed and variable peptide modifications to be 
#' applied in peptide-related calculations (fragment annotation and peptide mass calculations)
#' 
#' @details 
#' Does not return anything, but
#' \subsection{requires}{
#' \code{PeptideModificationsModule},
#'  \code{SelectMS2Module},
#'  \code{MainTableModule}
#' } 
#' \subsection{initiates}{
#' nothing
#' }
#' \subsection{reads}{
#' \code{values$GlobalOpts$Peptides.variableMods},
#'  \code{values$GlobalOpts$Peptides.fixedMods},
#'  \code{FTselection()}
#' }
#' \subsection{modifies}{
#' \code{values$MSData$MS2.selected}
#' }
#' 
#' 
#' @inherit MseekModules
#' 
#' @describeIn PeptideAnnotationModule server logic for PeptideAnnotationModule
#' 
#' @import MassTools
#'
#' @export
PeptideAnnotationModule <- function(input,output, session, values){
    
    ns <- NS(session$ns(NULL))
    
    internalValues <- reactiveValues(fragmentCache = list(),
                                     trigger = 0)
    
    observeEvent(c(values$MSData$MS2.selected, input$mzcheck),{
        if(!is.null(input$autocalc) & input$autocalc){
            internalValues$trigger <- internalValues$trigger + 1
        }
    })
    observeEvent(input$annotateButton,{
        internalValues$trigger <- internalValues$trigger + 1
        
    })
    
    observeEvent(internalValues$trigger,{
        
        if(!is.null(FTselection(values))){
            
            tryCatch({
                
                if(!identical(values$GlobalOpts$Peptides.fixedMods,internalValues$fragmentCache$fixedMods)
                   |!identical(values$GlobalOpts$Peptides.variableMods,internalValues$fragmentCache$variableMods)
                   |!identical(FTselection(values)$seq[1],internalValues$fragmentCache$sequence)
                ){
                    
                    unmodfrags <- MSnbase::calculateFragments(sequence = FTselection(values)$seq[1],
                                                              neutralLoss = list(water = character(0), ammonia = character(0)),
                                                              modifications = values$GlobalOpts$Peptides.fixedMods,
                                                              z = 1)
                    
                    fragments <- permutatePeptideMass(unmodfrags, values$GlobalOpts$Peptides.variableMods )
                    
                    #this caching will be worth it if there are many variable modifications
                    internalValues$fragmentCache <- list(fragments = fragments,
                                                         fixedMods = values$GlobalOpts$Peptides.fixedMods,
                                                         variableMods = values$GlobalOpts$Peptides.variableMods,
                                                         sequence = FTselection(values)$seq[1])
                }
                
                values$MSData$MS2.selected$labels <- annotateSpectrum(internalValues$fragmentCache$fragments,
                                                                      values$MSData$MS2.selected$spectrum,
                                                                      mzlabel = input$mzcheck,
                                                                      ppm = input$ppmmatch,
                                                                      abs = 0)
                values$MSData$MS2.selected$parseLabels <- TRUE
                values$MSData$MS2.selected$k <- 0
                
            },
            error = function(e){
                print("ERROR")
                print(e)
                
                internalValues$foundpeaks_annotated <- NULL
            })
            
        }else{
            
            internalValues$foundpeaks_annotated <- NULL
        }
    }, ignoreInit = T)
    
    callModule(PeptideCalcModule, "pepcalc", values)
    
    callModule(SelectMS2Module, "ms2sel", values)
    
    callModule(PeptideSequencePlotWidget, "pepseq",
               reactives = reactive({
                   if(!is.null(values$MSData$MS2.selected$labels)){
                       
                       #sorted by intensity, will show hoghest intensity ions as rank 1, second highest in rank 2
                       list(peaklabels = values$MSData$MS2.selected$labels,
                            sequence = FTselection(values)$seq[1],
                            parseLabels = F,
                            sortby = "intensitySpec",
                            cx = 2.2)
                   }
                   
               }),
               layout = reactive({list(active = T,
                                       height = 650)}))
    
    callModule(SpecModule2, "specview1", values,
               reactives = reactive({
                   values$MSData$MS2.selected
               }))
    
    output$pdfButton <- downloadHandler(filename= function(){
        titleout <- if(length(values$MSData$MS2.selected$specinfo$filename) == 1){
            paste0(FTselection(values)$seq[1],"_",FTselection(values)$modifications[1],
                   "_",values$MSData$MS2.selected$filename,"_",values$MSData$MS2.selected$acquisistionNum)
        }else{"averagedSpectra"}
        
        return(paste0(titleout,".pdf"))}, 
        content = function(file){
            
            pdf(file, height=11.69, width=8.27)
            layout(matrix(c(1,2), ncol = 1), heights = c(1.5,1))
            
            
            if(length(values$MSData$MS2.selected$labels)){
                do.call(plotAnnotatedPeptide, list(peaklabels = values$MSData$MS2.selected$labels,
                                                   sequence = FTselection(values)$seq[1],
                                                   parseLabels = F,
                                                   sortby = "intensitySpec",
                                                   cx = 2,
                                                   yoffset = -0.1 ))
            }else{
                plot(numeric(),
                     numeric(),
                     ylim = c(-1,1),
                     xlim = c(0,1),
                     xaxs = "i", yaxs = "i",
                     type = "n", ann = FALSE, bty = "n",
                     axes = F#, asp =0.5
                )
            }
            
            
            do.call(specplot2, c(list(fileName = parseTitle(values$MSData$MS2.selected$specinfo)),
                                 values$MSData$MS2.selected[!names(values$MSData$MS2.selected) %in% c("specinfo","scantable","type", "spectra")]))
            
            dev.off()
            
        },
        contentType = "application/pdf")
    
    output$pdfButtonAll <- downloadHandler(filename= function(){
        titleout <- if(length(values$MSData$MS2.selected$specinfo$filename) == 1){
            paste0(FTselection(values)$seq[1],"_",FTselection(values)$modifications[1],
                   "_",values$MSData$MS2.selected$filename,"_",values$MSData$MS2.selected$acquisistionNum)
        }else{"spectra_views"}
        
        return(paste0(titleout,".pdf"))}, 
        content = function(file){
            pdf(file, height=11.69, width=8.27)
            try({
                
                if(length(values$MSData$MS2.selected$spectra)){
                    
                    if(!is.null(FTselection(values)$seq)){
                        
                        if(!identical(values$GlobalOpts$Peptides.fixedMods,internalValues$fragmentCache$fixedMods)
                           |!identical(values$GlobalOpts$Peptides.variableMods,internalValues$fragmentCache$variableMods)
                           |!identical(FTselection(values)$seq[1],internalValues$fragmentCache$sequence)){
                            
                            unmodfrags <- MSnbase::calculateFragments(sequence = FTselection(values)$seq[1],
                                                                      neutralLoss = list(water = character(0), ammonia = character(0)),
                                                                      modifications = values$GlobalOpts$Peptides.fixedMods,
                                                                      z = 1)
                            
                            fragments <- permutatePeptideMass(unmodfrags, values$GlobalOpts$Peptides.variableMods )
                            
                            #this caching will be worth it if there are many variable modifications
                            internalValues$fragmentCache <- list(fragments = fragments,
                                                                 fixedMods = values$GlobalOpts$Peptides.fixedMods,
                                                                 variableMods = values$GlobalOpts$Peptides.variableMods,
                                                                 sequence = FTselection(values)$seq[1])
                        }
                    }
                    
                    tempvals <- values$MSData$MS2.selected
                    
                    for(i in seq(length(values$MSData$MS2.selected$spectra))){
                        tryCatch({
                            
                            layout(matrix(c(1,2), ncol = 1), heights = c(1.5,1))
                            tempvals$spectrum <- values$MSData$MS2.selected$spectra[[i]]
                            #  tempvals$specinfo <-  
                            tempvals$fileName = parseTitle(values$MSData$MS2.selected$specinfo[i,])
                            
                            if(!is.null(internalValues$fragmentCache$fragments) 
                               && nrow(internalValues$fragmentCache$fragments) > 0){
                                tempvals$labels <- annotateSpectrum(internalValues$fragmentCache$fragments,
                                                                    tempvals$spectrum,
                                                                    mzlabel = input$mzcheck,
                                                                    ppm = input$ppmmatch,
                                                                    abs = 0)
                                tempvals$parseLabels <- TRUE
                                tempvals$k <- 0
                            }else{
                                tempvals$labels <- NULL
                                tempvals$parseLabels <- NULL
                                tempvals$k <- NULL}
                            
                            
                            if(length(tempvals$labels)){
                                do.call(plotAnnotatedPeptide, list(peaklabels = tempvals$labels,
                                                                   sequence = FTselection(values)$seq[1],
                                                                   parseLabels = F,
                                                                   sortby = "intensitySpec",
                                                                   cx = 2,
                                                                   yoffset = -0.1 ))
                            }else{
                                plot(numeric(),
                                     numeric(),
                                     ylim = c(-1,1),
                                     xlim = c(0,1),
                                     xaxs = "i", yaxs = "i",
                                     type = "n", ann = FALSE, bty = "n",
                                     axes = F#, asp =0.5
                                )
                            }
                            do.call(specplot2, tempvals[!names(tempvals) %in% c("specinfo","scantable","type", "spectra")])
                        },
                        error = function(e){
                            
                            plot(numeric(),
                                 numeric(),
                                 ylim = c(-1,1),
                                 xlim = c(0,1),
                                 xaxs = "i", yaxs = "i",
                                 type = "n", ann = FALSE, bty = "n",
                                 axes = F#, asp =0.5
                            )
                            
                            plot(numeric(),
                                 numeric(),
                                 ylim = c(-1,1),
                                 xlim = c(0,1),
                                 xaxs = "i", yaxs = "i",
                                 type = "n", ann = FALSE, bty = "n",
                                 axes = F#, asp =0.5
                            )
                            
                            text(0.5,0, labels = "Something went wrong with this plot.", adj = 0.5)  
                            
                            warning(e)
                            
                        })
                        
                    } 
                }
            })
            dev.off()
            
        },
        contentType = "application/pdf")
    
}

#' @describeIn PeptideAnnotationModule UI elements for PeptideAnnotationModule
#' @export
PeptideAnnotationModuleUI <- function(id){
    
    ns <- NS(id)
    
    fluidRow(
        column(width = 5,
               SelectMS2ModuleUI(ns("ms2sel"))
        ),
        
        column(width = 7,
               
               fluidRow(
                   div(class = "centerContainer",
                       PeptideCalcModuleUI(ns("pepcalc")),
                       actionButton(ns("annotateButton"), "annotate Peptide"),
                       checkboxInput(ns("autocalc"), "Calculate automatically"),
                       numericInput(ns("ppmmatch"), "match ppm tolerance", value = 5)
                   )),
               fluidRow(
                   div(class = "centerContainer",
                       checkboxInput(ns("mzcheck"), "show mz in spectrum"),
                       downloadButton(ns('pdfButton'), "Save current view"),
                       downloadButton(ns('pdfButtonAll'), "Save individually",
                                      title = "Save a pdf file with all selected spectra individually annotated")
                       
                   )
               ),
               
               PeptideSequencePlotWidgetUI(ns("pepseq")),
               
               SpecModule2UI(ns("specview1")))
        
    )
    
}