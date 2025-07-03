#' PeptideCalcModule
#' 
#' This module generates a new feature table with mz values based on peptide sequences
#' and charges specified in the current FeatureTable, and also applies variable and fixed peptide modifications.
#' 
#' @inherit MseekModules
#' @details 
#' Does not return anything, but
#' \subsection{requires}{
#' \code{PeptideModificationsModule}
#' }
#' \subsection{modifies}{
#' \code{FeatureTable()}, \code{activeFT(values)}
#' }
#' \subsection{reads}{
#' \code{values$GlobalOpts$Peptides.variableMods},
#'  \code{values$GlobalOpts$Peptides.fixedMods},
#'  \code{FeatureTable(values)$df}
#' }
#' 
#' @describeIn PeptideCalcModule server logic for PeptideCalcModule
#'
#' @importFrom MassTools getExactMass PeptideMF permutatePeptideMass
#' @export
PeptideCalcModule <- function(input,output, session, values){
    
    ns <- NS(session$ns(NULL))
    
    dialog <- callModule(ModalWidget, "calcbutton",
                         reactives = reactive({  
                             list(fp = fluidPage(
                                 fluidRow(
                                     selectizeInput(ns("seqsel"), "Column with peptide sequences:",
                                                    choices = colnames(FeatureTable(values)$df),
                                                    selected = grep("seq",colnames(FeatureTable(values)$df), value = T)[1],
                                                    multiple = F,
                                                    options = list(maxOptions = 10000)),
                                     selectizeInput(ns("chargesel"), "Column with peptide charges:",
                                                    choices = colnames(FeatureTable(values)$df),
                                                    selected = grep("charge",colnames(FeatureTable(values)$df), value = T)[1],
                                                    multiple = F,
                                                    options = list(maxOptions = 10000)),
                                     checkboxInput(ns("checkptms"),"Use modifications", value = T),
                                     
                                     actionButton(ns("abutton"), "Get peptide mass")
                                 )
                             )
                             ) 
                         }),
                         static = list(tooltip = "Calculate peptide masses",
                                       title = "Calculate peptide masses", 
                                       label = "Get peptide masses",
                                       icon = icon("calculator", lib = "font-awesome")))
    
    observeEvent(input$abutton,{
        tryCatch({
            
            tempdf <- FeatureTable(values)$df
            
            #tempdf <- tab2$df
            
            
            tempdf$mz <- sapply(MassTools::PeptideMF(tempdf$seq),  getExactMass)
            
            if(length(values$GlobalOpts$Peptides.fixedMods)){
                
                AAcounts <- MassTools::countAAs(tempdf$seq)
                
                
                
                tempdf$mz <- tempdf$mz + sapply(AAcounts,function(x){
                    if(length(x)){
                m <- match(names(x), names(values$GlobalOpts$Peptides.fixedMods))
                
                if(any(!is.na(m))){
                return(sum(x[!is.na(m)] * values$GlobalOpts$Peptides.fixedMods[na.omit(m)]))
                }
                    }
                    return(0)
            })
            }
            
            tempdf$mz <- (tempdf$mz + tempdf$charge*1.007276467)/tempdf$charge
            
            if(input$checkptms){
                tempdf <- MassTools::permutatePeptideMass(tempdf,
                                                          values$GlobalOpts$Peptides.variableMods,
                                                          chargeCol = input$chargesel,
                                                          sequenceCol = input$seqsel)
            }
            
            newID <- paste0("table", length(values$featureTables$tables))
            
            FeatureTable(values,
                         tableID = newID,
                         replace = T,
                         update = F) <- tempdf
            
            activeFT(values) <- newID
            
            removeModal()
            
            
        }, error = function(e){print(e)})
        
    })
    
}

#' @describeIn PeptideCalcModule UI elements for PeptideCalcModule
#' @export
PeptideCalcModuleUI <- function(id){
    ns <- NS(id)
    ModalWidgetUI(ns("calcbutton"))
}