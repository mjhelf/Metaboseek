#' PeptideModificationsModule
#' 
#' This module allows setting fixed and variable peptide modifications to be 
#' applied in peptide-related calculations (fragment annotation and peptide mass calculations)
#' 
#' @details Does not return anything, but
#' \subsection{initiates}{
#' \code{values$GlobalOpts$Peptides.variableMods}, \code{values$GlobalOpts$Peptides.fixedMods}
#' }
#' \subsection{modifies}{
#' \code{values$GlobalOpts$Peptides.variableMods}, \code{values$GlobalOpts$Peptides.fixedMods}
#' }
#' \subsection{reads}{
#' \code{values$GlobalOpts$Peptides.variableMods}, \code{values$GlobalOpts$Peptides.fixedMods}
#' }
#' @inherit MseekModules
#' 
#' @describeIn PeptideModificationsModule server logic for PeptideModificationsModule
#'
#' @export
PeptideModificationsModule <- function(input,output, session, values){
    
    ns <- NS(session$ns(NULL))
    
    internalValues <- reactiveValues(variableTrigger = 1,
                                     Peptides.variableMods = if(file.exists(file.path(system.file("config", package = "Metaboseek"), "variableMods.tsv"))){
                                         fixme <- data.table::fread(file.path(system.file("config", package = "Metaboseek"), "variableMods.tsv"))
                                         #making sure no problems arise from empty columns(which would be loaded as logical)
                                         fixme$active <- as.logical(fixme$active)
                                         
                                         fixme$tag <- as.character(fixme$tag)
                                         fixme$AAs <- as.character(fixme$AAs)
                                         fixme$formula <- as.character(fixme$formula)
                                         
                                         fixme$min <- as.numeric(fixme$min)
                                         fixme$max <- as.numeric(fixme$max)
                                         fixme$mass <- as.numeric(fixme$mass)
                                         as.data.frame(fixme, stringsAsFactors = FALSE)
                                     }else{fixme <- data.frame(active = TRUE,
                                                               tag = "Ep",
                                                               min = 0,
                                                               max = 2,
                                                               AAs = "",
                                                               formula = "",
                                                               mass = 2.0141017778 - 1.0078250321,
                                                               stringsAsFactors = FALSE)
                                     fixme
                                     },
                                     Peptides.fixedMods = data.frame(active = T,
                                                                     AA = "C",
                                                                     mass = 57.02146)
    )
    
    observeEvent(values$GlobalOpts,{
        
        
        prevariable <- internalValues$Peptides.variableMods[(!is.na(internalValues$Peptides.variableMods$active)
                                                             & !is.na(internalValues$Peptides.variableMods$tag)
                                                             & !is.na(internalValues$Peptides.variableMods$min)
                                                             & !is.na(internalValues$Peptides.variableMods$max)
                                                             & !is.na(internalValues$Peptides.variableMods$mass)),
                                                            ]
        
        
        
        values$GlobalOpts$Peptides.variableMods <- prevariable[prevariable$active,]
        
        
        
        prefixed <- na.omit(internalValues$Peptides.fixedMods)
        
        prefixed <- prefixed[(prefixed$active
                              & prefixed$AA != ""),]
        
        prefixed2 <- prefixed$mass
        
        names(prefixed2) <- prefixed$AA
        
        values$GlobalOpts$Peptides.fixedMods <-  prefixed2
        
        
        
    }, once = T)
    
    fixedTab <- callModule(simpleTableModule,'fixedtab', 
                           df = reactive({internalValues$Peptides.fixedMods}),
                           static = list(readOnly = F,
                                         contextMenu = T,
                                         height = "auto")
    )
    
    
    variableTab <- callModule(simpleTableModule,'variabletab', 
                              df = reactive({internalValues$Peptides.variableMods}),
                              static = list(readOnly = F,
                                            contextMenu = T,
                                            height = "auto")
    )
    
    callModule(SaveTableModule, "savevariable",
               reactives = reactive({list(df = variableTab$liveView,
                                          filename = file.path("Table_grouping",
                                                               paste0("variableModifications.tsv")))}),
               values = reactiveValues(projectData = values$projectData),
               static = list(tooltip = "Save variable modification table",
                             label = "Save",
                             format = c("tsv"))
    )
    
    TableLoader <- callModule(LoadTableModule, "loadvariable",
                              values = reactiveValues(projectData = values$projectData,
                                                      featureTables = NULL),
                              static = list(tooltip = "Load variable modification table",
                                            label = "Load",
                                            format = list(header = T,
                                                          sep = "\t",
                                                          quote = '"',
                                                          stringsAsFactors = F),
                                            pattern = "\\.tsv$")
    )
    
    
    
    
    observeEvent(TableLoader$df,{
        
        if(!is.null(TableLoader$df)){
            
            fixme <- TableLoader$df
            
            #making sure no problems arise from empty columns(which would be loaded as logical)
            fixme$active <- as.logical(fixme$active)
            
            fixme$tag <- as.character(fixme$tag)
            fixme$AAs <- as.character(fixme$AAs)
            fixme$formula <- as.character(fixme$formula)
            
            fixme$min <- as.numeric(fixme$min)
            fixme$max <- as.numeric(fixme$max)
            fixme$mass <- as.numeric(fixme$mass)
            
            
            
            internalValues$Peptides.variableMods <- fixme
            internalValues$variableTrigger <- internalValues$variableTrigger + 1
        }
    })
    
    # observeEvent(input$abutton,{
    #     
    #     internalValues$variableTrigger <- internalValues$variableTrigger + 1})
    
    observeEvent(c(variableTab$liveView),{
        tryCatch({
            
            prevariable <- variableTab$liveView[(!is.na(variableTab$liveView$active)
                                                 & !is.na(variableTab$liveView$tag)
                                                 & !is.na(variableTab$liveView$min)
                                                 & !is.na(variableTab$liveView$max)
                                                 & !is.na(variableTab$liveView$mass)),
                                                ]
            
            
            values$GlobalOpts$Peptides.variableMods <- prevariable[prevariable$active,]
            
            
            
        }, error = function(e){warning(e)})
        
    }, ignoreNULL = T)
    
    
    observeEvent(c(fixedTab$liveView),{
        tryCatch({
            
            prefixed <- na.omit(fixedTab$liveView)
            
            prefixed <- prefixed[(prefixed$active
                                  & prefixed$AA != ""),]
            
            prefixed2 <- prefixed$mass
            
            names(prefixed2) <- prefixed$AA
            
            values$GlobalOpts$Peptides.fixedMods <-  prefixed2
            
        }, error = function(e){warning(e)})
        
    }, ignoreNULL = T)
    
}

#' @describeIn PeptideModificationsModule UI elements for PeptideModificationsModule
#'
#' @export
PeptideModificationsModuleUI <- function(id){
    ns <- NS(id)
    
    fluidPage(
        fluidRow(
            box(width = 8,
                div(class = "centerContainer", h4("Variable Modifications")),
                fluidRow(
                    div(class = "centerContainer",
                        LoadTableModuleUI(ns("loadvariable")),
                        SaveTableModuleUI(ns("savevariable"))
                    )
                ),
                
                fluidRow(
                    div(class = "centerContainer",
                        simpleTableModuleUI(ns("variabletab"))))
            ),
            box(width = 4, fluidRow(div(class = "centerContainer",
                                        h4("Fixed Modifications"))),
                fluidRow(div(class = "centerContainer",
                             simpleTableModuleUI(ns("fixedtab")))))
        )#,   
        # fluidRow(actionButton(ns("abutton"), "Update Modifications"))
    )
    
}