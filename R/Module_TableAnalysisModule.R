#' TableAnalysisModule
#' 
#' Module for Feature Table analysis
#' 
#' @inherit MseekModules
#' 
#' @describeIn TableAnalysisModule Server logic
#' 
#' @return Returns its internalValues
#' 
#' @import shiny
#' @importFrom shinyjs toggle
#' 
#' @export 
TableAnalysisModule <- function(input,output, session, values,       
                                reactives = reactive({list(fileGrouping = NULL)}), ##TODO: reactives probably not needed
                                static = list()
){
  #### Initialization ####
  
  ns <- NS(session$ns(NULL))
  
  FindMS2 <- callModule(FindMS2ScansModule, "findms2",
                        values = reactiveValues(featureTables = values$featureTables,
                                                MSData = values$MSData),
                        static = list(tooltip = "Find MS2 scans for all parent m/zs in feature table",
                                      label = "Find MS2 scans")
  )
  
  internalValues <- reactiveValues(normalize = T,
                                   useNormalized = T,
                                   logNormalized = F,
                                   controlGroups = NULL,
                                   
                                   normalizationFactors = NULL,
                                   zeroReplacement = NULL,
                                   replaceNAs = 0,
                                   
                                   analysesAvailable = list("Grouping required" = c("Basic analysis", "clara_cluster", "anova","t-test", "Calculate M"),
                                                            "No grouping required" = c("PCA features", "PCA samples"),
                                                            "No intensities required" = list("mzMatch" = "mzMatch")),
                                   
                                   analysesAvailable2 = c("Peak shapes", "Fast peak shapes"),
                                   
                                   analysesSelected = "Basic analysis",
                                   analysesSelected = NULL,
                                   numClusters = 2,
                                   dbselected = system.file("db", "smid-db_pos.csv", package = "Metaboseek"),
                                   normalizationMethod = "mean"
  )
  
  tempValues <- reactiveValues(zeroReplacementIntermediate = "lowest intensity value") #keep this one separate because it is not an FTAnalysisParam slot
  
  # observeEvent(internalValues$zeroReplacement,{
  #   print(internalValues$zeroReplacement)
  # },
  # ignoreNULL = FALSE,
  # ignoreInit = FALSE)
  # 
  observeEvent(values$featureTables,{
    internalValues$normalize <- is.null(values$featureTables)
    internalValues$useNormalized <- is.null(values$featureTables)
    
    
  }, once = T)
  
  observeEvent(reactives()$fileGrouping,{
    if(!is.null(reactives()$fileGrouping)){
    internalValues$fileGrouping <- tableGrouping(anagrouptable = reactives()$fileGrouping)$anagroupnames
    }
  })
  
  output$normDataCheck <- renderUI({
    div(title= "Apply normalization factor based on mean intensities for each column, and replace values of 0 by the lowest non-zero value in each column.",
        checkboxInput(ns('makenormdata'), 'Normalize data', value = internalValues$normalize))
  })
  
  observeEvent(input$makenormdata,{
    internalValues$normalize <- input$makenormdata
  })
  
  output$normDataUseCheck <- renderUI({
    div(title= "Use normalized data for subsequent analysis. Requires normalized data in table and will generate it if not present.",
        checkboxInput(ns('usenormdata'), 'Use normalized/imputed data', value = internalValues$useNormalized)
        )
  })
   observeEvent(input$usenormdata,{
    internalValues$useNormalized <- input$usenormdata
  }) 
  
  checkboxInput(ns("replaceNAs"), "Replace NA values with 0.",
                value = TRUE)
 
  output$replaceNAsCheck <- renderUI({
    div(title= "Replaces NA values in the intensity columns by 0 (before normalization, affects the original intensity columns).",
        checkboxInput(ns("replaceNAs"), "Replace NA values with 0.", value = !is.null(internalValues$replaceNAs))
    )
  })
  
  observeEvent(input$replaceNAs,{
    if(input$replaceNAs){
      internalValues$replaceNAs <- 0
    }else{
      internalValues$replaceNAs <- NULL
    }
  })
  
  output$zeroReplacementCheck <- renderUI({
    div(title= "Replaces 0 values in the 'normalized/imputed' intensity columns by a non-zero value (after NA replacement and before normalization, does not the original intensity columns.).",
  selectizeInput(ns("zeroReplacement"), "Replace zero values with", 
                 choices = c("lowest intensity value",
                             "1",
                             "100",
                             "1000",
                             "do not replace"),
                 selected = tempValues$zeroReplacementIntermediate
                 )
    )
  })
  
  #this seems necessary to avoid feedback loops...
  observeEvent(input$zeroReplacement,{
    tempValues$zeroReplacementIntermediate <- input$zeroReplacement
  })
  
  observeEvent(tempValues$zeroReplacementIntermediate,{
    
    switch(tempValues$zeroReplacementIntermediate,
           "lowest intensity value" = {internalValues$zeroReplacement <- NULL},
           "100" = {internalValues$zeroReplacement <- 100},
           "1000" = {internalValues$zeroReplacement <- 1000},
           "do not replace" = {internalValues$zeroReplacement <- 0},
           "1" = {internalValues$zeroReplacement <- 1}
    )
    
  })

  
   output$logDataUseCheck <- renderUI({
    div(title= "Calculate logarithm with base 10 of normalized intensity values (will replace normalized intensity values)",
        checkboxInput(ns('lognormdata'), 'Apply log10', value = internalValues$logNormalized))
  })
   
   observeEvent(input$lognormdata,{
     internalValues$logNormalized <- input$lognormdata
   })
   
   
  output$ctrlSelect <- renderUI({selectizeInput(ns('selctrl'), 'Select control group',
                                                choices = if(!is.null(values$featureTables)){c(values$featureTables$tables[[values$featureTables$active]]$gNames)}else{reactives()$fileGrouping$Group},
                                                selected = isolate({if(!is.null(values$featureTables)){values$featureTables$tables[[values$featureTables$active]]$ctrlGroups}else{internalValues$controlGroups}}),
                                                multiple = F)})
  observeEvent(input$selctrl,{
    if(!is.null(values$featureTables)){
      values$featureTables$tables[[values$featureTables$active]]$ctrlGroups <- input$selctrl}
    
    internalValues$controlGroups <- input$selctrl
  })
  
  
  output$analysisSelect <- renderUI({
    div(title = "Select analysis steps that will work on a feature table alone. Some of these will require a feature table with grouped intensity columns.",
    selectizeInput(ns('selAna'), 'Select feature table analyses',
                                                    choices = internalValues$analysesAvailable,
                                                    selected = internalValues$analysesSelected,
                                                    multiple = T)
    )
    })
  
  observeEvent(input$selAna,{
    internalValues$analysesSelected <- input$selAna
  })
  
  output$analysisSelect2 <- renderUI({
    div(title = "Select analysis steps that will use all MS data files in the currently selected MS grouping layout in combination with the active  feature table",
        
selectizeInput(ns('selAna2'), 'Select MS-data dependent analyses',
                                                    choices = internalValues$analysesAvailable2,
                                                    selected = internalValues$analysesSelected2,
                                                    multiple = T)
)
})
  
  observeEvent(input$selAna2,{
    internalValues$analysesSelected2 <- input$selAna2
  })
  
  output$normMethod <- renderUI({
    
  selectizeInput(ns("normalizationMethod"), "Normalization Method", 
                 choices = c("Column Means" = "mean",
                             "Geometric Column Means" = "gm_mean"),
                 selected = internalValues$normalizationMethod
                 )
  })
  
  observeEvent(input$normalizationMethod,{
    internalValues$normalizationMethod <- input$normalizationMethod
  })
  
  output$normSource <- renderUI({
    tempitem <- 'filteredTable'
    names(tempitem) <- paste0(names(values$featureTables$index)[values$featureTables$index == activeFT(values)], " (Filters applied)")
    
  div(title = "Use intensity columns from this table for normalization.\nWill use the NON-normalized columns to calculate normalization factors and ignore 0 and NA values.\nNeeds to have the same intensity column names as the currently active Feature Table.",
      selectizeInput(ns("normalizationSource"), "Normalization Source Table",
                     choices = c(tempitem, values$featureTables$index),
                     selected = activeFT(values))
  )
  })
  
  observeEvent(input$normalizationSource,{
    internalValues$normalizationSource <- input$normalizationSource
  })
  
  
  output$claraClusters <- renderUI({ 
    #if("clara_cluster" %in% internalValues$analysesSelected){
    div(title = "Number of clusters in which to group features based on their intensities across samples by k-medoids (clara).",
        numericInput('kclusternum',
                     "Number of clara clusters:",
                     value = internalValues$numClusters,
                     min = 2, step = 1))
    # }
  })
  
  observeEvent(input$kclusternum,{
    internalValues$numClusters <- input$kclusternum
  })
  
  observeEvent(input$analyzeButton,{
    
    tryCatch({
      withProgress(message = 'Please wait!', detail = "analyzing feature table", value = 0.5, {
        
        #if("mzMatch" %in% internalValues$analysesSelected){
          
      #  }
          stepsbefore <- length(processHistory(FeatureTable(values)))
          
        
          
          if(!length(internalValues$normalizationSource) 
             || internalValues$normalizationSource == activeFT(values)){
            nfrom <- NULL
          }else if(internalValues$normalizationSource %in% values$featureTables$index){
          nfrom <- FeatureTable(values,
                                tableID = internalValues$normalizationSource)
          }else if(internalValues$normalizationSource == "filteredTable"){
            nfrom <- getFilters(values)
          }else{
            nfrom <- NULL
          }
          
          if(internalValues$replaceNAs){
            FeatureTable(values) <- removeNAs(object = FeatureTable(values),
                                              replacement = 0)
          }
          
          FeatureTable(values) <- FTNormalizationFactors(object = FeatureTable(values),
                   normalizeFrom = nfrom,
                   normalizationMethod = internalValues$normalizationMethod,
                   zeroReplacement = internalValues$zeroReplacement,
                   transformation = if(internalValues$logNormalized){"log10"}else{NULL})
       
        FeatureTable(values) <- analyzeFT(object = FeatureTable(values),
                                          MSData = values$MSData$data,
                                          param = FTAnalysisParam(analyze = c(internalValues$analysesSelected, internalValues$analysesSelected2), 
                                                                  normalize = internalValues$normalize,
                                                                  useNormalized = internalValues$useNormalized,
                                                                  logNormalized = internalValues$logNormalized,
                                                                  
                                                                  normalizationFactors = FeatureTable(values)$normalizationFactors,
                                                                  zeroReplacement = internalValues$zeroReplacement,
                                                                  replaceNAs = internalValues$replaceNAs,
                                                                  
                                                                  .files = if(length(values$MSData$layouts[[values$MSData$active]]$filelist)){
                                                                      values$MSData$layouts[[values$MSData$active]]$filelist
                                                                      }else{
                                                                          character()
                                                                          },
                                                                  ppm = if(!is.null(values$MSData$data)){values$MSData$layouts[[values$MSData$active]]$settings$ppm}else{5},
                                                                  controlGroup = internalValues$controlGroups,
                                                                  p.adjust.method = values$GlobalOpts$p.adjust.method,
                                                                  numClusters = internalValues$numClusters,
                                                                  mzMatchParam = list(db = internalValues$dbselected,
                                                                                      ppm = 5,
                                                                                      mzdiff = 0.001),
                                                                  workers = values$GlobalOpts$enabledCores
                                                                  ))
        
        errorIndices <- which(sapply(processHistory(FeatureTable(values)), hasError))

        if(any(errorIndices > stepsbefore)){
            
            allerrs <- unlist(lapply(processHistory(FeatureTable(values))[errorIndices[errorIndices > stepsbefore]],
                              error))
          
          showModal(
            modalDialog(
              p(strong("A problem has occured!")),
              hr(),
              p( paste0(names(allerrs), ": ",
                        allerrs,
                 collapse = "\n" )),
              
              
              hr(),
              p("Other analyses completed without error."),
              title = "Warning",
              easyClose = T,
              footer = modalButton("Ok")
            ))
          
        }else{
          
          showNotification(paste("Feature table analysis completed."), duration = 10, type = "message")
          
          
        }
      })
    },
    error = function(e){
      showModal(
        modalDialog(
          p(strong("An error has oocured!")),
          p("The analysis was not successful. Error message:"),
          hr(),
          p(paste(e)),
          hr(),
          title = "ERROR",
          easyClose = T,
          footer = modalButton("Ok")
          
        ))
    })
    
    
  })
  
  NormSettings <- callModule(ModalWidget, "normSettings",
                       reactives = reactive({  
                         list(fp = fluidPage(
                           fluidRow(
                             column(4,
                                    htmlOutput(ns("replaceNAsCheck"))
                             ),
                             column(4,
                                    htmlOutput(ns("zeroReplacementCheck"))
                                    )),
                           
                             fluidRow(
                               
                               column(4,
                                      htmlOutput(ns('normDataCheck'))
                               ),
                               column(4,
                                      htmlOutput(ns('logDataUseCheck'))
                               ),
                               column(4,
                                      htmlOutput(ns('normMethod'))       
                                      )),
                               fluidRow(
                                 column(6,
                                        htmlOutput(ns('normSource'))       
                                        
                                        )
                               
                               )
                             
                            
                             )
                         
                         ) }),
                       static = list(tooltip = "Settings for Imputation and Normalization",
                                     title = "Settings for Imputation and Normalization", 
                                     label = "Normalization/Imputation Settings...",
                                     icon = NULL,
                                     modalButtonLabel = "Close"),
                       useActionLink = TRUE,
                       style = "color:#C41230;padding:2px;")
  
 
  

  
  
  output$advancedana <- renderUI({ 
    tagList(
    fluidRow(
      hr(),
      h4("Advanced analysis"),
      column(2,
             GetIntensitiesModuleUI(ns("gi"))),
      column(2,
             FindMS2ScansModuleUI(ns("findms2"))),
      column(2,
             FindPatternsModuleUI(ns("findpatterns"))),
      column(2,
             LabelFinderModuleUI(ns("labelfinder")))
      ),
    fluidRow(
      hr(),
      p("These analysis tools will use the current feature table to generate a new feature table with different properties."),
      column(2,
             PeakPickModuleUI(ns("pp"))
      ),
      column(2,
             MZcalcModuleUI(ns("mzcalc"))
      ))
  )
    
  })
  
  output$seldbs <- renderUI({ 
    selectizeInput(ns("selDB"), "select reference table for mz matching", 
                   choices = list("SMID-DB negative" = system.file("db", "smid-db_neg.csv", package = "Metaboseek"),
                                  "SMID-DB positive" = system.file("db", "smid-db_pos.csv", package = "Metaboseek"),
                                  "LipidBLAST negative" = system.file("db", "LipidBLAST_mz_trimmed_neg.csv", package = "Metaboseek"),
                                  "LipidBLAST positive" = system.file("db", "LipidBLAST_mz_trimmed_pos.csv", package = "Metaboseek"),
                                  "HMDB negative (endogenous detected)" = system.file("db", "HMDB_detected_neg.csv", package = "Metaboseek"),
                                  "HMDB positive (endogenous detected)" = system.file("db", "HMDB_detected_pos.csv", package = "Metaboseek")
                                  
                                  ),
                   selected = internalValues$dbselected, multiple = T)
  })
  
  observeEvent(input$selDB,{
    internalValues$dbselected <- input$selDB
  })
  
  
  output$NormInfoText <- renderPrint({FeatureTable(values)$normalizationFactors})
  
  output$NormalizationInfo <- renderUI({
    if(!is.null(values$featureTables)){
    tagList(
    fluidRow(
    p('Normalization Factors:')),
    fluidRow(
     verbatimTextOutput(ns("NormInfoText")) 
    )
    )
    }
  })
  
  observe({
    toggle(id = 'seldbs', condition = "mzMatch" %in% internalValues$analysesSelected)
    #toggle(id = "intensSettings", condition = !is.null(values$featureTables))
    toggle(id = 'claraClusters', condition = "clara_cluster" %in% internalValues$analysesSelected)
    toggle(id = 'analyzeButton', condition = !is.null(values$featureTables))
   # toggle(id = 'peakpickMod', condition = !is.null(values$featureTables$Maintable) && !is.null(values$featureTables) && !is.null(values$MSData))
  #  toggle(id = 'getintmod', condition = !is.null(values$featureTables$Maintable) && !is.null(values$featureTables) && !is.null(values$MSData))
    
    toggle(id = 'advancedana', condition = !is.null(values$featureTables) && !is.null(values$featureTables$Maintable))
    
  })
  
  callModule(GetIntensitiesModule, "gi", values)
  
  callModule(PeakPickModule, "pp", values)
  
  callModule(MZcalcModule, "mzcalc", values)
  
  callModule(FindPatternsModule, "findpatterns", values)
  
  callModule(LabelFinderModule, "labelfinder", values)
  
  
  return(internalValues)
  
}

#' @describeIn TableAnalysisModule UI elements
#' @export
TableAnalysisModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      h4("Prepare data"),
      column(4,
             div(style="display:inline-block",
                 htmlOutput(ns('normDataUseCheck')),
                 ModalWidgetUI(ns('normSettings')),
             )
      ),
      column(3,
             htmlOutput(ns('ctrlSelect'))
      ),
       column(5,
        htmlOutput(ns('NormalizationInfo'))
       )
      ),
    fluidRow(
      h4("Basic analysis"),
      column(3,
             htmlOutput(ns('analysisSelect'))
      ),
      column(3,
             htmlOutput(ns('analysisSelect2'))
      ),
      column(3,
             htmlOutput(ns('claraClusters'))),
      column(3,
             htmlOutput(ns('seldbs'))
      )
      
    ),
    fluidRow(
      column(5),
      column(2,
      div(title = "Run all selected feature table normalization and analysis steps",
      actionButton(ns('analyzeButton'),"Run selected analyses",style="color: #fff; background-color: #C41230; border-color: #595959")
    )),
    column(5))
    ,
    htmlOutput(ns("advancedana"))
  )
  
  
}