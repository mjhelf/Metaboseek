#' TableAnalysisModule
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @import shiny
#' @importFrom shinyjs toggle
#' 
#' @export 
TableAnalysisModule <- function(input,output, session,
                                reactives = reactive({list(fileGrouping = NULL)}),
                                values = reactiveValues(featureTables = featureTables,
                                                        MSData = MSData,
                                                        MainTable = MainTable),
                                static = list()
){
  #### Initialization ####
  
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(normalize = TRUE,
                                   useNormalized = TRUE,
                                   logNormalized = F,
                                   controlGroups = NULL,
                                   analysesAvailable = c("Basic analysis", "clara_cluster", "t-test", "Peak shapes", "PCA features", "PCA samples", "anova"),
                                   analysesSelected = "Basic analysis",
                                   numClusters = 2
  )
  
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
        checkboxInput(ns('usenormdata'), 'Use normalized data', value = internalValues$useNormalized))
  })
  
 
  
  observeEvent(input$usenormdata,{
    internalValues$useNormalized <- input$usenormdata
  })
  
   output$logDataUseCheck <- renderUI({
    div(title= "Calculate logarithm with base 10 of normalized intensity values (will replace normalized intensity values)",
        checkboxInput(ns('lognormdata'), 'Apply log10', value = internalValues$logNormalized))
  })
   
   observeEvent(input$lognormdata,{
     internalValues$logNormalized <- input$lognormdata
   })
   
   
  output$ctrlSelect <- renderUI({selectizeInput(ns('selctrl'), 'Select control group(s)',
                                                choices = if(!is.null(values$featureTables)){c(values$featureTables$tables[[values$featureTables$active]]$gNames)}else{reactives()$fileGrouping$Group},
                                                selected = if(!is.null(values$featureTables)){values$featureTables$tables[[values$featureTables$active]]$ctrlGroups}else{internalValues$controlGroups},
                                                multiple = T)})
  observeEvent(input$selctrl,{
    if(!is.null(values$featureTables)){
      values$featureTables$tables[[values$featureTables$active]]$ctrlGroups <- input$selctrl}
    
    internalValues$controlGroups <- input$selctrl
  })
  
  
  output$analysisSelect <- renderUI({selectizeInput(ns('selAna'), 'Select analyses',
                                                    choices = internalValues$analysesAvailable,
                                                    selected = internalValues$analysesSelected,
                                                    multiple = T)})
  
  observeEvent(input$selAna,{
    internalValues$analysesSelected <- input$selAna
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
        
        res <- analyzeTable(df = values$featureTables$tables[[values$featureTables$active]]$df,
                            intensities = values$featureTables$tables[[values$featureTables$active]]$intensities,
                            groups = values$featureTables$tables[[values$featureTables$active]]$anagroupnames,
                            analyze = internalValues$analysesSelected, 
                            normalize = internalValues$normalize,
                            useNormalized = internalValues$useNormalized,
                            logNormalized = internalValues$logNormalized,
                            MSData = values$MSData$data,
                            ppm = if(!is.null(values$MSData$data)){values$MSData$layouts[[values$MSData$active]]$settings$ppm}else{5},
                            controlGroup = internalValues$controlGroups,
                            numClusters = internalValues$numClusters)
        
        values$featureTables$tables[[values$featureTables$active]] <- updateFeatureTable(values$featureTables$tables[[values$featureTables$active]],res$df)
        values$featureTables$tables[[values$featureTables$active]]$anagrouptable <- updateDF(res$PCA_samples,
                                                                                             values$featureTables$tables[[values$featureTables$active]]$anagrouptable)
        

        if(length(res$errmsg) >0){
          
          showModal(
            modalDialog(
              p(strong("A problem has oocured!")),
              hr(),
              p( paste0(names(res$errmsg), ": ", unlist(res$errmsg), collapse = "\n" )),
              hr(),
              p("Other analyses completed without error."),
              title = "Warning",
              easyClose = T,
              footer = modalButton("Ok")
            ))
          
        }else{
          
          showNotification(paste("Feature table analysis completed."), duration = 0, type = "message")
          
          
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
  
  
  output$peakpickMod <- renderUI({ 
    PeakPickModuleUI(ns("pp"))
  })
  
  output$mzCalcMod <- renderUI({ 
    MZcalcModuleUI(ns("mzcalc"))
  })
  
  observe({
    
    toggle(id = 'claraClusters', condition = "clara_cluster" %in% internalValues$analysesSelected)
    toggle(id = 'analyzeButton', condition = !is.null(values$featureTables))
    toggle(id = 'peakpickMod', condition = !is.null(values$MainTable) && !is.null(values$featureTables) && !is.null(values$MSData))
    toggle(id = 'mzCalcMod', condition = !is.null(values$MainTable) && !is.null(values$featureTables))
    
  })
  
  PP <- callModule(PeakPickModule, "pp",
                   values = reactiveValues(MSData = values$MSData,
                                           featureTables = values$featureTables,
                                           MainTable = values$MainTable))
  
  MZcalc <- callModule(MZcalcModule, "mzcalc",
                   values = reactiveValues(featureTables = values$featureTables,
                                           MainTable = values$MainTable))
  
  
  
  return(internalValues)
  
}

#' TableAnalysisModuleUI
#' 
#' @param id id of the shiny module
#' 
#' @export
TableAnalysisModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      h4("Prepare data"),
      column(4,
             htmlOutput(ns('normDataCheck'))
      ),
      column(4,
             htmlOutput(ns('normDataUseCheck'))
      ),
      column(4,
             htmlOutput(ns('logDataUseCheck'))
      )
      
      ),
    fluidRow(
      h4("Basic analysis"),
      column(4,
             htmlOutput(ns('analysisSelect'))
      ),
      column(2,
             htmlOutput(ns('ctrlSelect'))
      ),
      column(2,
             htmlOutput(ns('peakpickMod'))
             ),
      column(2,
             htmlOutput(ns('mzCalcMod'))
      ),
      column(2,
             htmlOutput(ns('claraClusters'))
      )
      
    ),
    fluidRow(
      actionButton(ns('analyzeButton'),"Analyze data",style="color: #fff; background-color: #C41230; border-color: #595959")
    )
    # hr(),
    # h3("Step 2: Advanced feature analysis"),
    # hr(),
    # h3("Step 3: Sample group analysis")
    
    
    #,
    # fluidRow(
    #    column(6,
    #densplotModuleUI("nonNormalizedPlot")
    #    ),
    #   column(6,
    #densplotModuleUI("NormalizedPlot")
    #   )
    #)
    
  )
  
  
}