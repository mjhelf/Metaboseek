#' MZcalcModule
#' 
#' Module for calculating mzs from molecular formulas in the FeatureTable
#' 
#' @inherit MseekModules
#' 
#' @return Returns its internalValues
#' 
#' @describeIn MZcalcModule Server logic
#' 
#' @export 
MZcalcModule <- function(input,output, session, values){
  ns <- NS(session$ns(NULL))
  
  internalValues <- reactiveValues(done = FALSE)
  
  output$formulaColumn <- renderUI({
    
    selectizeInput(ns("formulacolumn"), "Molecular formula column:", 
                   selected = "formula",
                   choices = colnames(values$featureTables$tables[[values$featureTables$active]]$df),
                   multiple = F)
    
    
  })
  
  
  dialog <- callModule(MseekModalModule, "mzcalcbutton",
                       reactives = reactive({  
                         list(fp = fluidPage(
                           fluidRow(
                             p("A new Feature Table will be generated, based on the molecular formulas in the currently active Feature Table. Formulas provided should be those of uncharged molecules.")
                           ),
                           fluidRow(
                             column(4,
                                    htmlOutput(ns("formulaColumn"))
                             ),
                             column(3,
                                    selectizeInput(ns("chargesel"), "Charge states:", 
                                                   selected = 1,
                                                   choices = c(-3, -2, -1, 1, 2, 3),
                                                   multiple = T)
                             ),
                             
                             column(1,
                                    actionButton(ns("abutton"), "Calculate")
                             )
                           )
                           
                         ) )     }),
                       static = list(tooltip = "Calculate Calculate m/z values for the molecula formulas in this table",
                                     title = "Calculate m/z values", 
                                     label = "Calculate m/z",
                                     icon = icon("calculator", lib = "font-awesome")))
  
  
  
  
  observeEvent(input$abutton,{
    tryCatch({
      withProgress(message = 'Please wait!', detail = "Finding peaks", value = 0.5, {
        
        METABOseek:::TableUpdateChunk()
        
        tabid <- paste0("table",length(values$featureTables$index))
        names(tabid) <- paste0("mzcalc_", paste(input$chargesel, collapse = "_"), values$featureTables$tables[[values$featureTables$active]]$tablename)
        
        newdf <-  calcMZs(values$featureTables$tables[[values$featureTables$active]]$df,
                          charges = as.integer(input$chargesel), carrier = "H",
                          monoisotopic = T, mf_column = input$formulacolumn)

        
        if(is.null(newdf) || nrow(newdf) == 0){
          
          removeModal()
          showModal(
            modalDialog(
              p("No m/z values could be calculated for this feature table in the loaded feature table. Make sure you selected a column with molecula formulas.")
              ,
              title = "m/z calculation failed!",
              easyClose = T,
              fade = F,
              size = "s",
              footer = modalButton("Ok") 
            ))
          
        }else{
          
         
          #simpler:
          values$featureTables$tables[[tabid]] <- constructFeatureTable(df = newdf,
                                                                        tablename = names(tabid),
                                                                        anagrouptable = NULL,
                                                                        editable = F)
          #values$featureTables$tables[[tabid]]$df <- newdf
          
          values$featureTables$index <- c( values$featureTables$index, tabid)
          values$featureTables$tableSwitch <- T
          values$featureTables$active <- unname(tabid)
          values$featureTables$row_filters <- TRUE
          
          
          removeModal()
          showNotification(paste("m/z calculation completed."), duration = 0, type = "message")
          
        }}
      )},
      error = function(e){
        
        showNotification(paste("An error occured: ", e), duration = 0, type = "error")
        
        
      })
    
  })
  
  
  return(internalValues)
}

#' @describeIn MZcalcModule UI elements
#' @export
MZcalcModuleUI <- function(id)
{
  ns <- NS(id)
  
  MseekModalModuleUI(ns("mzcalcbutton"))
  
}