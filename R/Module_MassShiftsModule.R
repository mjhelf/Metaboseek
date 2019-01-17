#' MassShiftsModule
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' @param values Import data from the shiny session
#' @param static Import data from the shiny session
#' 
#' @export 
MassShiftsModule <- function(input,output, session,
                             values = reactiveValues(MSData = MSData)
){
  
  ns <- NS(session$ns(NULL))
  
  if(!file.exists(file.path(system.file("config", package = "METABOseek"), "MassShifts.csv"))){
    
    write.csv(data.frame(use = c(T,F,F,F,F,F),
                         Name = c("unmodified", "-H2O", "+13C iso", character(3)), 
                         Mol_formula = character(6),
                         charge = rep(1,6), 
                         mz_shift = c(0, -18.0105646837, 1.003355, numeric(3)),
                         stringsAsFactors = F),
              file.path(system.file("config", package = "METABOseek"), "MassShifts.csv"),
              row.names = F)
    
  }
  
  #####Mass shift table
  
  internalValues <- reactiveValues(df = read.csv(file.path(system.file("config", package = "METABOseek"), "MassShifts.csv"),
                                                 stringsAsFactors = F) )
  
  
  
  output$massShiftTab <- renderRHandsontable({if(!is.null(internalValues$df)){
    rhandsontable(internalValues$df,
                  digits = 9)%>%
      hot_col("mz_shift", format="0.000000")
  }
  })
  
  observeEvent(input$updateshifts,{
    tab <- hot_to_r(input$massShiftTab)
    
    write.csv(tab,
              file.path(system.file("config", package = "METABOseek"), "MassShifts.csv"),
              row.names = F)
    
    #more elegant
    #values$MSData$massShifts <- tab[tab$use,]
    
    #currently this works only:
    if(all(!tab$use)){
      values$MSData$massShifts <- list(labs = "",
                                       shifts = 0)
    }else{
    values$MSData$massShifts <- list(labs = tab[tab$use,"Name"],
                                     shifts = tab[tab$use,"mz_shift"] )
    }
    
  })
  
  return(internalValues)
}

#' MassShiftsModuleUI
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param id
#' 
#' @export
MassShiftsModuleUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      rHandsontableOutput(ns('massShiftTab'))
    ),
    fluidRow(
      actionButton(ns('updateshifts'), "Update mass shifts")
    )
  )
  
}