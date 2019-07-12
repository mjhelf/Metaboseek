#' MassShiftsModule
#' 
#' Module to select mass shifts to plot in EICs
#' 
#' @describeIn MassShiftsModule server logic
#' 
#' @inherit MseekModules
#' 
#' @return Returns its internalValues
#' 
#' @export 
MassShiftsModule <- function(input,output, session, values){
  
  ns <- NS(session$ns(NULL))
  
  if(!file.exists(file.path(system.file("config", package = "Metaboseek"), "MassShifts.csv"))){
    
    write.csv(data.frame(use = c(T,F,F,F,F,F),
                         Name = c("unmodified", "-H2O", "+13C iso", character(3)), 
                         Mol_formula = character(6),
                         charge = rep(1,6), 
                         mz_shift = c(0, -18.0105646837, 1.003355, numeric(3)),
                         stringsAsFactors = F),
              file.path(system.file("config", package = "Metaboseek"), "MassShifts.csv"),
              row.names = F)
    
  }
  
  #####Mass shift table
  
  internalValues <- reactiveValues(df = read.csv(file.path(system.file("config", package = "Metaboseek"), "MassShifts.csv"),
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
              file.path(system.file("config", package = "Metaboseek"), "MassShifts.csv"),
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

#' @describeIn MassShiftsModule UI elements
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