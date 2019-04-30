#' ShowSiriusModule
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param reactives Import data from the shiny session
#' 
#' 
#' @export 
ShowSiriusModule <- function(input,output, session, 
                          values = reactiveValues(SiriusModule = SiriusModule,
                                                  GlobalOpts = GlobalOpts),
                          reactives = reactive({
                            list(splash = NULL,
                                 mz = NULL)
                          })
){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(siriusStatus = NULL)
  
  observeEvent(input$getSirius,{
    if(is.null(internalValues$query)){
      values$SiriusModule$activeSirius<- NULL
      
    }else{
      tryCatch({
        
        values$SiriusModule$activeSirius <- getSirius(file.path(values$GlobalOpts$siriusFolder, "METABOseek"),
                                                      splash = internalValues$query$splash, 
                                                      ts = internalValues$query$timestamp)

      }, error = function(e){
        showNotification(paste("A problem occured and SIRIUS results were not found. Maybe they are not ready yet, try again in a minute."), type = "error", duration = 0)
        
      })
}
    values$SiriusModule$activeMF <- NULL
    values$SiriusModule$activeStructure <- NULL
  })
  
  
  output$getsiriusbutton <- renderUI({
    
    if(is.null(values$SiriusModule$siriusIndex) || is.null(reactives()$splash) || !reactives()$splash %in% values$SiriusModule$siriusIndex$splash){
      
      st <- "color: #000000; background-color: #C41230; border-color: #595959"
      ti <- "SIRIUS results not (yet) available for this spectrum."
      internalValues$query <- NULL
      
    }else{
      
      # fullquery  <- data.frame(mz = reactives()$mz,
      #                                    stringsAsFactors = F)

      searchterm <- paste(round(reactives()$mz,4),
                          reactives()$splash,
                          values$GlobalOpts$SiriusSelIon,
                          #values$SiriusModule$selCharge,
                          values$GlobalOpts$SiriusCheckFinger,
                          paste0("-c 50 ",
                                 if(!is.null(values$GlobalOpts$SiriusCheckFinger) 
                                    && values$GlobalOpts$SiriusCheckFinger){paste0("--fingerid-db ", values$GlobalOpts$SiriusDBselected," -e ")}else{"-e "},
                                 values$GlobalOpts$SiriusElements,
                                 " -p ", values$GlobalOpts$SiriusSelInstrument),
                          c(REVISION = 2), sep = "//")
      
      # values$SiriusModule$siriusIndex[,c("splash", "ion", "charge", "fingerid", "moreOpts", "METABOseek_sirius_revision")]                    
      #                     
      # fullquery$splash = reactives()$splash
      # fullquery$ion = values$SiriusModule$selIon
      # fullquery$charge = values$SiriusModule$selCharge
      # fullquery$fingerid = values$SiriusModule$checkFinger
      # fullquery$moreOpts = paste0("-e ",  values$SiriusModule$elements, " -p ", values$SiriusModule$selInstruments)
      # fullquery$METABOseek_sirius_revision = 1
      # 
      # checkme <- as.data.frame(rbindlist(list(query = fullquery, old = values$SiriusModule$siriusIndex[,c("splash", "ion", "charge", "fingerid", "moreOpts", "METABOseek_sirius_revision")]), fill = T))
      # 
      #To avoid problems with m/z digits possibly not retained when exporting and reimporting of siriusIndex to/from index.csv
      #checkme$mz <- round(checkme$mz, 4)
      
      #hits <- which(duplicated.data.frame(checkme)[seq(nrow(fullquery)+1,nrow(fullquery))])
      
      #if there is a perfect hit for the current query, retrieve the latest(bottom) one
      
      hits <- which(values$SiriusModule$quickLookup == searchterm)
     
      
     if(length(hits)>0){
       
       internalValues$query <- values$SiriusModule$siriusIndex[hits[length(hits)],]
       st <- "color: #000000; background-color: #9fe055; border-color: #595959"
       ti <- "SIRIUS results are available for this spectrum with the current SIRIUS settings."
       
     }else{
       #retrieve the latest sirius result for the same spectrum regardless of sirius settings
       hits <- which(reactives()$splash == values$SiriusModule$siriusIndex$splash)
       internalValues$query <- values$SiriusModule$siriusIndex[hits[length(hits)],]
       st <- "color: #000000; background-color: #ffd016; border-color: #595959"
       ti <- "SIRIUS results are available for this spectrum, but NOT with the current SIRIUS settings"
       
     }
      
      
      
    }
    
    actionButton(ns("getSirius"), "Show SIRIUS", style = st, title = ti)
    
  })
}

#' ShowSiriusModuleUI
#' 
#' 
#' UI module for interactive SIRIUS interface
#' 
#' @param id id to be used in ns()
#' 
#' @export
ShowSiriusModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns("getsiriusbutton"))
  )
  
}