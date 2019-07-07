#' ShowSiriusModule
#' 
#' A button that triggers retrieval of Sirius results for the selected MS2 scans
#' 
#' @inherit MseekModules
#' 
#' @return Returns nothing
#' 
#' @describeIn ShowSiriusModule Server logic
#' 
#' @export 
ShowSiriusModule <- function(input,output, session, 
                          values = reactiveValues(SiriusModule = NULL,
                                                  GlobalOpts = NULL),
                          reactives = reactive({
                            list(splash = NULL,
                                 ms1splash = NULL,
                                 mz = NULL)
                          })
){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(siriusStatus = NULL)
  
  
  #Don't show any SIRIUS results if the current query is NULL
  #or if the new query is different from the previous selection
  observeEvent(internalValues$query,{# input$getSirius,{#
      if(is.null(internalValues$query)
         || (!is.null(values$SiriusModule$activeSirius) && internalValues$query$splash != values$SiriusModule$activeSirius$splash)
         ){
          values$SiriusModule$activeSirius<- NULL
          
      }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  #
  observeEvent(internalValues$activeSirius,{# input$getSirius,{#
      if(!is.null(internalValues$activeSirius) && internalValues$query$splash != values$SiriusModule$activeSirius$splash){
          values$SiriusModule$activeSirius<- NULL
          
      }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
 
  ###Show SIRIUS results on demand when clicking the show SIRIUS button
  observeEvent(input$getSirius,{
    if(is.null(internalValues$query)){
      values$SiriusModule$activeSirius<- NULL

    }else{
      tryCatch({
        
        values$SiriusModule$activeSirius <- getSirius(file.path(values$GlobalOpts$siriusFolder, "METABOseek"),
                                                      splash = internalValues$query$splash, 
                                                      ts = internalValues$query$timestamp)

      }, error = function(e){
        showNotification(paste("A problem occured and SIRIUS results were not found. Maybe they are not ready yet, try again in a minute."), type = "error", duration = 3)
          values$SiriusModule$activeSirius<- NULL
          
      })
}
    values$SiriusModule$activeMF <- NULL
    values$SiriusModule$activeStructure <- NULL
  }, ignoreNULL = FALSE, ignoreInit = TRUE )
  
  
  output$getsiriusbutton <- renderUI({
    if(is.null(values$SiriusModule$siriusIndex) 
       || is.null(reactives()$splash) 
       || !reactives()$splash %in% values$SiriusModule$siriusIndex$splash){
      st <- "color: #000000; background-color: #C41230; border-color: #595959"
      ti <- "SIRIUS results not (yet) available for this spectrum."
      internalValues$query <- NULL
    }else{
      
      # fullquery  <- data.frame(mz = reactives()$mz,
      #                                    stringsAsFactors = F)

      searchterm <- apply(data.frame(ion = values$GlobalOpts$SiriusSelIon,
                                     ms1splash = reactives()$ms1splash,
                                     charge = values$SiriusModule$selCharge,
                                     fingerid = values$GlobalOpts$SiriusCheckFinger,
                                     moreOpts = paste0("-c 50 ",
                                                       if(!is.null(values$GlobalOpts$SiriusCheckFinger) 
                                                          && values$GlobalOpts$SiriusCheckFinger){paste0("--fingerid-db ", values$GlobalOpts$SiriusDBselected," -e ")}else{"-e "},
                                                       values$GlobalOpts$SiriusElements,
                                                       " -p ", values$GlobalOpts$SiriusSelInstrument),
                                     METABOseek_sirius_revision =  2,
                                     stringsAsFactors = F),1,
                          digest, algo = "xxhash64")
          
        
      hits <- which(reactives()$splash == values$SiriusModule$siriusIndex$splash
                    & values$SiriusModule$siriusIndex$settingsHash == searchterm[1])
     
      
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

#' @describeIn ShowSiriusModule UI elements
#' @export
ShowSiriusModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns("getsiriusbutton"))
  )
  
}