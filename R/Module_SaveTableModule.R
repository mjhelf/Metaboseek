#' SaveTableModule
#' 
#' Module for saving Tables
#' 
#' @inherit MseekModules
#' 
#' @return Returns nothing
#' 
#' @describeIn SaveTableModule Server logic
#' 
#' @importFrom data.table fwrite
#' 
#' @export 
SaveTableModule <- function(input,output, session,
                            values = reactiveValues(projectData = NULL,
                                                    featureTables = NULL),
                            reactives = reactive({list(df = NULL,
                                                       filename = "table.csv")}),
                            static = list(tooltip = "Save",
                                          label = "Save",
                                          format = c("tsv", "csv"),
                                          allowformats = NULL)
                            
){
  ns <- NS(session$ns(NULL))
  
  output$saveTableButton <- renderUI({
    actionButton(ns("savetable"), static$label, icon = icon("floppy-save", lib = "glyphicon"))
  })
  
  output$fileWarning <- renderUI({
    if(!is.null(values$projectData$projectFolder) && !is.null(input$tabname)){
      if(
        file.exists(file.path(values$projectData$projectFolder,
                              file.path(dirname(reactives()$filename),
                                        if(!is.null(input$selFormat) && input$selFormat == "instrumentList"){paste0(input$tabname,".txt")}
                                        else{input$tabname})))
      ){
        
        p("Warning: Filename already exists. You will override the existing file when saving locally!", style = "color:#ff1111;")
        
      }
    }
  })
  
  output$inclsettings <- renderUI({
    if(!is.null(input$selFormat) && !is.na(input$selFormat != "")){
      
      switch(input$selFormat,
             
             "csv" = {},
             "tsv" = {},
             "mskFT" = {},
             "MetaboAnalyst" = {},
             
             "instrumentList" = {
               tagList(
                 fluidRow(
                   column(4,
                          selectizeInput(ns("incType"), "Type",
                                         choices = list(Inclusion = "inclusion",
                                                        Exclusion = "exclusion"))
                   ),
                   column(4,
                          selectizeInput(ns("incPolarity"), 
                                         "Polarity",
                                         choices = c("Positive", "Negative"))
                   )),
                 fluidRow(
                   column(4,style = "margin-top: 20px;",
                          div(title = "If selected, retention time restrictions are defined in the inclusion or exclusion list.",
                              checkboxInput(ns("incUseRTw"), "Restrict RT", value = T)      
                          )
                   ),
                   column(4,
                          div(title = "Inclusion list retention time window (+/- feature RT, in seconds)",
                              numericInput(ns("incRTw"),
                                           "RT window", value = 10)      
                          )
                   )
                 ),
                 fluidRow(
                   if(length(values$featureTables$Maintable$order)>5000){
                     p("Warning: This table has",
                       length(values$featureTables$Maintable$order),
                       "rows. Thermo software will only accept up to 5000 rows in an inclusion list.",
                       style = "color:#ff1111;")
                   }
                 ))
             })
    }
    
  })
  
  observeEvent(input$savetable,{
    
    if(!is.null(values$featureTables)){
        updateFT(values)
    }
    
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            p(strong("You can save this table in your project folder, or download it through your browser"))
          ),
          hr(),
          fluidRow(
            textInput(ns("tabname"), "File name:",
                      value = paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"), "_",
                                     basename(reactives()$filename)))
          ),
          fluidRow(
            htmlOutput(ns("fileWarning")),
            if(length(static$allowformats) > 0){
              tagList(
                fluidRow(
                  div(title = "Select a file format for table export",
                      selectizeInput(ns("selFormat"), "Format:",
                                     choices = static$allowformats))
                ),
                htmlOutput(ns("inclsettings"))
              )
            }
          ),
          fluidRow(
            column(4, div(title = "Download table through Browser",
                          downloadButton(ns("modalDownload"),"Download"))),
            column(4,
                   fluidRow(
                   div( title = "Save directly to current projectFolder (only works if you are working in a project folder)",
                           actionButton(ns("modalProjectFolder"),
                                        "Save locally"))),
                   fluidRow(
                       if(!is.null(values$featureTables)
                          && "mskFT" %in% static$allowformats){
                   div( title = "Also save as .mskFT file when saving to a Project Folder (in addition to inclusion list or .csv format.
                                 .mskFT files retain process history and grouping information.",
                        checkboxInput(ns("alwaysMskFT"),
                                     "also save as mskFT", value = TRUE))}
                           )),
            column(4,
                   fluidRow(
                     div( title = "Apply currently active Filters and generate a new copy of the active Feature Table under a new name.",
                          actionButton(ns("modalSaveInSession"),
                                       "Save in Session"))))
          )),
        title = "Save table",
        easyClose = T,
        fade = F,
        size = "m",
        footer = modalButton("Cancel") 
      ))
    
  })
  
  output$modalDownload <- downloadHandler(filename= function(){
    #paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))
    if(!is.null(input$selFormat)){ 
       suffix <- if(input$selFormat == "instrumentList"){".txt"
       }else if(!is.null(input$selFormat) && input$selFormat == "MetaboAnalyst"){paste0("_forMetaboAnalyst.csv")
           }else{paste0(".",input$selFormat)}
       paste0(gsub('\\.[Mm][Ss][Kk][Ff][Tt]$|\\.csv$|\\.txt$','',
                   input$tabname),suffix)
    }else{
            input$tabname
        }
  }, 
  content = function(file){
      tryCatch({
      if(!is.null(input$selFormat) && input$selFormat == "mskFT"){
          
          if(length(values$featureTables$Maintable$sortCheck) 
                    && values$featureTables$Maintable$sortCheck){
             srt <-  values$featureTables$Maintable$sortBy
          }else{
              srt <- character()
              }
          
          if(length(srt) 
             && length(values$featureTables$Maintable$decreasing)){
             dec <-  values$featureTables$Maintable$decreasing
          }else{
              dec <-  TRUE
              }
          exp <- FTFilter(FeatureTable(values),
                          filters = getFilters(values),
                          sortBy = srt,
                          decreasing = dec)
          
          saveMseekFT(exp,file,
                      writeRDS = TRUE, writeCSV = FALSE)
          
          }else{
    written <- tableWriter(if(is.null(values$featureTables)){reactives()$df}
                           else{
                             values$featureTables$tables[[values$featureTables$active]]$df[values$featureTables$Maintable$order,]
                           },
                           fname =  file,
                           format = if(static$format =="tsv"){"tsv"}else if(!is.null(input$selFormat)){input$selFormat}else{"csv"},
                           moreArgs = switch(input$selFormat,
                                             "MetaboAnalyst" = list(groups = FeatureTable(values)$anagrouptable),
                                             "instrumentList" = list(rtwin = input$incRTw,
                                           polarity = input$incPolarity,
                                           instrument = "QExactive",
                                           listType = input$incType,
                                           restrictRT = input$incUseRTw)
                           )
                           
                           )
          }
    showNotification(paste("Downloading file: ", 
                           file),
                     #paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))),
                     duration = 10)
      
    removeModal()
    
  },
  error = function(e){
      writeLines(paste("ERROR: Table was NOT saved: ",e),file)
      showNotification(paste("ERROR: Table was NOT saved. The downloaded file is EMPTY and only serves to prevent the session from closing in Firefox. Error message: ",e), type = "error", duration = NULL)
      
  })
  },
  contentType = if(static$format =="tsv" 
                   || (!is.null(input$selFormat) 
                       && input$selFormat == "instrumentList")){
    "text/tab-separated-values"}else{"text/comma-separated-values"})
  
  
  
  
  observeEvent(input$modalProjectFolder,{
    tryCatch({
    if(!is.null(values$projectData$projectFolder)){
      
      if(!dir.exists(dirname(file.path(values$projectData$projectFolder,
                                       file.path(dirname(reactives()$filename),
                                                 input$tabname))))){
        dir.create(dirname(file.path(values$projectData$projectFolder,
                                     file.path(dirname(reactives()$filename),
                                               input$tabname))), recursive = T)
      }
      if(is.null(values$featureTables)){
          updateFT(values)
      }
        
        if((!is.null(input$selFormat) 
           && input$selFormat == "mskFT")
           ||(!is.null(input$alwaysMskFT) 
              && input$alwaysMskFT) ){
            finalname <- file.path(values$projectData$projectFolder, 
                                   file.path(dirname(reactives()$filename),
                                             paste0(gsub('\\.[Mm][Ss][Kk][Ff][Tt]$|\\.csv$|\\.txt$','',
                                     input$tabname),".mskFT")))
            
            if(length(values$featureTables$Maintable$sortCheck) 
               && values$featureTables$Maintable$sortCheck){
                srt <-  values$featureTables$Maintable$sortBy
            }else{
                srt <- character()
            }
            
            if(length(srt) 
               && length(values$featureTables$Maintable$decreasing)){
                dec <-  values$featureTables$Maintable$decreasing
            }else{
                dec <-  TRUE
            }
            
            exp <- FTFilter(FeatureTable(values),
                            filters = getFilters(values),
                            sortBy = srt,
                            decreasing = dec)
        
        saveMseekFT(exp,finalname,
                    writeRDS = TRUE, writeCSV = FALSE)
        
        showNotification(paste("Saving file: ", 
                               finalname),
                         #paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))),
                         duration = 10)
        }
        
        if(is.null(input$selFormat) || input$selFormat != "mskFT"){
      
      written <- tableWriter(if(is.null(values$featureTables)){reactives()$df}
                             else{
                               FeatureTable(values)$df[values$featureTables$Maintable$order,]
                             },
                             fname =  file.path(values$projectData$projectFolder, 
                                                file.path(dirname(reactives()$filename),
                                                          if(!is.null(input$selFormat) && input$selFormat == "instrumentList"){paste0(input$tabname,".txt")}
                                                          else if(!is.null(input$selFormat) && input$selFormat == "MetaboAnalyst"){paste0(input$tabname,"_forMetaboAnalyst.csv")}
                                                          else{input$tabname})),
                             format = if(static$format =="tsv"){"tsv"}else if(!is.null(input$selFormat)){input$selFormat}else{"csv"},
                             moreArgs = list(rtwin = input$incRTw,
                                             polarity = input$incPolarity,
                                             instrument = "QExactive",
                                             listType = input$incType,
                                             restrictRT = input$incUseRTw))
      
      
      
      
      showNotification(paste("Table saved as: ", file.path(values$projectData$projectFolder, 
                                                           file.path(dirname(reactives()$filename),
                                                                     if(!is.null(input$selFormat) && input$selFormat == "instrumentList"){paste0(input$tabname,".txt")}
                                                                     else{input$tabname})), duration = 10))
        }
      removeModal()
      
    }
    else{
      showNotification(paste("You have to work in a Project Folder to save files this way!"), type = "error", duration = 10)
    }
        
    },
    error = function(e){
        showNotification(paste("ERROR: Table was NOT saved: ",e), type = "error", duration = NULL)
        
        })
  })
  
  observeEvent(input$modalSaveInSession,{
    tryCatch({
      if(!input$tabname %in% sapply(values$featureTables$tables, "[[", "tablename")){
        
        
        if(!is.null(values$featureTables)){
          updateFT(values)
        }
        
        if(length(values$featureTables$Maintable$sortCheck) 
           && values$featureTables$Maintable$sortCheck){
          srt <-  values$featureTables$Maintable$sortBy
        }else{
          srt <- character()
        }
        
        if(length(srt) 
           && length(values$featureTables$Maintable$decreasing)){
          dec <-  values$featureTables$Maintable$decreasing
        }else{
          dec <-  TRUE
        }
        
          newFT <- FTFilter(FeatureTable(values),
                          filters = getFilters(values),
                          sortBy = srt,
                          decreasing = dec)
          
          newFT <- rename(newFT, input$tabname)
          
          FeatureTable(values, tableID = paste0("table",length(values$featureTables$tables))) <- newFT #this adds the FeatureTable and handles updating the FTIndex
          
          showNotification(paste("Added filtered table to session: ", 
                                 input$tabname),
                           #paste0(strftime(Sys.time(),"%Y%m%d_%H%M%S"),basename(reactives()$filename))),
                           duration = 10)
        
         # values$featureTables$active <- tabid
        removeModal()
        
      }
      else{
        showNotification(paste("Make sure the name of the new table is not the same as that of any other Feature Table currently loaded into this Session"), type = "error", duration = 10)
      }
      
    },
    error = function(e){
      showNotification(paste("ERROR: Table was NOT saved: ",e), type = "error", duration = NULL)
      
    })
  })
  
}

#' @describeIn SaveTableModule UI elements
#' @export
SaveTableModuleUI <- function(id)
{
  ns <- NS(id)
  
  htmlOutput(ns("saveTableButton"))
  
}