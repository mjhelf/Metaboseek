#' MultiSpecmodule
#' 
#' 
#' server module for multiple interactive mass spectrum views
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param tag id to be used in ns()
#' @param set Import data from the shiny session
#' 
#' @export 
MultiSpecmodule <- function(input,output, session, tag, set = list(spec = list(xrange = NULL,
                                                                               yrange = NULL,
                                                                               maxxrange = NULL,
                                                                               maxyrange = NULL,
                                                                               sel = NULL,
                                                                               mz = NULL,
                                                                               data = NULL,
                                                                               MS2 = T),
                                                                   layout = list(lw = 1,
                                                                                 cex = 1,
                                                                                 controls = F,
                                                                                 ppm = 5,
                                                                                 active = T,
                                                                                 height = 350),
                                                                   msdata = NULL),
                            keys,
                            static = list(title = "MS spectra")){
  
  ns <- NS(tag)
  
  defaultSet <- list(spec = list(xrange = NULL,
                                 yrange = NULL,
                                 maxxrange = NULL,
                                 maxyrange = NULL,
                                 sel = NULL,
                                 mz = NULL,
                                 data = NULL,
                                 MS2 = T),
                     layout = list(lw = 1,
                                   cex = 1,
                                   controls = F,
                                   ppm = 5,
                                   active = F,
                                   highlights = NULL,
                                   height = 550),
                     msdata = NULL)
  
  selections <- reactiveValues(plots = list(spec = list(keep = logical(5),
                                                        compare = !logical(5),
                                                        delete = logical(5),
                                                        active = logical(5)),
                                            sets = list("Spec1" = defaultSet,
                                                        "Spec2" = defaultSet,
                                                        "Spec3" = defaultSet,
                                                        "Spec4" = defaultSet,
                                                        "Spec5" = defaultSet),
                                            global = list(xranges = NULL,
                                                          yranges = NULL,
                                                          maxxrange = NULL,
                                                          maxyranges = NULL,
                                                          height = 550,
                                                          compare = T)#copies of set() to check if set() has changed
  )
  )
  
  observeEvent(set(),{
    if(set()$layout$active && !is.null(set()$spec$sel$File) 
       #&& !identical(selections$plots$set,set()$spec )
    ){
      selSpec <- which(!selections$plots$spec$keep)
      
      if(length(selSpec) > 0 ){
        selections$plots$sets[[selSpec[1]]]$spec <- set()$spec
        selections$plots$sets[[selSpec[1]]]$layout <- set()$layout
        selections$plots$sets[[selSpec[1]]]$msdata <- set()$msdata
        
        selections$plots$sets[[selSpec[1]]]$spec$maxxrange <- selections$plots$global$maxxrange
      }
      
      #print(selections$plots$sets[[1]]$spec)
    }
    
  })
  #unlist(sapply(setstest,"[[","layout")["active",])
  
  observeEvent(selections$plots$spec$delete,{
    if(any(selections$plots$spec$delete)){
      selections$plots$spec$active <- unlist(lapply(lapply(selections$plots$sets,"[[","layout"),"[[", "active"))
      
      delnum <- which(selections$plots$spec$delete)
      activenum <- which(selections$plots$spec$active)
      
      #move spectra up if the deleted one is not the last one
      if(delnum != max(activenum)){
        for(i in activenum[which(activenum > delnum)]){
          selections$plots$sets[[i-1]] <- selections$plots$sets[[i]]
          
        }
      }
      
      #inactivate the deleted spectrum or the last spectrum in list if spectra were moved up
      selections$plots$sets[[max(activenum)]]$layout$active <- FALSE
      
      
      #reset deletion trigger
      isolate(selections$plots$spec$delete[delnum] <- FALSE)
    }
  })
  
  observeEvent(c(Spec1()$spec$data,Spec2()$spec$data,Spec3()$spec$data,Spec4()$spec$data,Spec5()$spec$data,
                 selections$plots$global$compare, selections$plots$spec$compare),{
                   #print(sapply(selections$plots$sets,"[[","layout"))
                   if(any(unlist(lapply(lapply(selections$plots$sets,"[[","layout"),"[[", "active")))
                   ){
                     
                     #list all spectra
                     
                     complist <- list()
                     rangelist <- list()
                     for (i in seq(length(selections$plots$sets))){
                       if (selections$plots$sets[[i]]$layout$active && selections$plots$spec$compare[i]){
                         out <- eval(call(paste0("Spec",i)))
                         #print(out)
                         if(!is.null(out$spec$data)){
                           complist[[paste0("Spec",i)]] <- out$spec$data
                           rangelist[[paste0("Spec",i)]] <- out$spec$xrange
                         }
                       }
                     }
                     
                     
                     
                     
                     
                     if(length(complist) >0){
                       comp <- mergeMS(complist)
                       
                       selections$plots$global$maxxrange <- range(comp$merged[,1]) + c(-1,1)
                       
                       #harmonize maxxrange
                       for (i in names(complist)){
                         #print(dfr)
                         if(!all(selections$plots$sets[[i]]$spec$maxxrange == selections$plots$global$maxxrange)){
                           selections$plots$sets[[i]]$spec$maxxrange <- selections$plots$global$maxxrange
                         }
                         
                       }
                       
                       #find spectrum that has a new xrange
                       newrange <- sapply(rangelist,identical,selections$plots$global$xrange)
                       
                       if(any(!newrange)){
                         
                         newxr <- rangelist[[which(!newrange)[1]]]
                         
                         if(identical(newxr,selections$plots$global$maxxrange)){
                           
                           selections$plots$global$xrange <- NULL
                           
                           for(i in names(newrange)){
                             selections$plots$sets[[i]]$spec$xrange <- selections$plots$global$xrange
                           }
                           
                         }else{
                           #set global xrange to that new range
                           selections$plots$global$xrange <- rangelist[[which(!newrange)[1]]]
                           selections$plots$global$xrangeCache <- rangelist[[which(!newrange)[1]]]
                           
                           
                           #set all old/ different xranges to this range
                           if(length(newrange >1)){
                             for(i in names(newrange)#[which(newrange)]
                             ){
                               selections$plots$sets[[i]]$spec$xrange <- selections$plots$global$xrange
                             }
                           }
                         }
                       }
                       
                       #set comparison highlights
                       if(selections$plots$global$compare){
                         #print(comp)
                         #print(comp$intensity)
                         
                         sel <- which(comp$counts > 1)
                         if(length(sel > 0)){
                           for (i in colnames(comp$mz)){
                             
                             dfr <- data.frame(mz = na.omit(comp$mz[sel,i]),
                                               intensity = na.omit(comp$intensity[sel,i]))
                             #print(dfr)
                             if(nrow(dfr)>0){
                               selections$plots$sets[[i]]$layout$highlights <- dfr
                             }
                             
                           }
                         } 
                       }
                     }
                     if(!is.null(input$checkCompare)){
                       selections$plots$global$compare <- input$checkCompare
                     }
                     
                     for(i in seq(length(selections$plots$sets))){
                       # print(selections$plots$global$compare)
                       if(is.null(set()$layout$highlights) #only remove highlights if they have not been externally provided
                          &&(
                          !selections$plots$global$compare 
                          || !selections$plots$spec$compare[i] #remove highlights in spectra that are not in comparison
                          || length(complist) == 1)) #this is 0 if there is no peak in the comparison that is in more than one spectrum (e.g. if only one spectrum loaded)
                       {
                         selections$plots$sets[[i]]$layout$highlights <- NULL
                       }
                     }
                   }
                   #print(selections$plots$sets[[1]]$layout)
                 })
  
  output$pdfButton <- downloadHandler(filename= function(){
    titleout <- "spectrum"
    
    return(paste0(titleout,".pdf"))}, 
    content = function(file){
      
      pdf(file,
          14,6
      )
      
      if(any(unlist(lapply(lapply(selections$plots$sets,"[[","layout"),"[[","active")))){
        for (i in seq(length(selections$plots$sets))){
          
          if (selections$plots$sets[[i]]$layout$active){
            out <- eval(call(paste0("Spec",i)))
            if(!is.null(out$spec$fullplot)){
              replayPlot(out$spec$fullplot)
            }
          }
        }
      }
      #replayPlot(selections$plots$spec$fullplot)
      dev.off()
      
    },
    
    
    
    contentType = "application/pdf")
  
  
  Spec1 <- callModule(Specmodule,"Spec1", tag = ns("Spec1"),
                      set = reactive({selections$plots$sets[[1]]}),
                      keys = reactive({keys()})
  )
  
  Spec2 <- callModule(Specmodule,"Spec2", tag = ns("Spec2"),
                      set = reactive({selections$plots$sets[[2]]}),
                      keys = reactive({keys()})
  )
  
  Spec3 <- callModule(Specmodule,"Spec3", tag = ns("Spec3"),
                      set = reactive({selections$plots$sets[[3]]}),
                      keys = reactive({keys()})
  )
  
  Spec4 <- callModule(Specmodule,"Spec4", tag = ns("Spec4"),
                      set = reactive({selections$plots$sets[[4]]}),
                      keys = reactive({keys()})
  )
  
  Spec5 <- callModule(Specmodule,"Spec5", tag = ns("Spec5"),
                      set = reactive({selections$plots$sets[[5]]}),
                      keys = reactive({keys()})
  )
  
  observeEvent(Spec1()$spec$marker,{
    selections$marker <- Spec1()$spec$marker
  })
  observeEvent(Spec2()$spec$marker,{
    selections$marker <- Spec2()$spec$marker
  })
  observeEvent(Spec3()$spec$marker,{
    selections$marker <- Spec3()$spec$marker
  })
  observeEvent(Spec4()$spec$marker,{
    selections$marker <- Spec4()$spec$marker
  })
  observeEvent(Spec5()$spec$marker,{
    selections$marker <- Spec5()$spec$marker
  })
  
  
  output$multiOut <-renderUI({
    
    actives <- unlist(lapply(lapply(selections$plots$sets,"[[","layout"),"[[","active"))
    
    fluidPage(
      fluidRow(
        column(3,
               h4(static$title)),
        column(3,
               downloadButton(ns('pdfButton'), "Download spectra")),
        column(3,
               checkboxInput(ns('checkCompare'), "allow comparisons", value = selections$plots$global$compare)
        )
        
      ),
      
      
      if(actives[1]){
        fluidRow(
          column(3,
                 actionButton(ns('removespec1'), "Remove")),
          column(3,
                 checkboxInput(ns('checkkeep1'), "Keep", value = selections$plots$spec$keep[1])
          ),
          column(3,
                 checkboxInput(ns('checkcompare1'), "Compare", value = selections$plots$spec$compare[1])
                 
          )
        )}else{NULL}
      ,
      if(actives[1]){
        fluidRow(
          SpecmoduleUI(ns('Spec1'))
        )}else{NULL}
      ,
      if(actives[2]){
        fluidRow(
          column(3,
                 actionButton(ns('removespec2'), "Remove")),
          column(3,
                 checkboxInput(ns('checkkeep2'), "Keep", value = selections$plots$spec$keep[2])),
          column(3,
                 checkboxInput(ns('checkcompare2'), "Compare", value = selections$plots$spec$compare[2])
                 
          )
          
        )}else{NULL}
      ,
      if(actives[2]){
        fluidRow(
          SpecmoduleUI(ns('Spec2'))
        )}else{NULL}
      ,
      if(actives[3]){
        fluidRow(
          column(3,
                 actionButton(ns('removespec3'), "Remove")),
          column(3,
                 checkboxInput(ns('checkkeep3'), "Keep", value = selections$plots$spec$keep[3])),
          column(3,
                 checkboxInput(ns('checkcompare3'), "Compare", value = selections$plots$spec$compare[3])
                 
          )
          
        )}else{NULL}
      ,
      if(actives[3]){
        fluidRow(
          SpecmoduleUI(ns('Spec3'))
        )}else{NULL}
      ,
      if(actives[4]){
        fluidRow(
          column(3,
                 actionButton(ns('removespec4'), "Remove")),
          column(3,
                 checkboxInput(ns('checkkeep4'), "Keep", value = selections$plots$spec$keep[4])),
          column(3,
                 checkboxInput(ns('checkcompare4'), "Compare", value = selections$plots$spec$compare[4])
                 
          )
          
        )}else{NULL}
      ,
      if(actives[4]){
        fluidRow(
          SpecmoduleUI(ns('Spec4'))
        )}else{NULL}
      ,
      if(actives[5]){
        fluidRow(
          column(3,
                 actionButton(ns('removespec5'), "Remove")),
          column(3,
                 checkboxInput(ns('checkkeep5'), "Keep", value = selections$plots$spec$keep[5])),
          column(3,
                 checkboxInput(ns('checkcompare5'), "Compare", value = selections$plots$spec$compare[5])
                 
          )
          
        )}else{NULL}
      ,
      if(actives[5]){
        fluidRow(
          SpecmoduleUI(ns('Spec5'))
        )}else{NULL}
      
      
    )
  })
  
  observeEvent(input$checkkeep1,{
    selections$plots$spec$keep[1] <- input$checkkeep1
    #selections$plots$sets[[1]]$spec$layout$active <- input$checkkeep1
    
  })
  
  observeEvent(input$checkkeep2,{
    selections$plots$spec$keep[2] <- input$checkkeep2
    
  })
  
  observeEvent(input$checkkeep3,{
    selections$plots$spec$keep[3] <- input$checkkeep3
    
  })
  
  observeEvent(input$checkkeep4,{
    selections$plots$spec$keep[4] <- input$checkkeep4
    
  })
  
  observeEvent(input$checkkeep5,{
    selections$plots$spec$keep[5] <- input$checkkeep5
    
  })
  
  
  #####Compare toggle
  observeEvent(input$checkcompare1,{
    selections$plots$spec$compare[1] <- input$checkcompare1
  })
  
  observeEvent(input$checkcompare2,{
    selections$plots$spec$compare[2] <- input$checkcompare2
  })
  
  observeEvent(input$checkcompare3,{
    selections$plots$spec$compare[3] <- input$checkcompare3
  })
  
  observeEvent(input$checkcompare4,{
    selections$plots$spec$compare[4] <- input$checkcompare4
  })
  
  observeEvent(input$checkcompare5,{
    selections$plots$spec$compare[5] <- input$checkcompare5
  })
  
  
  observeEvent(input$removespec1,{
    selections$plots$spec$delete[1] <- TRUE
  })
  observeEvent(input$removespec2,{
    selections$plots$spec$delete[2] <- TRUE
  })
  observeEvent(input$removespec3,{
    selections$plots$spec$delete[3] <- TRUE
  })
  observeEvent(input$removespec4,{
    selections$plots$spec$delete[4] <- TRUE
  })
  observeEvent(input$removespec5,{
    selections$plots$spec$delete[5] <- TRUE
  })
  
  return(selections)
  
}
#' MultiSpecmoduleUI
#' 
#' 
#' UI module for interactive view of multiple spectra
#' 
#' @param id id to be used in ns()
#' 
#' @export
MultiSpecmoduleUI <-  function(id){
  ns <- NS(id)
  htmlOutput(ns("multiOut"))
}