#' FilterModuleUI
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param id
#' 
#' @export
FilterModuleUI <- function(id){
    ns <- NS(id)
    if(!is.null(htmlOutput(ns('colSel')))){
    tagList(
        hr(),
        fluidRow(
            column(2,
                   htmlOutput(ns('toggler'))
#                   checkboxInput(ns('toggler'), 'activate')
                   ),
            column(5,    
                   htmlOutput(ns('colSel'))),
            column(5,
                   div(style="display:inline-block",htmlOutput(ns('minSel'))),
                   div(style="display:inline-block",htmlOutput(ns('maxSel')),
                       htmlOutput(ns('modeSel'))),
                   htmlOutput(ns('txtSel')))),
        
                verbatimTextOutput(ns('insider'))
    )
    }
}

#' FilterModule
#' 
#' Module to apply filters to a featureTable (UI)
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param lab heading of this module in UI
#' @param tag same as id in UI module
#' @param df data frame to be filtered
#' @param presets Mosaic featureTable object filter$filter
#' 
#' @export
FilterModule <- function(input, output, session, lab = "Filter", tag, df, presets){
    
    ns <- NS(tag)
    #    reactive({paste0(df(),"!!!")})
    fu <- reactive({   df()})
    
    output$toggler <- renderUI({checkboxInput(ns('toggler'), 'activate', value = presets()$active)})
    
   
    #inp <- reactive({input$colSel})
    
    output$insider <- renderPrint({print(summary(df()[,input$colSel])
    )})
    
    output$colSel <- renderUI({selectizeInput(ns('colSel'), lab,
                                              choices = colnames(fu()),
                                              selected = presets()$column,
                                              multiple = F)}) 

    #returns TRUE if the selected column is numeric
    cond <- reactive({!is.na(as.numeric(df()[1,input$colSel]))})
    mins <- reactive({
      if(!is.na(input$colSel) && (is.null(presets()$minSel) | input$colSel != presets()$column)){min(fu()[,input$colSel])*0.99}else{presets()$minSel}
    })
    maxs <- reactive({
      if(!is.na(input$colSel) && (is.null(presets()$minSel) | input$colSel != presets()$column)){max(fu()[,input$colSel])*1.01}else{presets()$maxSel}
      
    })
  #  if(!is.null(cond()){
    output$minSel <- renderUI({if(length(cond()) !=0 && cond()){numericInput(ns('minSel'), 'min.', 
                                                       value = mins(),
                                                       min = min(df()[,input$colSel])*0.99,
                                                       max = max(df()[,input$colSel])*1.01,
                                                       width = '120px')}})
    
    output$maxSel <- renderUI({if(length(cond()) !=0 && cond()){numericInput(ns('maxSel'), 'max.',
                                                       value = maxs(),
                                                       min = min(df()[,input$colSel])*0.99,
                                                       max = max(df()[,input$colSel])*1.01,
                                                       width = '120px')}})
    
    output$modeSel <- renderUI({if(length(cond()) !=0 && !cond()){selectizeInput(ns('modeSel'), '',
                                                           choices = c("contains","is"),
                                                           selected = presets()$modeSel,
                                                           multiple = F)}})
    
    output$txtSel <- renderUI({if(length(cond()) !=0 && !cond()){textInput(ns('txtSel'), 'String',
                                                     value = presets()$txtSel,
                                                     width = '120px')}})
    
  
    selrows <- reactive({
        if(length(input$toggler) !=0 && input$toggler){
            if(cond()){row.names(fu())[which(as.numeric(fu()[,input$colSel]) >= as.numeric(input$minSel)
                                             & as.numeric(fu()[,input$colSel]) <= as.numeric(input$maxSel))]
                
            }else{
                if(input$modeSel=="contains"){
                    row.names(fu())[grep(input$txtSel,as.character(fu()[,input$colSel]))]}
                #if(input$modeSel=="is"){
                else{
                    row.names(fu())[which(fu()[,input$colSel] == input$txtSel)]}
            }
        }else{
            #c(1)
            row.names(fu())
            
        }
    })
    
    return(reactive({
      if(length(cond())==0){presets()}
      else{
      list(selected = selrows(),
           column = if(length(cond()) !=0){input$colSel}else{presets()$column},
           minSel = if(length(cond()) !=0 && cond()){input$minSel}else{NULL},
           maxSel = if(length(cond()) !=0 && cond()){input$maxSel}else{NULL},
           modeSel = if(length(cond()) !=0 && !cond()){input$modeSel}else{NULL},
           txtSel = if(length(cond()) !=0 && !cond()){input$txtSel}else{""},
           active = input$toggler
      )
      }
    })
    
    )

}