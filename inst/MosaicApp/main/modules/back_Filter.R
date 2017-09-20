FilterModuleUI <- function(id){
    ns <- NS(id)
    tagList(div(style="display:inline-block",
                div(style="display:inline-block",htmlOutput(ns('colSel'))),
        div(style="display:inline-block",checkboxInput(ns('toggler'), ''))),
        div(style="display:inline-block",
        div(style="display:inline-block",
            htmlOutput(ns('minSel'))),
            div(style="display:inline-block",
        htmlOutput(ns('maxSel')))),
        div(style="display:inline-block",
            htmlOutput(ns('modeSel'))),
        div(style="display:inline-block",
            htmlOutput(ns('txtSel'))),
        
        verbatimTextOutput(ns('insider'))
    )
    
}

FilterModule <- function(input, output, session, tag, df){
    
    ns <- NS(tag)
#    reactive({paste0(df(),"!!!")})
    fu <- reactive({   df()})
    output$colSel <- renderUI({selectizeInput(ns('colSel'), 'Filter',
                                                choices = colnames(fu()),
                                                multiple = F)})
    inp <- reactive({input$colSel})
    
    output$insider <- renderPrint({print(selrows())})
    
    cond <- reactive({!is.na(as.numeric(df()[1,input$colSel]))})
    

    output$minSel <- renderUI({if(cond()){numericInput(ns('minSel'), 'min.', value = min(fu()[,input$colSel]), width = '120px')}})
    output$maxSel <- renderUI({if(cond()){numericInput(ns('maxSel'), 'max.', value = max(fu()[,input$colSel]), width = '120px')}})
    
    output$modeSel <- renderUI({if(!cond()){selectizeInput(ns('modeSel'), '',
                                              choices = c("contains","is"),
                                              multiple = F)}})
    output$txtSel <- renderUI({if(!cond()){textInput(ns('txtSel'), 'String', width = '120px')}})
    
    
    selrows <- reactive({if(input$toggler){
        if(cond()){row.names(fu())[which(as.numeric(fu()[,input$colSel]) >= as.numeric(input$minSel)
                                                    & as.numeric(fu()[,input$colSel]) <= as.numeric(input$maxSel))]
        
    }else{
        if(input$modeSel=="contains"){
        row.names(fu())[grep(input$txtSel,as.character(fu()[,input$colSel]))]}
        #if(input$modeSel=="is"){
        else{
            row.names(fu())[which(fu()[,input$colSel] == input$txtSel)]}
    }
    }
    })
    
    return(selrows)

  #  return(reactive({list(col = typeof(df()[,input$colSel]), ty = "numeric",
  #                        min = input$minSel, max = input$maxSel)}))
# })
}