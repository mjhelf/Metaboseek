#' @title RatioPlotModule
#'
#' @description This is a ratio plotter
#'    
#' @return This module returns nothing
#' 
#' @examples
#' \dontrun{
#' MseekExamplePreload()
#' 
#' tab1 <- FTNormalize(tab1)
#' 
#' ui <- MseekMinimalUI(
#'   fluidPage(
#'     fluidRow(
#'       RatioPlotModuleUI("examplewidget")),
#'     fluidRow(
#'       MainTableModuleUI("mtb"))
#'   ),
#'   diagnostics = T)
#' 
#' server <- function(input, output) {
#'   MseekMinimalServer(diagnostics = T, data = F, tables = T)
#'   
#'   MainTable <- callModule(MainTableModule, "mtb",
#'                           values = reactiveValues(featureTables = featureTables,
#'                                                  GlobalOpts = GlobalOpts),
#'                          static = list(perpage = 100,
#'                                         height = 300,
#'                                         readOnly = T,
#'                                         contextMenu = T,
#'                                         fixedColumnsLeft = 1,
#'                                         invertReadOnly = NULL,
#'                                         controls = T,
#'                                         format = list(col = NULL,
#'                                                       format = NULL)))
#'   
#'   ExampleWidget <- callModule(RatioPlotModule, "examplewidget",
#'                               values = values)
#'   
#'   
#' }
#' 
#' # Create Shiny app ----
#' shinyApp(ui, server)
#' }
#' 
#' VennDiagramModule
#' 
#' server module for Venn diagrams in Metaboseek
#' 
#' @inherit MseekModules
#' @describeIn RatioPlotModule Server logic
#' 
#' 
#' @export 
RatioPlotModule <- function(input,output, session, values){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(
    plotprops = NULL,
    
    Numerator =  callModule(MultiFilterModule, "numerator", values = values,
                            static = list(lab = "numerator", #note: 'num' for instance causes a conflict with numFils internalValue object in grep()
                                          modFeatureTable = F,
                                          name = "Numerator",
                                          activate = T)),
    
    Denominator =  callModule(MultiFilterModule, "denominator", values = values,
                              static = list(lab = "denom",
                                            modFeatureTable = F,
                                            name = "Denominator",
                                            activate = T)),
    updateTrigger = 1
  )
  
  observeEvent(values$featureTables$Maintable$selected_rows,{
    
    if(length(values$featureTables$Maintable$selected_rows) > 0
       && is.logical(input$autoUpdate)
       && input$autoUpdate){
      
      internalValues$updateTrigger <- internalValues$updateTrigger + 1 
    }
    
  })
  
  observeEvent(input$ratioButton,{
    internalValues$updateTrigger <- internalValues$updateTrigger + 1 
  })
  
  observeEvent(internalValues$updateTrigger,{
    
    if((is.logical(internalValues$Numerator$active)
        && !is.null(internalValues$Numerator$results))
       || (is.logical(internalValues$Denominator$active)
           && !is.null(internalValues$Denominator$results))){
      
      intensCols <- intensityCols(FeatureTable(values))
      
      if(input$useNormalized){
        intensCols <- paste0(intensCols, "__norm")
        if(!all(intensCols %in% colnames(FeatureTable(values)$df))){
          showNotification(paste("No normalized intensity Values available!"), duration = 0, type = "error")
          return(NULL)
        }
      }
      dens <- 1
      nums <- 1
      
      if(input$numeratorSelect == "Feature Table Selection"){
        nums <-    sapply(FTselection(values)[,intensCols],sum)
      }else{
        
        if(!is.null(internalValues$Numerator$results) 
           && !is.null(internalValues$Numerator$active) 
           && internalValues$Numerator$active){
          
          numres <- if(length(internalValues$Numerator$results) == 1){
            rep(TRUE,nrow(FeatureTable(values)$df))}else{internalValues$Numerator$results}
          
          
          if(input$preFilter && !is.null(values$featureTables$row_filters)){
            
            numres <- numres & values$featureTables$row_filters
            
          }
          nums <-    sapply(FeatureTable(values)$df[numres,intensCols],sum)
        }
      }
      
      if(input$denominatorSelect == "Feature Table Selection"){
        dens <-    sapply(FTselection(values)[,intensCols],sum)
      }else{
        if(!is.null(internalValues$Denominator$results) 
           && !is.null(internalValues$Denominator$active) 
           && internalValues$Denominator$active){
          
          denres <- if(length(internalValues$Denominator$results) == 1){
            rep(TRUE,nrow(FeatureTable(values)$df))}else{internalValues$Denominator$results}
          
          
          if(input$preFilter && !is.null(values$featureTables$row_filters)){
            
            denres <- denres & values$featureTables$row_filters
            
          }
          dens <-    sapply(FeatureTable(values)$df[denres,intensCols],sum)
          
          
        }
      }
      
      plotdata <- data.frame(Ratio = nums/dens,
                             File = names(nums/dens), #duplicating the division operation should not have a significant performance downside
                             Numerator = nums,
                             Denominator = dens,
                             stringsAsFactors = FALSE)
      plotdata$Group <- NA_character_
      
      for(n in names(FeatureTable(values)$anagroupnames)){
        if(input$useNormalized){
          plotdata$Group[plotdata$File %in% FeatureTable(values)$anagroupnames_norm[[n]]] <- n
        }else{
          plotdata$Group[plotdata$File %in% FeatureTable(values)$anagroupnames[[n]]] <- n
        }
        
      }
      
      internalValues$plotdata <- plotdata
    }
    
  },
  ignoreInit = TRUE
  )
  
  callModule(PlotModule, "ratioplot", reactives = reactive({
    
    p <- NULL
    if(is.data.frame(internalValues$plotdata)){
      
      ptitle = paste(internalValues$Numerator$name, "/", internalValues$Denominator$name)
      
      
      p <- ggplot(internalValues$plotdata, aes_string('Group','Ratio'))+
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
        geom_bar(stat = "summary", width =.7, fun = "mean")+
        geom_point(aes(color = File), size =2)+
        ggtitle(ptitle)+
        theme(plot.title = element_text(hjust = 0.5, size =20, vjust = 5, face = "bold"))+
        labs(x= "Group", y = "Ratio")+
        stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                     geom="errorbar", color="black", width=0.2)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        theme(axis.title.y = element_text(size =18, vjust =2))+
        theme(axis.title.x = element_text(size =20))+
        theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
      
    }
    
    
    return(
      list(plot = p,
           interactive = T)
    )
  }))
  
  return(internalValues)
  
}

#' @describeIn RatioPlotModule UI elements
#' @export
RatioPlotModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(column(2,
                    
                    actionButton(ns("ratioButton"), "Display Ratios")
    ),
    column(2,
           
           checkboxInput(ns("autoUpdate"), "AutoUpdate", value = F)
    ),
    column(2,
           
           checkboxInput(ns("useNormalized"), "Use Normalized Intensities", value = T)
    ),
    column(2,
           
           checkboxInput(ns("preFilter"), "Apply global table filter", value = T)
    )
    ),
    hr(),
    fluidRow(    
      column(6,PlotModuleUI(ns("ratioplot"))),
      
      column(3,
             shinydashboard::box(width = 12,
                                 fluidRow(
                                   radioButtons(ns('numeratorSelect'), "Use...",
                                                choices = c('Feature Table Selection', 'Filter'),
                                                selected = 'Feature Table Selection',
                                                inline = TRUE)
                                 ),
                                 fluidRow(
                                   MultiFilterModuleUI(ns("numerator"))
                                 )
             )
      ),
      column(3,
             shinydashboard::box(width = 12,
                                 fluidRow(
                                   radioButtons(ns('denominatorSelect'), "Use...",
                                                choices = c('Feature Table Selection', 'Filter'),
                                                selected = 'Filter',
                                                inline = TRUE)
                                 ),
                                 fluidRow(
                                   MultiFilterModuleUI(ns("denominator"))
                                 )
             )
      )
    )
  )
  
}