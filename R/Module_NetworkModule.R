#' NetworkModule
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
NetworkModule <- function(input,output, session, 
                          values = reactiveValues(Networks = Networks),
                          reactives = reactive({list(active = T,
                                                     highlights = integer(0) # fixed__id values of nodes to be highlighted
                                                     )
                            }),
                          static = list(noSelection = T),
                          keys
){
  
  ns <- NS(session$ns(NULL))
  internalValues <- reactiveValues(graph = NULL,tables = NULL,
                                   xrange = NULL, #the x axis range
                                   yrange = NULL, #the y axis range
                                   maxxrange = NULL, #maximum x axis range
                                   maxyrange = NULL, #maximum y axis range
                                   layouts = NULL,
                                   activelayout = list(graph = NULL,
                                                       layout = NULL,
                                                       subg = NULL),
                                   marker = NULL, #selected peak with $mz and $intensity
                                   hover = NULL, #peak hovered over with $mz and $intensity
                                   elabels = NULL,
                                   elabelcheck = F,
                                   vlabels = NULL,
                                   vlabelcheck = F,
                                   vlabelcol = NULL,
                                   vlabccheck = F,
                                   vlabcolfactors = NULL,
                                   overview = F,
                                   markgroups = NULL,
                                   activeTab = "",
                                   active = NULL, #which network is currently selected,
                                   colscale = {crange <- colorRampPalette(c("blue", "gray", "red"))
                                   crange(200)                               },
                                   sliderValues = c(0,1),
                                   highlights = integer(0),
                                   hoverActive = T,
                                   recordedPlot = NULL
  )
  
  
  output$activenetwork <- renderUI({
    if(!static$noSelection){
    selectizeInput(ns('activeNetwork'), 'Show Network', 
                   selected = input$activeNetwork, 
                   choices = names(values$Networks)[names(values$Networks) != "numNetworks"],
                   multiple = FALSE)
    }
  })  
  
  observeEvent(input$activeNetwork, { 
    if(!static$noSelection){
    internalValues$active <- input$activeNetwork
    }
  })
  
  
  observeEvent(c(internalValues$active),{
    if(!is.null(values$Networks) 
       && length(names(values$Networks)[names(values$Networks) != "numNetworks"]) > 0
      # && !is.null(internalValues$active) 
      # && internalValues$active != ""
      # && !is.null(values$Networks[[internalValues$active]]$graph)
      ){
      
      if(is.null(internalValues$active) 
         || is.na(internalValues$active)
         || internalValues$active == ""
         || is.null(values$Networks[[internalValues$active]]$graph)){
        
        internalValues$active <- names(values$Networks)[names(values$Networks) != "numNetworks"][1]
   
           }
      internalValues$marker = NULL #selected peak with $mz and $intensity
      internalValues$hover = NULL #peak hovered over with $mz and $intensity
      internalValues$elabels = NULL
      internalValues$elabelcheck = F
      internalValues$vlabels = NULL
      internalValues$vlabelcheck = F
      internalValues$vlabelcol = NULL
      internalValues$vlabccheck = F
      internalValues$vlabcolfactors = NULL
      internalValues$markgroups = NULL
      
      internalValues$tables <- values$Networks[[internalValues$active]]$tables
      internalValues$graph <- values$Networks[[internalValues$active]]$graph

      
      #generate the initial layout
      res <- layout_components_qgraph(internalValues$graph, qgraph::qgraph.layout.fruchtermanreingold)
      
      internalValues$layouts <- res
      
     # print( V(internalValues$layouts$subgraphs[[1]])$fixed__id )
      
      internalValues$activelayout$graph <- internalValues$graph
      internalValues$activelayout$layout <- norm_coords(internalValues$layouts$layout)
      colnames(internalValues$activelayout$layout) <- c("x", "y")
      
      #if there are subgraphs, start in overview mode
      if(length(internalValues$layouts$subgraphs) > 1){internalValues$overview <- T}
      
      #set the maximum x axis range to cover the spectrum data
      internalValues$maxxrange <- c(-1,1)#range(internalValues$activelayout$layout[,1])
      internalValues$maxyrange <- c(-1,1)#range(internalValues$activelayout$layout[,2])
      
      internalValues$xrange <- c(-1,1)#range(internalValues$activelayout$layout[,1])
      internalValues$yrange <- c(-1,1)#range(internalValues$activelayout$layout[,2])
      
      

      
    }
  })
  
  
  output$netinfo <- renderUI({ 
    if(!is.null(reactives()$active) && reactives()$active && !is.null(internalValues$graph)){
      if(internalValues$overview && !is.null(internalValues$hover$subgraph)){
        p("Subgraph #", internalValues$hover$subgraph, "with ",
          strong(vcount(internalValues$layouts$subgraphs[[internalValues$hover$subgraph]])),
          "nodes and",
          strong(ecount(internalValues$layouts$subgraphs[[internalValues$hover$subgraph]])),
          "edges.")
        
      }
      else{
        p(paste0(if(!is.null(internalValues$marker$vertex)){
          paste0("Marker on ", input$vlabelsel," ", vertex_attr(internalValues$activelayout$graph,input$vlabelsel, internalValues$marker$vertex))
        }
        else{""},
        if(!is.null(internalValues$hover$vertex)){
          paste0(" Cursor on ",input$vlabelsel," ", vertex_attr(internalValues$activelayout$graph,input$vlabelsel, internalValues$hover$vertex))
        }else{""}
        ))
      }
    }
    
    
  })
  
  output$controls <- renderUI({
    if(!is.null(reactives()$active) && reactives()$active && !is.null(internalValues$activelayout$graph)){
      fluidRow(
        
        if(!static$noSelection){column(4,
          htmlOutput(ns("activenetwork")))}else{tagList()},
        
        column(if(!static$noSelection){2}else{3},
               checkboxInput(ns('vlabelcheck'), "Show all node labels", value = internalValues$vlabelcheck),
               selectizeInput(ns("vlabelsel"), "Node Labels",
                              choices = names(vertex_attr(internalValues$activelayout$graph)),
                              selected = internalValues$vlabels)
        ),
        
        column(if(!static$noSelection){2}else{3},
               checkboxInput(ns('elabelcheck'), "Show all edge labels", value = internalValues$elabelcheck),
               selectizeInput(ns("elabelsel"), "Edge Labels",
                              choices = names(edge_attr(internalValues$activelayout$graph)),
                              selected = internalValues$elabels)
        ),
        column(if(!static$noSelection){2}else{3},
               checkboxInput(ns('vlabccheck'), "Color-code nodes", value = internalValues$vlabccheck),
               selectizeInput(ns("vlabelcol"), "Node Colors",
                              choices = c(NULL, names(vertex_attr(internalValues$activelayout$graph))),
                              selected = internalValues$vlabelc)
        ),
        column(if(!static$noSelection){2}else{3},
               div(title = "Activate interactive highlighting on the network plot when hovering or selecting items in the feature table. Poor performance for large networks.",
  checkboxInput(ns('hoveractive'), "Highlights", value = internalValues$hoverActive)),
sliderInput(ns("seledges"), "Filter edges",
                           min = 0, max = 1,
                              value = c(0,1) #internalValues$sliderValues
                           )
               )
      )
    }
  })
  
  observeEvent(input$hoveractive,{internalValues$hoverActive <- input$hoveractive})
  
  
  observeEvent(input$vlabelcheck,{internalValues$vlabelcheck <- input$vlabelcheck})
  
  observeEvent(input$seledges,{internalValues$sliderValues <- input$seledges})
  
  
  observeEvent(input$vlabccheck,{
    internalValues$vlabccheck <- input$vlabccheck
    if(input$vlabccheck){
      internalValues$vlabcolfactors <- as.factor(vertex_attr(internalValues$graph,input$vlabelcol))
    }
  })
  
  observeEvent(input$elabelcheck,{internalValues$elabelcheck <- input$elabelcheck})
  
  observeEvent(input$vlabelsel,{internalValues$vlabels <- input$vlabelsel})
  
  
  observeEvent(input$vlabelcol,{internalValues$vlabelc <- input$vlabelcol
  if(input$vlabccheck){
    internalValues$vlabcolfactors <- as.factor(vertex_attr(internalValues$graph,input$vlabelcol))
  }
  })
  
  observeEvent(input$elabelsel,{internalValues$elabels <- input$elabelsel})
  
  output$nettables <- renderUI({
    if(!is.null(reactives()$active) && reactives()$active && !is.null(internalValues$tables)){
      
      selectizeInput(ns('tabs'), "Show network table", choices = c("",names(internalValues$tables)), selected = internalValues$activeTab)
      
    }
  })
  observeEvent(input$tabs,{
    internalValues$activeTab <- input$tabs
    
  })
  
  
  
  output$Netw <- renderPlot({
    #print(internalValues$activelayout$graph)
    if(reactives()$active 
       && !is.null(internalValues$activelayout$graph)){
      #sc()
      # print("plotting")
      # make color vectors here (so that graph is not changed! -> infinite loop)
      
      # set all frame color to black
      fc <- rep("black", times = vcount(internalValues$activelayout$graph))
      #set all edge colors to grey
      ec <- rep("grey50", times = ecount(internalValues$activelayout$graph))
      
      #set all vertex colors
      if(internalValues$vlabccheck){
        
        if(!is.numeric(vertex_attr(internalValues$activelayout$graph,input$vlabelcol)) 
           || length(unique(vertex_attr(internalValues$activelayout$graph,input$vlabelcol))) <= 5){
        
        if(internalValues$overview){
          vc <- Mseek.colors(n = length(levels(internalValues$vlabcolfactors)), alpha = 1)[internalValues$vlabcolfactors]
          
          #make sure subgraphs use same color scheme as overview:
        }else{
          
          colassign <- as.character(vertex_attr(internalValues$activelayout$graph,input$vlabelcol))
          for (l in seq(length(levels(internalValues$vlabcolfactors)))){
            sel <- which(colassign == levels(internalValues$vlabcolfactors)[l])
            if(length(sel) >0){
              colassign[sel] <- l
            }
          }
          vc <- Mseek.colors(n = length(levels(internalValues$vlabcolfactors)), alpha = 1)[as.integer(colassign)]
        }
        }else{
          
          vc <- assignColor(vertex_attr(internalValues$activelayout$graph,input$vlabelcol), internalValues$colscale)
          
        }
        
      }else{
        vc <- rep("olivedrab1", times = vcount(internalValues$activelayout$graph))
      }
      
      #edge labels
      elabs <- if(internalValues$elabelcheck){
        if(is.numeric(edge_attr(internalValues$activelayout$graph,input$elabelsel))){
          round(edge_attr(internalValues$activelayout$graph,input$elabelsel),2)}
        else{edge_attr(internalValues$activelayout$graph,input$elabelsel)}
      }else{rep(NA, times = ecount(internalValues$activelayout$graph))}
      
      #edge label color and font
      elabc <- rep("blue", times = ecount(internalValues$activelayout$graph))
      elabf <- rep(1, times = ecount(internalValues$activelayout$graph))
      
      #vertex labels
      vlabs <- if(internalValues$vlabelcheck){
        if(is.numeric(vertex_attr(internalValues$activelayout$graph,input$vlabelsel))){
          as.character(round(vertex_attr(internalValues$activelayout$graph,input$vlabelsel), 5))
          
        }else{
        as.character(vertex_attr(internalValues$activelayout$graph,input$vlabelsel))
        }
      }else{rep(NA, times = vcount(internalValues$activelayout$graph))}
      
      #vertex label color and font
      vlabc <- rep("black", times = vcount(internalValues$activelayout$graph))
      vlabf <- rep( 2, times = vcount(internalValues$activelayout$graph))
      
      
      
      #recolor based on hovering
      if(!is.null(internalValues$hover) && !internalValues$overview){
        
        #which vortex is hovered over
        sel <- internalValues$hover$vertex
        neigh <- neighbors(internalValues$activelayout$graph, V(internalValues$activelayout$graph)[sel] )  
        edg <- incident(internalValues$activelayout$graph,V(internalValues$activelayout$graph)[sel])
        #recolor 
        fc[sel] <- "cyan"
        fc[neigh] <- "lightcyan3"
        ec[edg] <- "cyan"
        elabs[edg] <- if(is.numeric(edge_attr(internalValues$activelayout$graph,input$elabelsel))){
          round(edge_attr(internalValues$activelayout$graph,input$elabelsel)[edg],2)}
        else{edge_attr(internalValues$activelayout$graph,input$elabelsel)[edg]}
        elabc[edg] <- "blue"
        elabf[edg] <- 2
        
        vlabs[c(sel,neigh)] <- if(is.numeric(vertex_attr(internalValues$activelayout$graph,input$vlabelsel)[c(sel,neigh)])){
          as.character(round(vertex_attr(internalValues$activelayout$graph,input$vlabelsel)[c(sel,neigh)], 5))
          
        }else{as.character(vertex_attr(internalValues$activelayout$graph,input$vlabelsel)[c(sel,neigh)])}
        vlabc[c(sel,neigh)] <- "black"
        vlabf[sel] <- 4
      }
      
      #recolor based on marking (overrides hover colors)
      if(!is.null(internalValues$marker) && !internalValues$overview){
        sel <- internalValues$marker$vertex
        neigh <- neighbors(internalValues$activelayout$graph, V(internalValues$activelayout$graph)[sel] ) 
        edg <- incident(internalValues$activelayout$graph,V(internalValues$activelayout$graph)[sel])
        
        fc[sel] <- "red"
        fc[neigh] <- "orange"
        #vc[sel] <- "indianred3"
        #vc[neigh] <- "indianred1"  
        ec[edg] <- "red"
        elabs[edg] <- if(is.numeric(edge_attr(internalValues$activelayout$graph,input$elabelsel))){
          round(edge_attr(internalValues$activelayout$graph,input$elabelsel)[edg],2)}
        else{edge_attr(internalValues$activelayout$graph,input$elabelsel)[edg]}
        elabc[edg] <- "darkorange2"
        elabf[edg] <- 2
        
        
        vlabs[c(sel,neigh)] <- as.character(vertex_attr(internalValues$activelayout$graph,input$vlabelsel)[c(sel,neigh)])
        vlabc[c(sel,neigh)] <- "black"
        vlabf[sel] <- 4
        
      }
      
      #scale node width based on label width
      if(!internalValues$overview){
        scalingH <- max(420*max(strwidth(vlabs, units = "figure")),
                        #420*strwidth(as.character(vertex_attr(internalValues$activelayout$graph,input$vlabelsel)[1]),
                         #            units = "figure")
                        420*max(strwidth("A", units = "figure"))
                        )
        scalingV <- 550*strheight(vertex_attr(internalValues$activelayout$graph,input$vlabelsel)[1], units = "figure")
        
      }else{
        #in overview mode, just show squares with pleasant aspect ratio
        scalingH <- 70/sqrt(vcount(internalValues$activelayout$graph))
        scalingV <- (70/sqrt(vcount(internalValues$activelayout$graph)))/1.414
        
      }
      
      ew <- rep(2, ecount(internalValues$activelayout$graph))
      
      if(length(internalValues$sliderValues) > 0){
       hidethese <-  which(edge_attr(internalValues$activelayout$graph,"cosine") < min(internalValues$sliderValues)
                 | edge_attr(internalValues$activelayout$graph,"cosine") > max(internalValues$sliderValues))
       
       ew[hidethese] <- 0
       elabs [hidethese] <- ""
      }
      
      if(length(internalValues$highlights) >0 ){
        
        vc[internalValues$highlights] <- "red"
        
      }
      
      plot(internalValues$activelayout$graph, 
           xlim = internalValues$xrange,
           ylim = internalValues$yrange,
           
           mark.groups = internalValues$markgroups,
           mark.expand = 1,
           vertex.size = scalingH,
           vertex.size2 = scalingV,
           vertex.frame.color = fc,
           vertex.color = vc,
           vertex.shape = "rectangle",
           
           #this alone does not improve plotting speed:
          # edge.lty = if(length(elabs) > 5000){0}else{1},
          
           edge.label= elabs,
           edge.label.family = "sans",
           edge.label.color = elabc,
           edge.label.font = elabf,
           edge.color = ec,
           edge.width = ew,
           vertex.label = vlabs,
           vertex.label.family = "sans",
           vertex.label.color = vlabc,
           vertex.label.font = vlabf,
           
           main = if(internalValues$overview){"Overview"}else{NULL},
           margin = 0,
           rescale = F,
           layout=internalValues$activelayout$layout)
      
      internalValues$recordedPlot <- recordPlot()
      
      
      
    }
  }, height = 900)
  
  output$ColorLegend <- renderPlot({
    if(internalValues$vlabccheck){
      
      if(!is.numeric(vertex_attr(internalValues$graph,input$vlabelcol)) 
         || length(unique(vertex_attr(internalValues$graph,input$vlabelcol))) <= 5){
        
      colfacs <- as.factor(vertex_attr(internalValues$graph,input$vlabelcol))
      cols <- Mseek.colors(n = length(levels(colfacs)), alpha = 1)
      
      
      legendplot("center",
                 legend = as.character(levels(colfacs)),
                 fill = cols,
                 col = "black", bty = "n", 
                 cex = 1, horiz = T)
      }else{
        
        colorRampLegend(vertex_attr(internalValues$activelayout$graph,input$vlabelcol), internalValues$colscale, input$vlabelcol)
        
      }
      
    }
    
  }, height = 85)
  
  
  
  
  observeEvent(input$Netw_click,{
    if (length(keys())>0 && keys() == 16) {
      internalValues$marker <- nearPoints(as.data.frame(internalValues$activelayout$layout),
                                          input$Netw_click,
                                          xvar = "x",
                                          yvar = "y",
                                          threshold = 100,
                                          maxpoints = 1)
      
      internalValues$marker$vertex <- which(internalValues$activelayout$layout[,1] == internalValues$marker$x
                                            & internalValues$activelayout$layout[,2] == internalValues$marker$y)
      
      #selecting a subgraph and setting up plot for it
      if (internalValues$overview) {
        subg <- findsubgraph(V(internalValues$activelayout$graph)$fixed__id[internalValues$marker$vertex], internalValues$layouts$subgraphs)
        
        #record which subgraph is selected
        internalValues$activelayout$subg <- subg
        
        
        internalValues$activelayout$graph <- internalValues$layouts$subgraphs[[subg]]
        internalValues$activelayout$layout <- norm_coords(as.matrix(internalValues$layouts$sublayouts[[subg]]))
        colnames(internalValues$activelayout$layout) <- c("x", "y")
        internalValues$hover <- NULL
        internalValues$marker <- NULL
        internalValues$markgroups <- NULL
        internalValues$overview <- F
        internalValues$xrange <- internalValues$maxxrange
        internalValues$yrange <- internalValues$maxyrange
      }
      
    }
    
    #replacement for doubleclick zoom-in or zoom-out: press Z and click.
    if (length(keys())>0 && keys() == 90) {
      
      if (!is.null(input$Netw_brush)) {
        
        internalValues$xrange <- c(input$Netw_brush$xmin, input$Netw_brush$xmax)
        internalValues$yrange <- c(input$Netw_brush$ymin, input$Netw_brush$ymax)
        
      } else {
        #switch back into overview mode on doubleclick in subgraph if full xy range is shown in current plot
        if(internalValues$xrange == internalValues$maxxrange
           && internalValues$yrange == internalValues$maxyrange
           && !internalValues$overview){
          internalValues$activelayout$graph <- disjoint_union(internalValues$layouts$subgraphs)#internalValues$graph
          
          #update large layout with new coords from changes in subgraphs
          internalValues$layouts$layout <- merge_coords(internalValues$layouts$subgraphs, internalValues$layouts$sublayouts)
          internalValues$activelayout$layout <- norm_coords(internalValues$layouts$layout)
          colnames(internalValues$activelayout$layout) <- c("x", "y")
          internalValues$overview <- T
          internalValues$hover <- NULL
          internalValues$marker <- NULL
          
        }
        internalValues$xrange <- internalValues$maxxrange
        internalValues$yrange <- internalValues$maxyrange
        
      }
    }
    
  })
  
  observeEvent(input$Netw_brush,{
    # if(is.null(input$Netw_brush)){
    # internalValues$hover <- 
    if (length(keys())>0 && keys() == 17) {     
      
      xs <- c(input$Netw_brush$xmin,input$Netw_brush$xmax)
      ys <- c(input$Netw_brush$ymin,input$Netw_brush$ymax)
      
      internalValues$hover$x<- xs[which.max(abs(xs-internalValues$hover$x))]
      internalValues$hover$y<- ys[which.max(abs(ys-internalValues$hover$y))]
      
      internalValues$activelayout$layout[internalValues$hover$vertex,1] <- internalValues$hover$x
      internalValues$activelayout$layout[internalValues$hover$vertex,2] <- internalValues$hover$y
      
      internalValues$layouts$sublayouts[[internalValues$activelayout$subg]] <- internalValues$activelayout$layout
      
    }
  })
  
  observeEvent(input$Netw_hover,{
    
    
    
    
    if(internalValues$hoverActive && is.null(input$Netw_brush) && !(length(keys())>0 && keys() == 17)){
      
      
      #print("h1")
      internalValues$hover <- nearPoints(as.data.frame(internalValues$activelayout$layout),
                                         input$Netw_hover,
                                         xvar = "x",
                                         yvar = "y",
                                         threshold = 100,
                                         maxpoints = 1)

      #print("h2")
      internalValues$hover$vertex <- which(internalValues$activelayout$layout[,1] == internalValues$hover$x
                                           & internalValues$activelayout$layout[,2] == internalValues$hover$y)
      
      #print("h3")
      if(internalValues$overview && !is.null(internalValues$hover)){
        #print("h4")
        internalValues$hover$subgraph <- findsubgraph(V(internalValues$activelayout$graph)$fixed__id[internalValues$hover$vertex], internalValues$layouts$subgraphs)
      }
      #print("h5")
      internalValues$markgroups <- if(internalValues$overview && !is.null(internalValues$hover)){
        #print("h6")
        which(V(internalValues$activelayout$graph)$subcl == V(internalValues$activelayout$graph)$subcl[internalValues$hover$vertex])
      }else{NULL}
      #print("h7")
      
      
    }
    # else{
    #   print(input$Netw_brush)
    #   }
    
  })
  
  observeEvent(input$Netw_dblclick, {
    internalValues$dblclick <- input$Netw_dblclick
    
    
    if (!is.null(input$Netw_brush)) {
      
      internalValues$xrange <- c(input$Netw_brush$xmin, input$Netw_brush$xmax)
      internalValues$yrange <- c(input$Netw_brush$ymin, input$Netw_brush$ymax)
      
    } else {
      #switch back into overview mode on doubleclick in subgraph if full xy range is shown in current plot
      if(internalValues$xrange == internalValues$maxxrange
         && internalValues$yrange == internalValues$maxyrange
         && !internalValues$overview){
        internalValues$activelayout$graph <- disjoint_union(internalValues$layouts$subgraphs)#internalValues$graph
        
        #update large layout with new coords from changes in subgraphs
        internalValues$layouts$layout <- merge_coords(internalValues$layouts$subgraphs, internalValues$layouts$sublayouts)
        internalValues$activelayout$layout <- norm_coords(internalValues$layouts$layout)
        colnames(internalValues$activelayout$layout) <- c("x", "y")
        internalValues$overview <- T
        internalValues$hover <- NULL
        internalValues$marker <- NULL
        
      }
      internalValues$xrange <- internalValues$maxxrange
      internalValues$yrange <- internalValues$maxyrange
      
    }})
  
  
  #Edge or Node table
  table1 <- callModule(TableModule,'nettab', tag = ns('nettab'), set = reactive({list(df =  internalValues$tables[[internalValues$activeTab]],
                                                                                      update = NULL,
                                                                                      layout = list(
                                                                                        perpage = 100,
                                                                                        height = 300,
                                                                                        readOnly = T,
                                                                                        contextMenu = F,
                                                                                        fixedColumnsLeft = 2,
                                                                                        format = list(col = c("RTStdErr"),
                                                                                                      format = "0.000"),
                                                                                        invertReadOnly = NULL
                                                                                      ))})
  )
  
  output$netAll <- renderUI({
    if(!is.null(reactives()$active) 
       && reactives()$active 
       && !is.null(internalValues$graph)
    ){
      fluidPage(
            
        htmlOutput(ns('controls')),
        fluidRow(
          
          plotOutput(ns("Netw"),
                     click = ns("Netw_click"),
                     hover = hoverOpts(id = ns("Netw_hover"),
                                       delay = 150),
                     dblclick = ns("Netw_dblclick"),
                     brush = brushOpts(
                       id = ns("Netw_brush"),
                       opacity = 0,
                       #direction = "x",
                       resetOnNew = TRUE),
                     height = "900px"#,
                     #width = "100%"
          )
          ),
        fluidRow(
          plotOutput(ns("ColorLegend"), height = "85px")
          ),
        fluidRow(
          htmlOutput(ns("netinfo"))
        )
          ,
        
        hr(),
        fluidRow(
          htmlOutput(ns("nettables"))
        ),
        fluidRow(
          TableModuleUI(ns('nettab'))
        )
      )
    }
    
  })
  
  
  
  return(internalValues)
}

#' NetworkModuleUI
#' 
#' 
#' UI module for interactive spectrum view
#' 
#' @param id id to be used in ns()
#' 
#' @export
NetworkModuleUI <- function(id){
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns("netAll"))
  )
  
}