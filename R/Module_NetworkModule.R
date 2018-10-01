#' NetworkModule
#' 
#' 
#' server module for interactive mass spectrum view
#' 
#' @param input 
#' @param output 
#' @param session 
#' @param tag id to be used in ns()
#' @param set Import data from the shiny session
#' 
#' @export 
NetworkModule <- function(input,output, session, tag, set = list(net = list(xrange = NULL,
                                                                            yrange = NULL,
                                                                            maxxrange = NULL,
                                                                            maxyrange = NULL,
                                                                            sel = NULL,
                                                                            mz = NULL,
                                                                            data = NULL,
                                                                            tables = list(nodes = NULL,
                                                                                          edges = NULL)),
                                                                 layout = list(lw = 1,
                                                                               cex = 1,
                                                                               controls = F,
                                                                               ppm = 5,
                                                                               active = T)),
                          keys
){
  
  ns <- NS(tag)
  selections <- reactiveValues(plots = list(net = list(graph = NULL,
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
                                                       data = NULL,
                                                       elabels = NULL,
                                                       elabelcheck = F,
                                                       vlabels = NULL,
                                                       vlabelcheck = F,
                                                       vlabelcol = NULL,
                                                       vlabccheck = F,
                                                       vlabcolfactors = NULL,
                                                       overview = F,
                                                       markgroups = NULL,
                                                       
                                                       tables = NULL,
                                                       activeTab = ""
                                                       
  ),
  set = NULL #copy of set() to check if set() has changed
  )
  )
  
  # observeEvent(set(),{sc()})
  #sc <- eventReactive({
  observeEvent(set(),{
    # print(set())
    
    if(set()$layout$active && !is.null(set()$net$data) && !identical(selections$plots$set,set()$net )){
      #print(set()$net$data)
      
      selections$plots$net$graph <- set()$net$data
      selections$plots$net$tables <- set()$net$tables
      
      
      #generate the initial layout
      res <- layout_components_qgraph(selections$plots$net$graph, qgraph.layout.fruchtermanreingold)
      
      selections$plots$net$layouts <- res
      
      selections$plots$net$activelayout$graph <- selections$plots$net$graph
      selections$plots$net$activelayout$layout <- norm_coords(selections$plots$net$layouts$layout)
      colnames(selections$plots$net$activelayout$layout) <- c("x", "y")
      
      #if there are subgraphs, start in overview mode
      if(length(selections$plots$net$layouts$subgraphs) > 1){selections$plots$net$overview <- T}
      
      #set the maximum x axis range to cover the spectrum data
      selections$plots$net$maxxrange <- c(-1,1)#range(selections$plots$net$activelayout$layout[,1])
      selections$plots$net$maxyrange <- c(-1,1)#range(selections$plots$net$activelayout$layout[,2])
      
      selections$plots$net$xrange <- c(-1,1)#range(selections$plots$net$activelayout$layout[,1])
      selections$plots$net$yrange <- c(-1,1)#range(selections$plots$net$activelayout$layout[,2])
      
      
      selections$plots$set <- set()$net
      return(res)
      
    }
  })
  
  output$netinfo <- renderUI({ 
    if(!is.null(set()$layout$active) && set()$layout$active && !is.null(set()$net$data)){
      if(selections$plots$net$overview && !is.null(selections$plots$net$hover$subgraph)){
        p("Subgraph #", selections$plots$net$hover$subgraph, "with ",
          strong(vcount(selections$plots$net$layouts$subgraphs[[selections$plots$net$hover$subgraph]])),
          "nodes and",
          strong(ecount(selections$plots$net$layouts$subgraphs[[selections$plots$net$hover$subgraph]])),
          "edges.")
        
      }
      else{
        p(paste0(if(!is.null(selections$plots$net$marker$vertex)){
          paste0("Marker on ", input$vlabelsel," ", vertex_attr(selections$plots$net$activelayout$graph,input$vlabelsel, selections$plots$net$marker$vertex))
        }
        else{""},
        if(!is.null(selections$plots$net$hover$vertex)){
          paste0(" Cursor on ",input$vlabelsel," ", vertex_attr(selections$plots$net$activelayout$graph,input$vlabelsel, selections$plots$net$hover$vertex))
        }else{""}
        ))
      }
    }
    
    
  })
  
  output$controls <- renderUI({
    if(!is.null(set()$layout$active) && set()$layout$active && !is.null(selections$plots$net$activelayout$graph)){
      fluidRow(
        column(2,
               checkboxInput(ns('vlabelcheck'), "Show all node labels", value = selections$plots$net$vlabelcheck),
               selectizeInput(ns("vlabelsel"), "Node Labels",
                              choices = names(vertex_attr(selections$plots$net$activelayout$graph)),
                              selected = selections$plots$net$vlabels)
        ),
        
        column(2,
               checkboxInput(ns('elabelcheck'), "Show all edge labels", value = selections$plots$net$elabelcheck),
               selectizeInput(ns("elabelsel"), "Edge Labels",
                              choices = names(edge_attr(selections$plots$net$activelayout$graph)),
                              selected = selections$plots$net$elabels)
        ),
        column(2,
               checkboxInput(ns('vlabccheck'), "Color-code nodes", value = selections$plots$net$vlabccheck),
               selectizeInput(ns("vlabelcol"), "Node Colors",
                              choices = c(NULL, names(vertex_attr(selections$plots$net$activelayout$graph))),
                              selected = selections$plots$net$vlabelc)
        )
      )
    }
  })
  
  observeEvent(input$vlabelcheck,{selections$plots$net$vlabelcheck <- input$vlabelcheck})
  observeEvent(input$vlabccheck,{
    selections$plots$net$vlabccheck <- input$vlabccheck
    if(input$vlabccheck){
      selections$plots$net$vlabcolfactors <- as.factor(vertex_attr(selections$plots$net$graph,input$vlabelcol))
    }
  })
  
  observeEvent(input$elabelcheck,{selections$plots$net$elabelcheck <- input$elabelcheck})
  
  observeEvent(input$vlabelsel,{selections$plots$net$vlabels <- input$vlabelsel})
  observeEvent(input$vlabelcol,{selections$plots$net$vlabelc <- input$vlabelcol
  if(input$vlabccheck){
    selections$plots$net$vlabcolfactors <- as.factor(vertex_attr(selections$plots$net$graph,input$vlabelcol))
  }
  })
  
  observeEvent(input$elabelsel,{selections$plots$net$elabels <- input$elabelsel})
  
  output$nettables <- renderUI({
    if(!is.null(set()$layout$active) && set()$layout$active && !is.null(selections$plots$net$tables)){
      
      selectizeInput(ns('tabs'), "Show network table", choices = c("",names(selections$plots$net$tables)), selected = selections$plots$net$activeTab)
      
    }
  })
  observeEvent(input$tabs,{
    selections$plots$net$activeTab <- input$tabs
    
  })
  
  
  
  output$Netw <- renderPlot({
    #print(selections$plots$net$activelayout$graph)
    if(set()$layout$active && !is.null(selections$plots$net$activelayout$graph)){
      #sc()
      # print("plotting")
      # make color vectors here (so that graph is not changed! -> infinite loop)
      
      # set all frame color to black
      fc <- rep("black", times = vcount(selections$plots$net$activelayout$graph))
      #set all edge colors to black
      ec <- rep("grey50", times = ecount(selections$plots$net$activelayout$graph))
      
      #set all vertex colors
      if(selections$plots$net$vlabccheck){
        
        if(selections$plots$net$overview){
          vc <- mosaic.colors(n = length(levels(selections$plots$net$vlabcolfactors)), alpha = 1)[selections$plots$net$vlabcolfactors]
          
          #make sure subgraphs use same color scheme as overview:
        }else{
          
          colassign <- as.character(vertex_attr(selections$plots$net$activelayout$graph,input$vlabelcol))
          for (l in seq(length(levels(selections$plots$net$vlabcolfactors)))){
            sel <- which(colassign == levels(selections$plots$net$vlabcolfactors)[l])
            if(length(sel) >0){
              colassign[sel] <- l
            }
          }
          vc <- mosaic.colors(n = length(levels(selections$plots$net$vlabcolfactors)), alpha = 1)[as.integer(colassign)]
        }
        
        
      }else{
        vc <- rep("olivedrab1", times = vcount(selections$plots$net$activelayout$graph))
      }
      
      #edge labels
      elabs <- if(selections$plots$net$elabelcheck){
        if(is.numeric(edge_attr(selections$plots$net$activelayout$graph,input$elabelsel))){
          round(edge_attr(selections$plots$net$activelayout$graph,input$elabelsel),2)}
        else{edge_attr(selections$plots$net$activelayout$graph,input$elabelsel)}
      }else{rep(NA, times = vcount(selections$plots$net$activelayout$graph))}
      
      #edge label color and font
      elabc <- rep("blue", times = vcount(selections$plots$net$activelayout$graph))
      elabf <- rep(1, times = vcount(selections$plots$net$activelayout$graph))
      
      #vertex labels
      vlabs <- if(selections$plots$net$vlabelcheck){
        as.character(vertex_attr(selections$plots$net$activelayout$graph,input$vlabelsel))
      }else{rep(NA, times = vcount(selections$plots$net$activelayout$graph))}
      
      #vertex label color and font
      vlabc <- rep("black", times = vcount(selections$plots$net$activelayout$graph))
      vlabf <- rep( 2, times = vcount(selections$plots$net$activelayout$graph))
      
      #recolor based on marking
      if(!is.null(selections$plots$net$marker) && !selections$plots$net$overview){
        sel <- selections$plots$net$marker$vertex
        neigh <- neighbors(selections$plots$net$activelayout$graph, V(selections$plots$net$activelayout$graph)[sel] ) 
        edg <- incident(selections$plots$net$activelayout$graph,V(selections$plots$net$activelayout$graph)[sel])
        
        fc[sel] <- "red"
        fc[neigh] <- "orange"
        vc[sel] <- "indianred3"
        vc[neigh] <- "indianred1"  
        ec[edg] <- "red"
        elabs[edg] <- if(is.numeric(edge_attr(selections$plots$net$activelayout$graph,input$elabelsel))){
          round(edge_attr(selections$plots$net$activelayout$graph,input$elabelsel)[edg],2)}
        else{edge_attr(selections$plots$net$activelayout$graph,input$elabelsel)[edg]}
        elabc[edg] <- "darkorange2"
        elabf[edg] <- 2
        
        
        vlabs[c(sel,neigh)] <- as.character(vertex_attr(selections$plots$net$activelayout$graph,input$vlabelsel)[c(sel,neigh)])
        vlabc[c(sel,neigh)] <- "black"
        vlabf[sel] <- 4
        
      }
      
      #recolor based on hovering (overrides marking colors!)
      if(!is.null(selections$plots$net$hover) && !selections$plots$net$overview){
        
        #which vortex is hovered over
        sel <- selections$plots$net$hover$vertex
        neigh <- neighbors(selections$plots$net$activelayout$graph, V(selections$plots$net$activelayout$graph)[sel] )  
        edg <- incident(selections$plots$net$activelayout$graph,V(selections$plots$net$activelayout$graph)[sel])
        #recolor 
        fc[sel] <- "cyan"
        fc[neigh] <- "lightcyan3"
        ec[edg] <- "cyan"
        elabs[edg] <- if(is.numeric(edge_attr(selections$plots$net$activelayout$graph,input$elabelsel))){
          round(edge_attr(selections$plots$net$activelayout$graph,input$elabelsel)[edg],2)}
        else{edge_attr(selections$plots$net$activelayout$graph,input$elabelsel)[edg]}
        elabc[edg] <- "blue"
        elabf[edg] <- 2
        
        vlabs[c(sel,neigh)] <- as.character(vertex_attr(selections$plots$net$activelayout$graph,input$vlabelsel)[c(sel,neigh)])
        vlabc[c(sel,neigh)] <- "black"
        vlabf[sel] <- 4
      }
      
      #scale node width based on label width
      if(!selections$plots$net$overview){
        scalingH <- max(420*max(strwidth(vlabs, units = "figure")),
                        420*strwidth(as.character(vertex_attr(selections$plots$net$activelayout$graph,input$vlabelsel)[1]), units = "figure"))
        scalingV <- 550*strheight(vertex_attr(selections$plots$net$activelayout$graph,input$vlabelsel)[1], units = "figure")
        
      }else{
        #in overview mode, just show squares with pleasant aspect ratio
        scalingH <- 70/sqrt(vcount(selections$plots$net$activelayout$graph))
        scalingV <- (70/sqrt(vcount(selections$plots$net$activelayout$graph)))/1.414
        
      }
      
      plot(selections$plots$net$activelayout$graph, 
           xlim = selections$plots$net$xrange,
           ylim = selections$plots$net$yrange,
           
           mark.groups = selections$plots$net$markgroups,
           mark.expand = 1,
           vertex.size = scalingH,
           vertex.size2 = scalingV,
           vertex.frame.color = fc,
           vertex.color = vc,
           vertex.shape = "rectangle",
           
           edge.label= elabs,
           edge.label.family = "sans",
           edge.label.color = elabc,
           edge.label.font = elabf,
           edge.color = ec,
           edge.width = 2,
           vertex.label = vlabs,
           vertex.label.family = "sans",
           vertex.label.color = vlabc,
           vertex.label.font = vlabf,
           
           main = if(selections$plots$net$overview){"Overview"}else{NULL},
           margin = 0,
           rescale = F,
           layout=selections$plots$net$activelayout$layout)
      
      
      
      #print(selections$plots$net$hover)
      # if(!is.null(selections$plots$net$hover)){
      
      #  points(selections$plots$net$hover[,1],
      #        selections$plots$net$hover[,2],
      #       bty = "n", type = "p", lwd = 5, col = "#00FF0080")
      
      #}
      
      #  if(!is.null(selections$plots$net$marker)){
      
      #   points(selections$plots$net$marker[,1],
      #         selections$plots$net$marker[,2],
      #        bty = "n", type = "p", lwd = 5, col = "#FFAB3680")
      
      #}
      
      
      
    }
  }, height = 900)
  
  output$ColorLegend <- renderPlot({
    if(selections$plots$net$vlabccheck){
      colfacs <- as.factor(vertex_attr(selections$plots$net$graph,input$vlabelcol))
      cols <- mosaic.colors(n = length(levels(colfacs)), alpha = 1)
      
      
      legendplot("center",
                 legend = as.character(levels(colfacs)),
                 fill = cols,
                 col = "black", bty = "n", 
                 cex = 1, horiz = T)
    }
    
  }, height = 30)
  
  
  
  
  observeEvent(input$Netw_click,{
    if (length(keys())>0 && keys() == 16) {
      selections$plots$net$marker <- nearPoints(as.data.frame(selections$plots$net$activelayout$layout),
                                                input$Netw_click,
                                                xvar = "x",
                                                yvar = "y",
                                                threshold = 100,
                                                maxpoints = 1)
      
      selections$plots$net$marker$vertex <- which(selections$plots$net$activelayout$layout[,1] == selections$plots$net$hover$x
                                                  & selections$plots$net$activelayout$layout[,2] == selections$plots$net$hover$y)
      
      #selecting a subgraph and setting up plot for it
      if (selections$plots$net$overview) {
        subg <- findsubgraph(V(selections$plots$net$activelayout$graph)$id[selections$plots$net$marker$vertex], selections$plots$net$layouts$subgraphs)
        
        #record which subgraph is selected
        selections$plots$net$activelayout$subg <- subg
        
        
        selections$plots$net$activelayout$graph <- selections$plots$net$layouts$subgraphs[[subg]]
        selections$plots$net$activelayout$layout <- norm_coords(as.matrix(selections$plots$net$layouts$sublayouts[[subg]]))
        colnames(selections$plots$net$activelayout$layout) <- c("x", "y")
        selections$plots$net$hover <- NULL
        selections$plots$net$marker <- NULL
        selections$plots$net$markgroups <- NULL
        selections$plots$net$overview <- F
        selections$plots$net$xrange <- selections$plots$net$maxxrange
        selections$plots$net$yrange <- selections$plots$net$maxyrange
      }
      
    }
    
    #replacement for doubleclick zoom-in or zoom-out: press Z and click.
    if (length(keys())>0 && keys() == 90) {
      
      if (!is.null(input$Netw_brush)) {
        
        selections$plots$net$xrange <- c(input$Netw_brush$xmin, input$Netw_brush$xmax)
        selections$plots$net$yrange <- c(input$Netw_brush$ymin, input$Netw_brush$ymax)
        
      } else {
        #switch back into overview mode on doubleclick in subgraph if full xy range is shown in current plot
        if(selections$plots$net$xrange == selections$plots$net$maxxrange
           && selections$plots$net$yrange == selections$plots$net$maxyrange
           && !selections$plots$net$overview){
          selections$plots$net$activelayout$graph <- disjoint_union(selections$plots$net$layouts$subgraphs)#selections$plots$net$graph
          
          #update large layout with new coords from changes in subgraphs
          selections$plots$net$layouts$layout <- merge_coords(selections$plots$net$layouts$subgraphs, selections$plots$net$layouts$sublayouts)
          selections$plots$net$activelayout$layout <- norm_coords(selections$plots$net$layouts$layout)
          colnames(selections$plots$net$activelayout$layout) <- c("x", "y")
          selections$plots$net$overview <- T
          selections$plots$net$hover <- NULL
          selections$plots$net$marker <- NULL
          
        }
        selections$plots$net$xrange <- selections$plots$net$maxxrange
        selections$plots$net$yrange <- selections$plots$net$maxyrange
        
      }
    }
    
  })
  
  observeEvent(input$Netw_brush,{
    # if(is.null(input$Netw_brush)){
    # selections$plots$net$hover <- 
    if (length(keys())>0 && keys() == 17) {     
      
      xs <- c(input$Netw_brush$xmin,input$Netw_brush$xmax)
      ys <- c(input$Netw_brush$ymin,input$Netw_brush$ymax)
      
      selections$plots$net$hover$x<- xs[which.max(abs(xs-selections$plots$net$hover$x))]
      selections$plots$net$hover$y<- ys[which.max(abs(ys-selections$plots$net$hover$y))]
      
      selections$plots$net$activelayout$layout[selections$plots$net$hover$vertex,1] <- selections$plots$net$hover$x
      selections$plots$net$activelayout$layout[selections$plots$net$hover$vertex,2] <- selections$plots$net$hover$y
      
      selections$plots$net$layouts$sublayouts[[selections$plots$net$activelayout$subg]] <- selections$plots$net$activelayout$layout
      
    }
  })
  
  observeEvent(input$Netw_hover,{
    
    
    
    
    if(is.null(input$Netw_brush) && !(length(keys())>0 && keys() == 17)){
      
      
      
      selections$plots$net$hover <- nearPoints(as.data.frame(selections$plots$net$activelayout$layout),
                                               input$Netw_hover,
                                               xvar = "x",
                                               yvar = "y",
                                               threshold = 100,
                                               maxpoints = 1)
      selections$plots$net$hover$vertex <- which(selections$plots$net$activelayout$layout[,1] == selections$plots$net$hover$x
                                                 & selections$plots$net$activelayout$layout[,2] == selections$plots$net$hover$y)
      
      if(selections$plots$net$overview && !is.null(selections$plots$net$hover)){
        
        selections$plots$net$hover$subgraph <- findsubgraph(V(selections$plots$net$activelayout$graph)$id[selections$plots$net$hover$vertex], selections$plots$net$layouts$subgraphs)
      }
      
      selections$plots$net$markgroups <- if(selections$plots$net$overview && !is.null(selections$plots$net$hover)){
        which(V(selections$plots$net$activelayout$graph)$subcl == V(selections$plots$net$activelayout$graph)$subcl[selections$plots$net$hover$vertex])
      }else{NULL}
      
      
      
    }
    # else{
    #   print(input$Netw_brush)
    #   }
    
  })
  
  observeEvent(input$Netw_dblclick, {
    selections$plots$net$dblclick <- input$Netw_dblclick
    
    
    if (!is.null(input$Netw_brush)) {
      
      selections$plots$net$xrange <- c(input$Netw_brush$xmin, input$Netw_brush$xmax)
      selections$plots$net$yrange <- c(input$Netw_brush$ymin, input$Netw_brush$ymax)
      
    } else {
      #switch back into overview mode on doubleclick in subgraph if full xy range is shown in current plot
      if(selections$plots$net$xrange == selections$plots$net$maxxrange
         && selections$plots$net$yrange == selections$plots$net$maxyrange
         && !selections$plots$net$overview){
        selections$plots$net$activelayout$graph <- disjoint_union(selections$plots$net$layouts$subgraphs)#selections$plots$net$graph
        
        #update large layout with new coords from changes in subgraphs
        selections$plots$net$layouts$layout <- merge_coords(selections$plots$net$layouts$subgraphs, selections$plots$net$layouts$sublayouts)
        selections$plots$net$activelayout$layout <- norm_coords(selections$plots$net$layouts$layout)
        colnames(selections$plots$net$activelayout$layout) <- c("x", "y")
        selections$plots$net$overview <- T
        selections$plots$net$hover <- NULL
        selections$plots$net$marker <- NULL
        
      }
      selections$plots$net$xrange <- selections$plots$net$maxxrange
      selections$plots$net$yrange <- selections$plots$net$maxyrange
      
    }})
  
  
  #Edge or Node table
  table1 <- callModule(TableModule,'nettab', tag = ns('nettab'), set = reactive({list(df =  selections$plots$net$tables[[selections$plots$net$activeTab]],
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
    if(!is.null(set()$layout$active) && set()$layout$active && !is.null(set()$net$data)){
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
                     height = "900px",
                     width = "900px"
          )),
        fluidRow(
          plotOutput(ns("ColorLegend"), height = "30px")),
        fluidRow(
          htmlOutput(ns("netinfo"))
        ),
        fluidRow(
          htmlOutput(ns("nettables"))
        ),
        fluidRow(
          TableModuleUI(ns('nettab'))
        )
      )
    }
    
  })
  
  
  
  return(
    reactive({
      selections$plots
    })
  )
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
  
  htmlOutput(ns("netAll"))
  
  
}