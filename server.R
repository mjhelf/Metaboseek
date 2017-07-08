function(input, output, session) {
    options(shiny.maxRequestSize=1024*1024^2)
    
    #runcodeServer()
    
    allids <- c('tab-6812-1', 'tab-7850-1', 'file1', 'file1_progress', 'header',
      'sep', 'quote', 'ldtbl', 'preview', 'rmcols', 'rmrows', 'intcols',
      'anagrouping',
      'confgroups', 'savegroups', 'loadgroups', 'loadgroups_progress',
      'anatbl', 'pv', '._bookmark_', 'tab-7850-2', 
      #'rfileload', 'rfileload_progress',
      'rnamelvl', 'rawgrouping', 'confrgroups', 'savergroups', 'loadrgroups', 'loadrgroups_progress',
      'tab-6812-2', 'selgr', 'subsel1', 'subsel2', 'subsel3', 'subsel4', 'subsel5', 'fname', 'pdfButton',
      'tbButton', 'mzquery', 'mzspace', 'mzcharge', 'mzppm', 'mzButton', 'hot1', 'rtwd', 'tab-2316-1',
      'tab-2316-2', 'plot1', 'tab-2316-3', 'goButton', 'plot2', 'tab-2316-4', 'EICfiles', 'chromly',
      'specinfo', 'spec', 'spec_click', 'spec_dblclick', 'spec_hover', 'TRUE', 'spec_brush', 'tab-2316-5',
      'fragtab2', 'sFrags', 'molplot', 'specinfo2', 'spec2', 'spec2_click', 'spec2_dblclick', 'spec2_hover',
      'TRUE', 'spec2_brush', 'parenttab', 'fButton', 'fButton2', 'filtable2', 'hmtable', 'txt')
    
    exids <- c("loadgroups", "file1","ldtbl"
               ,"confgroups")
    
    setBookmarkExclude(allids)
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
        state$values$tablestuff$tablecut <- tablestuff$tablecut
        state$values$tablestuff$grprops <- tablestuff$grprops
        state$values$tablestuff$fprop <- tablestuff$fprop
        state$values$tablestuff$gprop <- tablestuff$gprop
        state$values$tablestuff$grpropl <- tablestuff$grpropl
        state$values$tablestuff$fragments <- tablestuff$fragments
        
        state$values$colstuff$colrange <- colstuff$colrange
        print(colstuff$anagroupraw)
        state$values$colstuff$anagroupraw <- colstuff$anagroupraw
        
        ##Saving rawdata
        state$values$rawstuff$rfilesin <- rawstuff$rfilesin
        state$values$rawstuff$rawgroupraw <- rawstuff$rawgroupraw
        #state$values$rawstuff$rawgroups <- rawstuff$rawgroups
        #state$values$rawstuff$rawdata <- rawstuff$rawdata
        state$values$rawstuff$rtw <- rawstuff$rtw
        
    })
    
    # Read values from state$values when we restore
    onRestore(function(state) {
        ##Restoring Tables
        
        tablestuff$tablecut <- state$values$tablestuff$tablecut
        tablestuff$grprops <- state$values$tablestuff$grprops
        tablestuff$fprop <- state$values$tablestuff$fprop
        tablestuff$gprop <- state$values$tablestuff$gprop
        tablestuff$grpropl <- state$values$tablestuff$grpropl
        tablestuff$fragments <- state$values$tablestuff$fragments
        
       colstuff$colrange  <- state$values$colstuff$colrange

       
       ###Restoring Rawdata
       
       #rawstuff$rfilesin <- state$values$rawstuff$rfilesin
       #rawstuff$rawgroupraw <- state$values$rawstuff$rawgroupraw
      # rawstuff$rawgroups <- state$values$rawstuff$rawgroups
       #rawstuff$rawdata <- state$values$rawstuff$rawdata
       rawstuff$rtw <- state$values$rawstuff$rtw
       
    })
    onRestored(function(state){
        rawstuff$rawgroupraw <- state$values$rawstuff$rawgroupraw
        
        colstuff$anagroupraw  <- state$values$colstuff$anagroupraw
        
        ## Make list object of grouped column names                                        
        colme <- list()
        for (l in levels(colstuff$anagroupraw$Group)){
            colme[[l]] <- as.character(colstuff$anagroupraw$Column[which(colstuff$anagroupraw$Group==l)])
        }
        colstuff$anagroupnames <- colme
        ### Get column numbers from column names
        colnu <- integer(0)
        for (i in colstuff$anagroupraw$Column){
            colnu<- c(colnu,which(colnames(tablestuff$tablecut) == i))
        }
        ## Make list object of grouped column numbers
        colme <- list()
        for (l in levels(colstuff$anagroupraw$Group)){
            colme[[l]] <- as.integer(colnu[which(colstuff$anagroupraw$Group==l)])
        }
        colstuff$anagroupnums <- colme
        
        
        if(!is.null(rawstuff$rawgroupraw)){
        ## Make list object of grouped file names                                        
        colme <- list()
        for (l in unique(rawstuff$rawgroupraw$Group)){
            colme[[l]] <- as.character(paste0(dirname(input$rfileload$datapath),rawstuff$rawgroupraw$File[which(rawstuff$rawgroupraw$Group==l)]))
        }
        #get groups in alphabetic order
        rawstuff$rawgroups <- colme[order(names(colme))]
        #make xcmsRaws (time consuming)
        rawstuff$rawdata <- EICrawP(rawstuff$rawgroups, workers = 10)
        }
    })
    
    
 #   observeEvent(input$bookmark2, {
  #      session$doBookmark()
   # })
    
    ##############LOAD TABLE MODULE#############
    ####Tablecollection reactive Values
    
    tablestuff <- reactiveValues(tablecut = NULL,#NULL,pl_pos,#
                                 grprops = NULL,
                                 fprop = NULL,
                                 gprop = NULL,
                                 grpropl = list(),
                                 fragments = NULL #character(nrow(pl_pos))#
                                 )
    
    ###Load the Table uploaded through input$file1 Button using options from the UI
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    tablein <- eventReactive(input$ldtbl,{read.csv(input$file1$datapath, header=input$header, sep=input$sep, 
                                                   quote=input$quote, stringsAsFactors = F)})
    
    #####Prepare the loaded table for preview and further use (cutting off rows and columns)
    observeEvent(c(tablein(), input$rmcols, input$rmrows),{tablestuff$tablecut <- tablein()[input$rmrows:nrow(tablein()),input$rmcols:ncol(tablein())]
    if(length(grep("_stats",colnames(tablestuff$tablecut)))>0){
        if(length(strsplit(as.character(tablestuff$tablecut[1,grep("_stats",colnames(tablestuff$tablecut))[1]])," ")[[1]])==8){
            tablestuff$grprops <- c("mean_fold_over_other_groups", "sdev", "pval", "pval_adjusted", "samples_10fold_over_ctrl", "samples_10fold_over_other_groups", "mean_intensity", "max_intensity")
        }else{
            tablestuff$grprops <- c("mean_fold_over_other_groups", "sdev", "samples_10fold_over_ctrl", "samples_10fold_over_other_groups", "mean_intensity", "max_intensity")
        }
    }
    if(length(grep("Fragments",colnames(tablestuff$tablecut)))>0){
        tablestuff$fragments <- as.character(tablestuff$tablecut[,"Fragments"])
    }else{
        tablestuff$fragments <- character(nrow(tablestuff$tablecut))
    }
    
    })
    
    
    
    ####Render the preview table with first 10 rows of tablestuff$tablecut
    output$preview <- renderRHandsontable({if(!is.null(tablestuff$tablecut)){
        rhandsontable(tablestuff$tablecut[1:10,], selectCallback = T)%>%
    hot_cols(renderer = "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.TextCell.renderer.apply(this, arguments);
              td.style.color = 'black';
       }")
        #return td;
        }
    })
    
    ####Currently selected columns in preview rhandsontable
    selcols <- reactive({ # sp<- strsplit("R1C1:R10C10",":")
        C1 <- as.numeric(input$preview_select$select$c)#substr(sp[[1]][1],gregexpr("C",sp[[1]][1])[[1]][[1]]+1,nchar(sp[[1]][1])))
        C2 <- as.numeric(input$preview_select$select$c2)#substr(sp[[1]][2],gregexpr("C",sp[[1]][2])[[1]][[1]]+1,nchar(sp[[1]][2])))
        rng <- C1:C2
        return(rng)
    })
    
    
    ###Initialize analysis grouping parameters                      
    colstuff <- reactiveValues(colrange = NULL, #columns in tablestuff$tablecut containing intensity values of interest
                               anagroupraw = NULL, #columnnames in tablestuff$tablecut containing intensity values of interest with their respective analysis group (dataframe)
                               anagroupnames = NULL, #columnnames in tablestuff$tablecut containing intensity values of interest grouped (list)
                               anagroupnums = NULL) #columnnumbers in tablestuff$tablecut containing intensity values of interest grouped (list)
    
    ###If tablecut (input table with row/column adjustments) is changed, load all columns with "_XIC" (default)
    observeEvent(c(tablein(),tablestuff$tablecut),{if(!is.null(tablestuff$tablecut)){
        if(length(grep("_XIC",colnames(tablestuff$tablecut)))==0){
            colstuff$colrange <- 1
        }else{
            colstuff$colrange <- grep("_XIC",colnames(tablestuff$tablecut))}
    } })
    ###Override default column range with selected columns when pressing Button intcols
    observeEvent(input$intcols,{colstuff$colrange <- selcols()})
    
    ##### Make the default grouping table based on groupnames table and current colstuff$colrange
    observeEvent(colstuff$colrange,{if(!is.null(tablestuff$tablecut)){
        colstuff$anagroupraw <- data.frame(Column=colnames(tablestuff$tablecut)[colstuff$colrange],
                                           Group = rep("G1",(length(colstuff$colrange))),
                                           stringsAsFactors = F)
    }})
    ##### Render the current grouping table
    output$anagrouping <- renderRHandsontable({
        rhandsontable(colstuff$anagroupraw)
    })
    
    ######## Download current grouping table as shown
    output$savegroups <- downloadHandler(filename= function(){paste("AnalysisGrouping.tsv")}, 
                                         content = function(file){write.table(hot_to_r(input$anagrouping)
                                                                              #colstuff$anagroupraw
                                                                              , file, sep = "\t", quote = F,
                                                                              row.names = F)},
                                         contentType = "text/tab-separated-values")
    
   # onRestored(function(state){
    #### Load grouping table from file
    observeEvent(input$loadgroups$datapath,{colstuff$anagroupraw <- read.table(input$loadgroups$datapath, header=T, sep='\t')})
    
    ### When the Groups are confirmed, save the current view in colstuff
    ### And also generate corresponding list objects
    
    observeEvent(input$confgroups,{colstuff$anagroupraw <- data.frame(Column = as.character(hot_to_r(input$anagrouping)$Column),
                                                                      Group = as.character(hot_to_r(input$anagrouping)$Group)
    )
    ## Make list object of grouped column names                                        
    colme <- list()
    for (l in levels(colstuff$anagroupraw$Group)){
        colme[[l]] <- as.character(colstuff$anagroupraw$Column[which(colstuff$anagroupraw$Group==l)])
    }
    colstuff$anagroupnames <- colme
    ### Get column numbers from column names
    colnu <- integer(0)
    for (i in colstuff$anagroupraw$Column){
        colnu<- c(colnu,which(colnames(tablestuff$tablecut) == i))
    }
    ## Make list object of grouped column numbers
    colme <- list()
    for (l in levels(colstuff$anagroupraw$Group)){
        colme[[l]] <- as.integer(colnu[which(colstuff$anagroupraw$Group==l)])
    }
    colstuff$anagroupnums <- colme
    })
   
    
    
    ######Run Analysis of Table#####
    observeEvent(input$anatbl,{temp <- MSTana(pl=tablestuff$tablecut,
                                              glist=colstuff$anagroupnums,
                                              ctrl = colstuff$anagroupnums[1],
                                              pval = input$pv,
                                              normalize = T)
    
    tablestuff$tablecut <- temp$data
    tablestuff$grprops <- temp$groupsplitnames
    
    })
    
    ##############LOAD RAW FILES MODULE#############
    ###Load the mzXML files
    
    #### Load grouping table from file
    observeEvent(input$rfileload$datapath,{exfolder = file.path(dirname(input$rfileload$datapath), gsub("\\.[^.]*$","",input$rfileload$name))
    print(input$rfileload$datapath)
    print(exfolder)
    unzip(input$rfileload$datapath, exdir = exfolder )
        rawstuff$rfilesin <- list.files(exfolder, pattern=".mzXML", recursive = TRUE, full.names=T)
                                           
                                           })
    
    
    #observeEvent(input$rfileload,{rawstuff$rfilesin <- list.files(gsub("\\\\","/", input$rawpath), pattern=".mzXML", recursive = TRUE, full.names=T)})
    
    ###Initialize analysis grouping parameters                      
    rawstuff <- reactiveValues(rfilesin = NULL, #List of rawfile paths (unsorted)
                               rawgroupraw = NULL, #rawfile paths and groups (dataframe)
                               rawgroups = NULL,
                               rawdata = NULL,
                               rtw = 30 #rawfs
    ) 
    
    observeEvent(c(rawstuff$rawdata,tablestuff$tablecut),
                 {if(!is.null(rawstuff$rawdata) & !is.null(tablestuff$tablecut))
                     {if(is.null(tablestuff$tablecut$rt))
                         {rtval <- mean(c(max(sapply(sapply(unlist(rawstuff$rawdata),slot,"scantime"),max)),min(sapply(sapply(unlist(rawstuff$rawdata),slot,"scantime"),min))))
                          tablestuff$tablecut$rt <- rtval
                          rawstuff$rtw <- rtval
                         
                     }}})
    
    
    ##### Make the default rgrouping table based on groupnames table and currently selected folder
    observeEvent(c(rawstuff$rfilesin,input$rnamelvl),{if (!is.null(rawstuff$rfilesin)) {File=gsub(dirname(input$rfileload$datapath),"",rawstuff$rfilesin, ignore.case = T)
                                                                                        Group = if(input$rnamelvl ==1){as.character(unname(sapply(sapply(File,strsplit,split = "/"),tail,input$rnamelvl)))}else{
                                                                                        as.character(unname(apply(sapply(sapply(File,strsplit,split = "/"),tail,input$rnamelvl),2,"[",1)))}
                                                                                        rawstuff$rawgroupraw <- data.frame(File, Group, stringsAsFactors = F)
                                                                                                          }})
    ##### Render the current rgrouping table
    output$rawgrouping <- renderRHandsontable({
        rhandsontable(rawstuff$rawgroupraw)
    })
    
    ######## Download current grouping table as shown
    output$savergroups <- downloadHandler(filename= function(){paste("RawGrouping.tsv")}, 
                                          content = function(file){write.table(hot_to_r(input$rawgrouping)
                                                                               #colstuff$anagroupraw
                                                                               , file, sep = "\t",quote = F,
                                                                               row.names = F)},
                                          contentType = "text/tab-separated-values")
    #### Load grouping table from file
    observeEvent(input$loadrgroups$datapath,{rawstuff$rawgroupraw <- read.table(input$loadrgroups$datapath, header=T, sep='\t')})
    
    #  When the Groups are confirmed, save the current view in rawstuff and make xcmsRaw objects
    #  And also generate corresponding list objects
    
    observeEvent(input$confrgroups,{rawstuff$rawgroupraw <- data.frame(File = as.character(hot_to_r(input$rawgrouping)$File),
                                                                       Group = as.character(hot_to_r(input$rawgrouping)$Group),
                                                                       stringsAsFactors = F
    )
    ## Make list object of grouped file names                                        
    colme <- list()
    for (l in unique(rawstuff$rawgroupraw$Group)){
        colme[[l]] <- as.character(paste0(dirname(input$rfileload$datapath),rawstuff$rawgroupraw$File[which(rawstuff$rawgroupraw$Group==l)]))
    }
    #get groups in alphabetic order
    rawstuff$rawgroups <- colme[order(names(colme))]
    #make xcmsRaws (time consuming)
    rawstuff$rawdata <- EICrawP(rawstuff$rawgroups, workers = 10)
    
    })    
    
    ##################################################################   
    ###########FILTER MODULE######
    
    observeEvent((!is.null(colstuff$anagroupnums)),{
        
        sa <- reactive({colstuff$anagroupnums[[input$selgr]]})
        
        sa2 <- reactive({
            sl <- sa()
            names(sl) <- gsub("^X","",gsub("_XIC","",colnames(tablestuff$tablecut)[sl]))
            return(sl)})
        
        output$selgr <- renderUI({
            selectizeInput('selgr', 'Select group to filter for', choices = names(colstuff$anagroupnames))
        })
        
        
        output$subsel1 <- renderUI({
            selectizeInput('selcol1', 'Select group property columns', selected = tablestuff$grprops[c(1,5)], choices = tablestuff$grprops, multiple = TRUE)
        })    
        
        output$subsel3 <- renderUI({
            selectizeInput('selcol2', 'Select sample property columns', selected= saprops()[['fold_over_mean']][which(saprops()[['fold_over_mean']] %in% paste0(colstuff$anagroupnames[[input$selgr]],"_foldav"))], choices = saprops(), multiple = TRUE)
        })    
        
        output$subsel4 <- renderUI({
            selectizeInput('selcol3', 'Select sample intensity columns', selected = colstuff$anagroupnames[[input$selgr]], choices = colstuff$anagroupnames, multiple = TRUE)
        })    
        
        output$subsel5 <- renderUI({
            selectizeInput('selcol4', 'Select additional columns', 
                           choices = colnames(tablestuff$tablecut)[which(!colnames(tablestuff$tablecut) %in% c(unlist(colstuff$anagroupnames),'mz','rt'))], multiple = TRUE)
        })    
        
        output$subsel2 <- renderUI({
            selectizeInput('selsa', 'Select sample of interest',  choices = sa2())
        })
        
        output$rtwd <- renderUI({
            
            numericInput('rtw', 'EIC RT window (sec)', rawstuff$rtw, min=1, step = 1)
        })
        
      #  output$rtmin <- renderUI({
       #     selectizeInput('selsa', 'Select sample of interest',  choices = sa2())
        #})
        
        #output$rtmax <- renderUI({
         #   selectizeInput('selsa', 'Select sample of interest',  choices = sa2())
        #})
        
        #get the stats column for selected group and split it for filtering purposes
        #  observeEvent(input$selgr,{if(is(try(tablestuff$tablecut[[paste0(input$selgr,"_stats")]]),"try-error")){
        #     tablestuff$gprop <- NULL}
        #    else
        #   {tablestuff$gprop <- data.frame(apply(splitter(tablestuff$tablecut[[paste0(input$selgr,"_stats")]],
        #                                                    coln = tablestuff$grprops ),2,as.numeric))}})
        
        gprop <- reactive ({data.frame(apply(splitter(tablestuff$tablecut[[paste0(input$selgr,"_stats")]],
                                                      coln = tablestuff$grprops ),2,as.numeric))})
        
        #get the stats column for all groupd and split it for filtering purposes
        observeEvent(c(input$selgr,tablestuff$tablecut),{
            slc <- which(paste0(names(colstuff$anagroupnames),"_stats") %in% colnames(tablestuff$tablecut))
            print(slc)
            print(paste0(names(colstuff$anagroupnames)[slc[1]],"_",tablestuff$grprops))
            if(length(slc)==0){
                tablestuff$gprop <- NULL}
            else
            {   
                tablestuff$gprop <- data.frame(apply(splitter(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[1]],"_stats")]],
                                                              coln = paste0(names(colstuff$anagroupnames)[slc[1]],"_",tablestuff$grprops)),2,as.numeric))
                
                if(!is(try(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[1]],"_fold_Max")]]),"try-error")){
                    tablestuff$gprop <- cbind(tablestuff$gprop, data.frame(apply(splitter(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[1]],"_fold_Max")]],
                                                                                          coln = paste0(colnames(tablestuff$tablecut)[colstuff$anagroupnums[[names(colstuff$anagroupnames)[slc[1]]]]],"_foldmx")),2,as.numeric)))}
                
                if(!is(try(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[1]],"_fold_Average")]]),"try-error")){
                    tablestuff$gprop <- cbind(tablestuff$gprop, data.frame(apply(splitter(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[1]],"_fold_Average")]],
                                                                                          coln = paste0(colnames(tablestuff$tablecut)[colstuff$anagroupnums[[names(colstuff$anagroupnames)[slc[1]]]]],"_foldav")),2,as.numeric)))}
                
                if(length(slc)>1){for (i in c(2:length(slc))){
                    tablestuff$gprop <- cbind(tablestuff$gprop,data.frame(apply(splitter(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[i]],"_stats")]],
                                                                                         coln = paste0(names(colstuff$anagroupnames)[slc[i]],"_",tablestuff$grprops)),2,as.numeric)))
                    
                    if(!is(try(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[i]],"_fold_Max")]]),"try-error")){
                        tablestuff$gprop <- cbind(tablestuff$gprop, data.frame(apply(splitter(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[i]],"_fold_Max")]],
                                                                                              coln = paste0(colnames(tablestuff$tablecut)[colstuff$anagroupnums[[names(colstuff$anagroupnames)[slc[i]]]]],"_foldmx")),2,as.numeric)))}
                    
                    if(!is(try(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[i]],"_fold_Average")]]),"try-error")){
                        tablestuff$gprop <- cbind(tablestuff$gprop, data.frame(apply(splitter(tablestuff$tablecut[[paste0(names(colstuff$anagroupnames)[slc[i]],"_fold_Average")]],
                                                                                              coln = paste0(colnames(tablestuff$tablecut)[colstuff$anagroupnums[[names(colstuff$anagroupnames)[slc[i]]]]],"_foldav")),2,as.numeric)))}
                    
                }}
            }})
        
        saprops <- reactive({list(fold_over_max = grep("_foldmx",colnames(tablestuff$gprop), value=T), fold_over_mean = grep("_foldav",colnames(tablestuff$gprop), value=T))})
        
        
        ###filtered data
        selex <- eventReactive(input$fButton,{whichvec(cbind(tablestuff$tablecut,tablestuff$gprop),
                                                       fil=isolate(hot_to_r(input$filtable2))
        )})
        
        sele <- reactive({return(if(class(try(length(selex()))) == "try-error"){1:(nrow(tablestuff$tablecut))}else{selex()})})
        
        
        
        
        
        #filter the input dataframe to show only items that match filter criteria
        filt <- reactive ({tablestuff$tablecut[sele(),]})
        
        combino <- reactive({if(!is(try(tablestuff$gprop[sele(),paste0(input$selgr,"_",input$selcol1)]),"try-error") & !is.null(tablestuff$gprop)){
            return(cbind(tablestuff$tablecut[sele(),c('mz','rt',input$selcol3,input$selcol4)],
                         tablestuff$gprop[sele(),paste0(input$selgr,"_",input$selcol1)],
                         tablestuff$gprop[sele(),input$selcol2]))
        }else{
            return(tablestuff$tablecut[sele(),])
        }})         
        
        hmap <- eventReactive(input$goButton,
                              {heatmapx(filt(), cols=unlist(colstuff$anagroupnums), rel=T, log=T, 
                                        seriate="mean", margins=c(160,150),dendrogram="both")})
        
        
        temptable <- eventReactive(input$fButton,{hot_to_r(input$filtable2)}) 
        fixtable1 <- eventReactive(input$fButton,{df <- as.data.frame(setNames(replicate(length(c(colnames(tablestuff$tablecut),colnames(grprops))),
                                                                                         character(1), simplify = F),
                                                                               c(colnames(tablestuff$tablecut),colnames(grprops))),
                                                                      stringsAsFactors=FALSE)
        return(dftrans(isolate(temptable()),df))  
        })
        
        
        
        ftable1 <- eventReactive(input$fButton2,{df <- as.data.frame(setNames(replicate(length(colnames(combino())),character(1), simplify = F), colnames(combino())),stringsAsFactors=FALSE) 
        return(if(class(try(dftrans(isolate(fixtable1()),df))) == "try-error"){df}else{dftrans(isolate(fixtable1()),df)})         }, ignoreNULL=TRUE)
        
        
        output$pdfButton <- downloadHandler(filename= function(){paste0(input$fname,"_EICs.pdf")}, 
                                            content = function(file){EICplot(rawstuff$rawgroups,rawstuff$rawdata, binvar1 =names(rawstuff$rawgroups),
                                                                             alli=combino(), fileNamex = file, rtw = input$rtw, evalmode =F , grn=3, pdfout=T, deffile = T)},
                                            contentType = "application/pdf")
        
        
        
        ######## Download current grouping table as shown
        output$tbButton <- downloadHandler(filename= function(){paste0(input$fname,"_table.csv")}, 
                                           content = function(file){temp <- tablestuff$tablecut[sele(),]
                                                                    temp$Fragments <- tablestuff$fragments[sele()]
                                               return(write.csv(temp, file, row.names = F))
                                               #  expo <- cbind(filt(),
                                               #  gfilt()[,input$selcol1],
                                               #  cbind(ffilt(),ffilta())[,input$selcol2])
                                               
                                               #                                        empty <- as.data.frame(setNames(replicate(length(colnames(expo)),character(1), simplify = F), colnames(expo)), stringsAsFactors=FALSE, row.names = "FILTER")
                                               #                                      expo <- rbind(dftrans(temptable(), empty),
                                               #                                                   expo, stringsAsFactors = FALSE)
                                               #                                      return(write.csv(expo, file, row.names = F))
                                           },
                                           contentType = "text/csv"
        )
        
        ############################################
output$txt <- renderPrint({print(tablestuff)
    print(input$loadrgroups$datapath)
                     #print(specranges())
                      #     print(specranges2())#MS2stuff$parents[input$parenttab_rows_selected,"scannum"])#MS2stuff$spectra)
            #paste0(colstuff$anagroupnames[[input$selgr]],"_foldav")
            #  print(length(strsplit(as.character(tablestuff$tablecut[1,grep("_stats",colnames(tablestuff$tablecut))[1]])," ")[[1]]))
            # print(tablestuff$tablecut[1,grep("_stats",colnames(tablestuff$tablecut))[1]])
            # print(rawstuff$rawgroupraw)
            #print(rawstuff$rawgroups)
            #print(rawstuff$rawdata)
        })#renderPrint({hot_to_r(input$filtable2)})#renderPrint({ilistn()})#renderPrint({(gfilt()[1:5,input$selcol1])})#renderPrint({typeof(input$selcol1)})#
        #############################################                    
        

        
        output$hmtable <- DT::renderDataTable({
            DT::datatable(combino(), selection = 'single',
                          #filter='top',
                          extensions= c('FixedHeader'),
                          options=list(scrollX=T,
                                       scrollY=T,
                                       paging= T,
                                       lengthMenu = c(10,100,1000),
                                       searching =F
                          )) %>% DT::formatRound('mz', 5) %>% DT::formatRound('rt', 1)
        })
        
        # output$hmtable2 <- DT::renderDataTable({
        #    DT::datatable(
        
        #       if(!is(try(tablestuff$gprop[sele(),paste0(input$selgr,"_",input$selcol1)]),"try-error")){
        #      cbind(tablestuff$tablecut[sele(),c('mz','rt',input$selcol3,input$selcol4)],
        #           tablestuff$gprop[sele(),paste0(input$selgr,"_",input$selcol1)],
        #          tablestuff$gprop[sele(),input$selcol2])
        #      }else{
        #         tablestuff$tablecut[sele(),]  
        #              },
        #       selection = 'single',
        #filter='top',
        #                extensions= c('FixedHeader'),
        #               options=list(scrollX=T,
        #                           scrollY=T,
        #                          paging= T,
        #                         lengthMenu = c(10,100,1000),
        #                        searching =F
        #          ))# %>% DT::formatRound('mz', 5) %>% DT::formatRound('rt', 1)
        #})
        
        
        output$filtable2 <- renderRHandsontable({
            
            rhandsontable(ftable1(),
                          #ftable1()[which(rownames(ftable1()) %in% input$selcol1),],
                          readOnly = FALSE,
                          contextMenu = FALSE,
                          stringsAsFactors = FALSE)
        }) 
        
        output$plot1 <- renderPlot({
            # plot(feats$mz,feats$rt)
            #  EICplot(colme,ttsumme, binvar1 =names(colme),
            #         alli=feats[1,], fileNamex = "ttest", rtw = 45, evalmode =F , grn=3, pdfout=F)
            EICplot(rawstuff$rawgroups,rawstuff$rawdata, binvar1 =names(rawstuff$rawgroups),
                    alli=combino()[input$hmtable_rows_selected,], fileNamex = "XTEST", rtw = input$rtw, evalmode =F , grn=3, pdfout=F, cx= 1.2)
        })
        
        output$plot2 <- renderPlotly({
            #plot(showme[input$mytable1_rows_selected,1],showme[input$mytable1_rows_selected,2])
            hmap()
        })
        ########################################################################################
        ############EXPLORER MODULE###################
        #make list of files for selection of EIC traces
        leic <- reactive({leic <- lapply(rawstuff$rawgroups,basename)
        #names(leic) <- names(rawstuff$rawgroups)
        return(leic)})
        
        #render selection dialog for raw files to use as EIC source
        output$EICfiles <- renderUI({
            selectizeInput('EICsel', 'Select raw files',
                           #selected= saprops()[['fold_over_mean']], 
                           choices = leic(), multiple = TRUE)
        })    
        
        # get a list containing the xcmsRaw objects as selected in dialog EICfiles=EICsel 
        rfs <- reactive({ rawselect(input$EICsel,unlist(rawstuff$rawdata)) })
        
        #list of EICs with metadata
        rr <- reactive({readlist <- list()
        for (i in c(1:length(rfs()))){
            readlist[[i]] <- rawread(rfs()[[i]], mzrange = EICranges$mz)}
        return(readlist)
        })
        
        #Show TICs instead of EICs
        TIC <- reactive({FALSE})
        
        #list of EICs reformatted for use as plot input
        EICs <- reactive({ eiclist <- list()
        for (i in c(1:length(rr()))){
            if (TIC()){
                out <- data.frame(rr()[[i]]$rt, rr()[[i]]$tic, rr()[[i]]$scan)
                colnames(out) = c("rt","intensity","scan")}
            else{
                out <- data.frame(rr()[[i]]$rt, rr()[[i]]$intensity, rr()[[i]]$scan)
                colnames(out) = c("rt","intensity","scan")
            }
            eiclist[[i]] <- out}
        return(eiclist)
        })
        ##############EIC plot###################   
        #render the EIC plot
        
        EICranges <- reactiveValues(mz=c(0,1500), rt=NULL)
        
        output$chromly <- renderPlotly({
            p <- plot_ly(EICs()[[1]], x = EICs()[[1]][,1], y = EICs()[[1]][,2], 
                         name = paste(rr()[[1]]$fname, format(max(EICs()[[1]][,2]),scientific = T, digits=4)), 
                         source="chromatogram",
                         type = 'scatter', mode = 'lines+markers',
                         marker = list(size=1),
                         xaxis = "RT (min)", yaxis = "Intensity"
            )
            
            p <- layout(p, xaxis = list(range = EICranges$rt ),
                        yaxis = list(autorange = T, exponentformat = "e"))
            
            if (length(EICs())>1){
                for (i in c(2:length(EICs()))){
                    p <- add_trace(p,x = EICs()[[i]][,1], y = EICs()[[i]][,2], 
                                   name = paste(rr()[[i]]$fname, format(max(EICs()[[i]][,2]),scientific = T, digits=4)), 
                                   source="chromatogram",
                                   type = 'scatter', mode = 'lines+markers',
                                   marker = list(size=1),
                                   xaxis = "RT (min)", yaxis = "Intensity"
                    )}}
            return(p)
        })
        
        #data selected in output$chromly by selection box
        chrom_selected <- reactive({event_data("plotly_selected", source = "chromatogram")})
        #data selected in output$chromly by click
        chrom_point <- reactive({event_data("plotly_click", source = "chromatogram")})
        
        #############################SPECTRUM##########
        
        
        specranges <- reactiveValues(x=c(0,1500), y=c(0,100), file = 1, scan = 1)
        
        selpoints <- reactiveValues(a=data.frame(mz=0,intensity=100), b=data.frame(mz=0,intensity=100))
        
        keyin <- reactiveValues(keyd = "NO")
        
        observeEvent(input$keyd,{keyin$keyd <- input$keyd})
        
        observeEvent(input$spec_click,{if (keyin$keyd == 16){
            
            selpoints$a <- nearPoints(data.frame(mz=Spec()$mz,
                                                 intensity=Spec()$intensity/(max(Spec()$intensity)/100)),
                                      input$spec_click, 
                                      xvar= "mz", yvar = "intensity", 
                                      threshold = 10, maxpoints = 1)}})
        
        observeEvent(input$spec_hover,{selpoints$b <- nearPoints(data.frame(mz=Spec()$mz,
                                                                            intensity=Spec()$intensity/(max(Spec()$intensity)/100)),
                                                                 input$spec_hover, 
                                                                 xvar= "mz", yvar = "intensity", 
                                                                 threshold = 10, maxpoints = 1)})
        
        observeEvent(chrom_point(),{specranges$file <- chrom_point()$curveNumber+1
        specranges$scan <- EICs()[[specranges$file]]$scan[which.min(abs(EICs()[[specranges$file]]$rt-chrom_point()$x))]
        
        })
        
        #get spectrum data
        Spec <- eventReactive(chrom_point(), {as.data.frame(na.omit(getScan(rfs()[[specranges$file]],
                                                                            scan = specranges$scan)))})
        
        #spectrum info
        
        output$specinfo <- renderPrint({#cat("RT: ", 
            #rfs()[[specranges$file]]@scantime[which(rfs()[[specranges$file]]@acquisitionNum == specranges$scan)])
            print(input$keyd)
            print(input$spec_brush$xmin)
            cat(selpoints$a$mz,selpoints$b$mz,selpoints$b$mz-selpoints$a$mz)
        })
        
        #plot spectrum
        output$spec <- renderPlot({
            #  Get subset based on selection
            specplot(x=Spec()$mz,
                     y=Spec()$intensity/(max(Spec()$intensity)/100),
                     norm=1,
                     cx=2.5,
                     k = 10,
                     fileName = rr()[[chrom_point()$curveNumber+1]]$fname,
                     xrange = specranges$x, yrange = specranges$y,
                     maxi=max(Spec()$intensity)
                     #xlim = specranges$x, ylim =specranges$y
            )
            points(selpoints$a, col = "red")
            points(selpoints$b, col = "orange")
            
        })
        ###Zooming in the spectrum
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$spec_dblclick, {
            brush <- input$spec_brush
            if (!is.null(brush)) {
                specranges$x <- c(brush$xmin, brush$xmax)
                specranges$y <- c(0,max((Spec()$intensity/(max(Spec()$intensity)/100))[which(Spec()$mz>=input$spec_brush$xmin
                                                                                             & Spec()$mz<=input$spec_brush$xmax)]) )
                
            } else {
                specranges$x <- range(Spec()$mz)
                specranges$y <- c(0,100)
            }})
        
        
        #####Change EIC and Spec ranges based on selection in hmtable
        observeEvent(input$hmtable_rows_selected, {
            if(is.null(input$hmtable_rows_selected)){
                EICranges$mz <- NULL
                EICranges$rt <- NULL
                specranges$x <- range(Spec()$mz)
                specranges$y <- c(0,100)
                
            }
            EICranges$mz <- c(combino()[input$hmtable_rows_selected,]$mz-combino()[input$hmtable_rows_selected,]$mz*5e-6,
                              combino()[input$hmtable_rows_selected,]$mz+combino()[input$hmtable_rows_selected,]$mz*5e-6)
            EICranges$rt <- c(combino()[input$hmtable_rows_selected,]$rt-90,
                              combino()[input$hmtable_rows_selected,]$rt+90)
            specranges$x <- c(combino()[input$hmtable_rows_selected,]$mz-2.2,
                              combino()[input$hmtable_rows_selected,]$mz+3.3)
            
            maxi <-   max((Spec()$intensity/(max(Spec()$intensity)/100))[which(Spec()$mz >= combino()[input$hmtable_rows_selected,]$mz-5
                                                                               & Spec()$mz <= combino()[input$hmtable_rows_selected,]$mz+5)])
            if (is.infinite(maxi)){maxi <- 100}
            specranges$y <-  c(0,maxi)
            
        })
        
        ########MS2 explorer module###########
        
        MS2stuff <- reactiveValues(parents = NULL,
                                   spectra = NULL,
                                   cache = NULL,
                                   fragtab = NULL,
                                   fragints = NULL,
                                   highlightfrags = NULL)
        specranges2 <- reactiveValues(x=c(0,1500), y=c(0,100), file = 1, scan = 1)
        
        
        observeEvent(input$hmtable_rows_selected,
                     {MS2stuff$parents <- Parentsearch(rawstuff$rawdata,#rawfs,
                                                       combino()[input$hmtable_rows_selected,"mz"],
                                                       combino()[input$hmtable_rows_selected,"rt"],
                                                       partol= 1.02, rttol=20, MS1 = F)@MS2meta
                     
                     MS2stuff$fragtab <- if(nrow(decodeanno(tablestuff$fragments[sele()][input$hmtable_rows_selected]))>0){
                         decodeanno(tablestuff$fragments[sele()][input$hmtable_rows_selected])
                     }else{
                             data.frame(mz=numeric(1),sum_formula=character(1),SMILE=character(1),stringsAsFactors = F)}
                     })
        
        observeEvent(input$parenttab_select$select$r,
                     {MS2stuff$spectra <- as.data.frame(getMSnOwn(rawselect2(hot_to_r(input$parenttab)[input$parenttab_select$select$r,"file"],
                                                                             rawstuff$rawdata),
                                                                               as.integer(hot_to_r(input$parenttab)[input$parenttab_select$select$r,"scannum"])
                                                                                   ))
                     
                     })
        observeEvent(c(MS2stuff$spectra,MS2stuff$fragtab),{if(!is.null(MS2stuff$spectra)&!is.null(MS2stuff$fragtab)&nrow(MS2stuff$fragtab)>0){
            MS2stuff$highlightfrags <- MS2stuff$spectra[which(rowMin(abs(outer(MS2stuff$spectra$mz,MS2stuff$fragtab$mz,"-")))/MS2stuff$spectra$mz < 5e-6),]
        }
        })
        
        
    #    observeEvent(input$hmtable_rows_selected,
     #                {   if(!is.null(MS2stuff$cache)){
      #                   tablestuff$fragments[sele()][MS2stuff$cache] <- encodeanno(hot_to_r(input$fragtab))
       #                  }
        #                 MS2stuff$fragtab <- decodeanno(tablestuff$fragments[sele()][input$hmtable_rows_selected])
         #                MS2stuff$cache <- input$hmtable_rows_selected
          #           })
        observeEvent(input$sFrags,
                     {tablestuff$fragments[sele()][input$hmtable_rows_selected] <- encodeanno(hot_to_r(input$fragtab2))
                     MS2stuff$fragtab <- hot_to_r(input$fragtab2)
                     })
        
    #    observeEvent(input$hmtable_rows_selected,
     #                {   
      #                   MS2stuff$fragtab <- decodeanno(tablestuff$fragments[sele()][input$hmtable_rows_selected])
       #                  MS2stuff$cache <- input$hmtable_rows_selected
        #             })
        
        output$fragtab2 <- renderRHandsontable({if(nrow(decodeanno(tablestuff$fragments[sele()][input$hmtable_rows_selected]))>0){
            rhandsontable(decodeanno(tablestuff$fragments[sele()][input$hmtable_rows_selected]),
                          rowHeaders = NULL,
                          selectCallback = T)
        }else{
                  rhandsontable(data.frame(mz=numeric(1),sum_formula=character(1),SMILE=character(1),stringsAsFactors = F),
                               rowHeaders = NULL,
                              selectCallback = T)
                }
        })
        
   #     output$fragtab2 <- renderRHandsontable({if(!is.null(MS2stuff$fragtab) & nrow(MS2stuff$fragtab)>0){
    #        rhandsontable(MS2stuff$fragtab, rowHeaders = NULL, selectCallback = T)
     #   }else{
      #      rhandsontable(data.frame(mz=numeric(1),sum_formula=character(1),SMILE=character(1),stringsAsFactors = F),
       #                   rowHeaders = NULL,
        #                  selectCallback = T)
    #    }
            
     #   }) 
        
        ###plot image of molecule selected in fragtab2
        output$molplot <- renderPlot({psmile(hot_to_r(input$fragtab2)$SMILE[input$fragtab2_select$select$r], height = 300, width =300)})
        
        
        
        output$parenttab <- renderRHandsontable({if(!is.null(MS2stuff$parents)){
            rhandsontable(MS2stuff$parents, selectCallback = T)
        }   
        })
        
        #plot spectrum
        output$spec2 <- renderPlot({
            #  Get subset based on selection
            specplot(x=MS2stuff$spectra$mz,
                     y=MS2stuff$spectra$intensity/(max(MS2stuff$spectra$intensity)/100),
                     norm=1,
                     cx=2.5,
                     k = 10,
                     fileName = MS2stuff$parents[input$parenttab_select$select$r,"file"],
                     xrange = specranges2$x, yrange = specranges2$y,
                     maxi=max(MS2stuff$spectra$intensity)
                     #xlim = specranges$x, ylim =specranges$y
            )
            points(selpoints2$a, col = "red")
            points(selpoints2$b, col = "orange")
            if(!is.null(MS2stuff$highlightfrags)){if(nrow(MS2stuff$highlightfrags)>0){
            segments(x0=MS2stuff$highlightfrags$mz,y0=0,x1=MS2stuff$highlightfrags$mz,y1=MS2stuff$highlightfrags$intensity/(max(MS2stuff$spectra$intensity)/100), col = "red")
                if(length(which(abs(hot_to_r(input$fragtab2)$mz[input$fragtab2_select$select$r]-MS2stuff$highlightfrags$mz)/hot_to_r(input$fragtab2)$mz[input$fragtab2_select$select$r] < 5e-6))>0){
                    colme <- MS2stuff$highlightfrags[which(abs(hot_to_r(input$fragtab2)$mz[input$fragtab2_select$select$r]-MS2stuff$highlightfrags$mz)/hot_to_r(input$fragtab2)$mz[input$fragtab2_select$select$r] < 5e-6),]
                    segments(x0=colme$mz,y0=0,x1=colme$mz,y1=colme$intensity/(max(MS2stuff$spectra$intensity)/100), col = "green")
                    
                }
                }}
            
        })
        ###Zooming in the spectrum
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$spec2_dblclick, {
            brush <- input$spec2_brush
            if (!is.null(brush)) {
                specranges2$x <- c(brush$xmin, brush$xmax)
                specranges2$y <- c(0,max((MS2stuff$spectra$intensity/(max(MS2stuff$spectra$intensity)/100))[which(MS2stuff$spectra$mz >=input$spec2_brush$xmin
                                                                                             & MS2stuff$spectra$mz<=input$spec2_brush$xmax)]) )
                
            } else {
                specranges2$x <- range(MS2stuff$spectra$mz)
                specranges2$y <- c(0,100)
            }})
        
        selpoints2 <- reactiveValues(a=data.frame(mz=0,intensity=100), b=data.frame(mz=0,intensity=100))
        
        observeEvent(input$spec2_click,{if (keyin$keyd == 16){
            
            selpoints2$a <- nearPoints(data.frame(mz=MS2stuff$spectra$mz,
                                                 intensity=MS2stuff$spectra$intensity/(max(MS2stuff$spectra$intensity)/100)),
                                      input$spec2_click, 
                                      xvar= "mz", yvar = "intensity", 
                                      threshold = 10, maxpoints = 1)}})
        
        observeEvent(input$spec2_hover,{selpoints2$b <- nearPoints(data.frame(mz=MS2stuff$spectra$mz,
                                                                            intensity=MS2stuff$spectra$intensity/(max(MS2stuff$spectra$intensity)/100)),
                                                                 input$spec2_hover, 
                                                                 xvar= "mz", yvar = "intensity", 
                                                                 threshold = 10, maxpoints = 1)})
        
        output$specinfo2 <- renderPrint({#cat("RT: ", 
            #rfs()[[specranges$file]]@scantime[which(rfs()[[specranges$file]]@acquisitionNum == specranges$scan)])
            print(input$keyd)
            print(input$spec2_brush$xmin)
            cat(selpoints2$a$mz,selpoints2$b$mz,selpoints2$b$mz-selpoints2$a$mz)
        })
        
        
        
        ####Chemcalc mz query module######
        
        ###Enter mz textInput
        queryval <- reactiveValues(a = 0)
        
        observeEvent(selpoints$a$mz,{queryval$a <- round(selpoints$a$mz,6)})
        observeEvent(selpoints2$a$mz,{queryval$a <- round(selpoints2$a$mz,6)})
        
        output$mzquery <- renderUI({
            textInput("mzquery", label = "Sum formula", value = queryval$a)
        }) 
        
        ##currently not used: store selected files in reactiveValues
        #mzq <- reactiveValues(mz=0, tab=data.frame(em=NULL, fm=NULL, unsat=NULL, ppm=NULL))
        #observeEvent(input$mzButton,{mzq$mz <- input$mzquery
        #mzq$tab <- massquery(input$mzquery)
        #                            })
        
        #get sum formula table from ChemCalc
        mztab <- eventReactive(input$mzButton,{return(massquery(input$mzquery,elem = input$mzspace, ppm= input$mzppm, charge = input$mzcharge))})
        
        #render sum formula table
        output$hot1 <- renderRHandsontable({
            #options(digits=8)
            rhandsontable(mztab(),#format(mztab(),digits=10),
                          #ftable1()[which(rownames(ftable1()) %in% input$selcol1),],
                          readOnly = TRUE,
                          contextMenu = FALSE,
                          selectCallback = TRUE,
                          height = 300,
                          digits=8) %>%
                hot_cols(columnSorting = TRUE,format="0.000000")%>%
                hot_col("em",format="0.000000")
        }) 
        
    })
    
}