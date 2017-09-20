#' reformatTable
#' 
#' 
#' a function to change a data.frame into a Mosaic table object
#' mostly contains indexing lists plus the original df plus intensity values in a matrix
#' 
#' NOTES: Consider rtmin/max case, think about labeling experiments!
#' add filtertable slot
#' 
constructFeatureTable <- function(df= data.frame(mz=numeric(3), rt = numeric(3)),# data frame 
                          mzcol= "mz", #column in df with mz values (columnname)
                          rtcol= "rt", #column in df with mz values (columnname)
                          commentcol = "comments",
                          fragmentcol = "fragments",
                          rtFormat = "sec", # "sec" or "min" 
                          anagrouptable = NULL,
                          tablename = "Custom Table",
                          editable = T){ #T: free editing (add rows), but always see all columns in viewer, F: only comments can be edited directly, no adding of columns
    
    #make columns if they don't exist:
    if (class(try(df[,mzcol], silent = T))=="try-error"){ df[,mzcol] <- numeric(nrow(df)) }
    if (class(try(df[,rtcol], silent = T))=="try-error"){ df[,rtcol] <- numeric(nrow(df)) }
    if (class(try(df[,commentcol], silent = T))=="try-error"){ df[,commentcol] <- character(nrow(df)) }
    if (class(try(df[,fragmentcol], silent = T))=="try-error"){ df[,fragmentcol] <- character(nrow(df)) }
    
    FT = list()
    #save the entire dataframe 
    FT$df = df
    
    FT$tablename = tablename
    FT$editable = editable
    FT$core = c(mzcol,rtcol)
    FT$mz = mzcol
    FT$rt = rtcol
    FT$comments = commentcol
    FT$fragments = fragmentcol
    
    #default analysis result columns
    FT$summaryStats = unname(unlist(sapply(c("topgroup","maxfold", "maxfoldover"),
                                           grep,colnames(FT$df),
                                           value = T)))
    
    #make intensity value matrix
    if(!is.null(anagrouptable)){
        FT <- updateFTgrouping(FT,anagrouptable)
    }
    FT$others = colnames(FT$df)[which(!colnames(FT$df) %in% c(unlist(FT$gProps), unlist(FT$sProps),
                                                              FT$intensities, FT$intensities_norm,
                                                              FT$summaryStats, FT$commentcol,
                                                              FT$fragmentcol, FT$core))]   
    #selection and filtering for this object (these interact with selectize UI objects)
    FT$sele = nrow(df)
    FT$sortBy = mzcol
    FT$sortByDecreasing = F
    FT$order = nrow(df)
    
    
    FT$filters = list(  page = 1,  
                        inpage = 1,
                        filters = list(Filter1 = list(selected = 1:nrow(df),
                                              column = colnames(df)[1],
                                              minSel = NULL,
                                              maxSel = NULL,
                                              modeSel = NULL,
                                              txtSel = "",
                                              active = F
                          )),
                          sele= 1:nrow(df),
                          sortBy = "mz",
                          sortByDecreasing = F,
                          order = 1:nrow(df)
                          )
    
    FT$selectedCols = list(core = FT$core,
                           comments = FT$comments,
                           gProps = FT$gProps[[FT$gNames[1]]],
                           sProps = FT$sProps[[FT$gNames[1]]],
                           intensities = FT$anagroupnames[[FT$gNames[1]]],
                           others = FT$summaryStats)
    FT$selectedGroup = FT$gNames[1]
    FT$ctrlGroups = NULL
    FT$useNorm = F
    
    class(FT) <- "MOSAiC feature table"
    
    return(FT)

}

updateDF <- function(a, b){

        for (i in colnames(a)){
             b[,i] <- a[,i]}
        
        return(b)
    }
    


updateFeatureTable <- function(FT, df){ #update an existing Mosaic feature table with new data frame columns (leaving intensitiy tables and constants in effect)
    
    FT$df = updateDF(df,FT$df)
    if(!is.null(FT$anagrouptable)){
        FT <- updateFTgrouping(FT,FT$anagrouptable)
    }
    FT$summaryStats = unname(unlist(sapply(c("topgroup","maxfold", "maxfoldover"),
                                           grep,colnames(FT$df),
                                           value = T)))
    FT$others = colnames(FT$df)[which(!colnames(FT$df) %in% c(unlist(FT$gProps), unlist(FT$sProps),
                                                              FT$intensities, FT$intensities_norm,
                                                              FT$summaryStats, FT$commentcol,
                                                              FT$fragmentcol, FT$core))]
    FT$selectedCols = list(core = FT$core,
                           comments = FT$comments,
                           gProps = FT$gProps[[FT$gNames[1]]],
                           sProps = FT$sProps[[FT$gNames[1]]],
                           intensities = FT$anagroupnames[[FT$gNames[1]]],
                           others = FT$summaryStats)
    FT$selectedGroup = FT$gNames[1]
    
    return(FT)
}

updateFTgrouping <- function(FT,anagrouptable){
    
    FT$anagrouptable = anagrouptable
    newgrouping <- tableGrouping(FT$df,anagrouptable)
    
    FT$anagroupnames = newgrouping$anagroupnames
    FT$intensities = unname(unlist(FT$anagroupnames))
    FT$anagroupnums = newgrouping$anagroupnums

    if(length(grep("__norm",colnames(FT$df)))>0){
        FT$anagroupnames_norm = relist(paste0(unlist(FT$anagroupnames),"__norm"),FT$anagroupnames)
        FT$intensities_norm = unname(unlist(FT$anagroupnames_norm))
    }
    
    #Group names
    FT$gNames <- unique(anagrouptable$Group)
    
    #Sample names, grouped
    FT$sNames <- list()
    for(i in names(FT$anagroupnames)){
        #remove everything after double underscore (new default notation)
        FT$sNames[[i]] <- gsub("__(.*)","",FT$anagroupnames[[i]])
        #remove the XIC tag if it has only one underscore (old notation)
        FT$sNames[[i]] <- gsub("_XIC","",FT$sNames[[i]])
        #remove trailing underscores if any
        FT$sNames[[i]] <- gsub("_$","",FT$sNames[[i]])
    }
    
    FT$gProps = list()
    #detect and group property columns (group name + doubleunderscore)
    for(i in FT$gNames){
        #list all columns containing group names  followed by double underscore
        selCols <- grep(paste0(i,"__"),colnames(FT$df), value =T)
        if(length(selCols)>0){
            FT$gProps[[i]] <- selCols}
    }
    
    #detect and group sample property columns (sample name + doubleunderscore)
    FT$sProps = list()
    for(i in names(FT$gNames)){
        #list all columns containing sample names from group i followed by double underscore which are not in the intensities table
        selCols <- unlist(sapply(paste0(FT$sNames[[i]],"__"),grep,colnames(FT$df)[which(!colnames(FT$df) %in% c(FT$intensities,FT$intensities_norm))], value = T))
        if(length(selCols)>0){
            FT$sProps[[i]] <- sort(selCols)}
    }
    return(FT)
}

updateFTIndex <- function(tables){
    out <- names(tables)
    
    names(out) <- unname(sapply(tables, "[[", "tablename"))

    return(out)
}

## helper function to reformat Table analysis grouping tables

tableGrouping <- function(table, anagrouptable){
    ## Make list object of grouped column names                                        
    colme <- list()
    for (l in unique(anagrouptable$Group)){
        colme[[l]] <- as.character(anagrouptable$Column[which(anagrouptable$Group==l)])
    }
    anagroupnames <- colme
    ### Get column numbers from column names
    colnu <- integer(0)
    for (i in anagrouptable$Column){
        colnu<- c(colnu,which(colnames(table) == i))
    }
    ## Make list object of grouped column numbers
    colme <- list()
    for (l in unique(anagrouptable$Group)){
        colme[[l]] <- as.integer(colnu[which(anagrouptable$Group==l)])
    }
    anagroupnums <- colme
    return(list(anagroupnames = anagroupnames,anagroupnums = anagroupnums))
}

filterDF <- function(df,
                     filters = list()){
    
        cn <- names(filters)
        whi= row.names(df)

        for (i in cn[which(cn %in% colnames(df))]){
            whi <- row.names(df[whi,])[which( as.numeric(df[whi,i]) >= as.numeric(filters[[i]]$min)
                                         & as.numeric(df[whi,i]) <= as.numeric(filters[[i]]$max))]
            
        }
        
        
        return(unique(whi))
        }

####To be removed, goes by index rather than row.name
filterDFold <- function(df = pcame_mini,
                     filters = list(mz = list(ty = "numeric",
                                              min = 240,
                                              max = 300),
                                    rt = list(ty = "numeric",
                                              min = 200,
                                              max = 400))){
    
    cn <- names(filters)
    # print(cn)
    # print(cn[which(cn %in% colnames(df))])
    whi= 1:nrow(df)
    whit <- whi
    
    for (i in cn[which(cn %in% colnames(df))]){
        whi <- which( as.numeric(df[,i]) >= as.numeric(filters[[i]]$min)
                      & as.numeric(df[,i]) <= as.numeric(filters[[i]]$max))
        
        
        # print(whi)
        wx <- c(whit,whi)
        whit <- as.numeric(names(table(wx)[which(table(wx)==2)]))
        #print(whit)
    }
    return(unique(whit))
    }

