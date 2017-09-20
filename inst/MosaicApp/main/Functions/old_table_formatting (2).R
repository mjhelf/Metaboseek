#' reformatTable
#' 
#' 
#' a function to change a data.frame into a Mosaic table object
#' mostly contains indexing lists plus the original df plus intensity values in a matrix
#' 
#' NOTES: Consider rtmin/max case, think about labeling experiments!
#' add filtertable slot
#' 
constructFeatureTable <- function(df= data.frame(mz=1:3, rt = character(3), 
                                         int1_XIC=4:6, int2__XIC=4:6, int_3=7:9, 
                                         G1_stats=c("1 2 3 4 5 6","1 2 3 4 5 6","1 2 3 4 5 6"),
                                         G2_stats=c("1 2 3 4 5 6","1 2 3 4 5 6","1 2 3 4 5 6")
                                         ),# data frame 
                          mzcol= "mz", #column in df with mz values (columnname)
                          rtcol= "rt", #column in df with mz values (columnname)
                          commentcol = "comments",
                          fragmentcol = "fragments",
                          rtFormat = "sec", # "sec" or "min" 
                          anagrouptable = data.frame(Column = c("int1_XIC", "int2__XIC", "int_3"), Group = c("G1","G2","G2"),stringsAsFactors = F),
                          tablename = "testtable"){
    
    #make columns if they don't exist:
    if (class(try(df[,mzcol], silent = T))=="try-error"){ df[,mzcol] <- numeric(nrow(df)) }
    if (class(try(df[,rtcol], silent = T))=="try-error"){ df[,rtcol] <- numeric(nrow(df)) }
    if (class(try(df[,commentcol], silent = T))=="try-error"){ df[,commentcol] <- character(nrow(df)) }
    if (class(try(df[,fragmentcol], silent = T))=="try-error"){ df[,fragmentcol] <- character(nrow(df)) }
    
    collector = list()
    
    collector$tablename = tablename
    collector$core = c(mzcol,rtcol)
    collector$comments = commentcol
    collector$fragments = fragmentcol
    

    
    
    #make intensity value matrix
    if(!is.null(anagrouptable)){
        #position of intensity columns in the input column
        colPos <- unlist(tableGrouping(df,anagrouptable)$anagroupnums)
    
    if (length(colPos)==1){
        collector$intensities = as.numeric(df[,colPos])
        }else{
        collector$intensities = as.matrix(apply(df[,colPos], 2, as.numeric))
        }
        
        newgrouping <- tableGrouping(intensities,anagrouptable)
    
    #Group names
    collector$gNames <- unique(anagrouptable$Group)
    
    #Sample names, grouped
    collector$sNames <- list()
    for(i in names(newgrouping$anagroupnames)){
    #remove everything after double underscore (new default notation)
        collector$sNames[[i]] <- gsub("__(.*)","",newgrouping$anagroupnames[[i]])
    #remove the XIC tag if it has only one underscore (old notation)
        collector$sNames[[i]] <- gsub("_XIC","",sNames[[i]])
    #remove trailing underscores if any
        collector$sNames[[i]] <- gsub("_$","",sNames[[i]])
    }
    
    collector$gProps = list()
    #detect and group property columns (group name + doubleunderscore)
    for(i in gNames){
        #list all columns containing group names  followed by double underscore
        selCols <- grep(paste0(i,"__"),colnames(df), value =T)
        if(length(selCols)>0){
            collector$gProps[[i]] <- selCols}
    }
    
    #detect and group sample property columns (sample name + doubleunderscore)
    collector$sProps = list()
    for(i in names(sNames)){
        #list all columns containing sample names from group i followed by double underscore which are not in the intensities table
        selCols <- unlist(sapply(paste0(sNames[[i]],"__"),grep,colnames(df)[which(!colnames(df) %in% colnames(intensities))], value = T))
        if(length(selCols)>0){
            collector$sProps[[i]] <- selCols}
    }
    
    
    collector$anagroupnames = newgrouping$anagroupnames
    collector$anagroupnums = newgrouping$anagroupnums
    collector$anagrouptable = anagrouptable
    
    }
    collector$others = colnames(df)[which(!colnames(df) %in% c(unlist(gProps), unlist(sProps), colnames(intensities), commentcol, fragmentcol, core))]
    
    
    #selection and filtering for this object
    collector$sele = nrow(df)
    collector$filters = list()
    
    #save the entire dataframe 
    collector$df = df
    
    class(collector) <- "MOSAiC feature table"
    
    return(collector)

   }

list(
    
    tablename = "Custom Features",
    
    core = NULL, #mz, rt values
    comments = NULL, #comments column
    intensities = NULL, #intensity values
    norm_intensities = NULL,
    #grprops = NULL,
    gprop = NULL, #group property columns
    fragments = NULL, #fragment database column
    others = NULL, #all other columns
    
    #columns
    anagroupnames = NULL, #grouped columnnames in intensity table/matrix
    anagroupnums = NULL, #grouped columnnumbers in intensity table/matrix
    #selection and sorting
    sele = NULL
    
)

tt <- reformatTable()

write_json(tt, "./testjson.txt")


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


splitProperties <- function(df, tag="_stats"){
    
    statPos <- grep(tag, colnames(df))
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
        
