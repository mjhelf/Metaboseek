#' reformatTable
#' 
#' 
#' a function to change a data.frame into a Mosaic table object
reformatTable <- function(df= data.frame(mz=1:3, rt = character(3), int1=4:6, int2=4:6, int3=7:9), # data frame 
                          mzcol= "mz", #column in df with mz values (columnname)
                          rtcol= "rt", #column in df with mz values (columnname)
                          commentcol = "comments",
                          fragmentcol = "fragments",
                          rtFormat = "sec", # "sec" or "min" 
                          anagrouptable = data.frame(Column = c("int1", "int2", "int3"), Group = c("G1","G2","G2")),
                          tablename){
    
    #make columns if they don't exist:
    if (class(try(df[,mzcol], silent = T))=="try-error"){ df[,mzcol] <- numeric(nrow(df)) }
    if (class(try(df[,rtcol], silent = T))=="try-error"){ df[,rtcol] <- numeric(nrow(df)) }
    if (class(try(df[,commentcol], silent = T))=="try-error"){ df[,commentcol] <- character(nrow(df)) }
    if (class(try(df[,fragmentcol], silent = T))=="try-error"){ df[,fragmentcol] <- character(nrow(df)) }
    
    core = as.matrix(apply(df[,c(mzcol,rtcol)], 2, as.numeric))
    comments = as.character(df[,commentcol])
    fragments = as.character(df[,fragmentcol])
    
    #make intensity value matrix
    if(!is.null(anagrouptable)){
        #position of intensity columns in the input column
        colPos <- unlist(tableGrouping(df,anagrouptable)$anagroupnums)
    
    if (length(colPos)==1){
        intensities = as.numeric(df[,colPos])
        }else{
    intensities = as.matrix(apply(df[,colPos], 2, as.numeric))
        }
        
        newgrouping <- tableGrouping(intensities,anagrouptable)
        
    }
    
   typeof(data.frame(core,comments))
object.size(df[,unlist(tableGrouping(df,anagrouptable)$anagroupnums)])
object.size(intensities)

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