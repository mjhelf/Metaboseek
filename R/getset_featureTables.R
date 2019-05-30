#' FeatureTable
#' 
#' @param values reactiveValues or list object with at least a values$featureTables$tables
#' @param tableID table ID to retrieve. will use currently active Feature Table if not set.
#'  For setter, will create a new Feature Table under this ID if ID does not exist, 
#'  otherwise will override or update the selected Feature Table
#' 
#' @rdname FeatureTable
#' 
#' @export
FeatureTable <- function(x, ...){
    
    UseMethod('FeatureTable', x)
    
}

#' @export
FeatureTable.reactivevalues <- function(x, update = F, tableID = NULL){
    
    if(update){        
        #make sure this part does not trigger the observer containing the setter function
        isolate({
            updateFT(x)
        })
    }
    
    if(is.null(tableID) || missing(tableID)){
        return(x$featureTables$tables[[x$featureTables$active]])
    }else{
        return(x$featureTables$tables[[tableID]])
    }
    
}




#' FeatureTable<-
#'
#' Getter and setter methods to retireve a Feature Table from \code{values} 
#'
#'
#' @param x the \code{values} object
#' @param value the value that gets set
#' @param replace replace the current Feature table with the given tableID? will update it if FALSE.
#'
#'  
#' @rdname FeatureTable
#'
#' @export
'FeatureTable<-' <- function(x, value, ...){
    
    UseMethod('FeatureTable<-', value)
    
}

#' @export
'FeatureTable<-.data.frame' <- function(x, value, replace = F, update = T, tableID = NULL){
    
    if(!missing(value) && !is.null(value)){
        
        if(update){        
            #make sure this part does not trigger the observer containing the setter function
            isolate({
                updateFT(x)
            })
        }
        
        
        if(is.null(tableID) || missing(tableID)){
            tableID <- activeFT(x)
        }
        
        #make a new featureTable if the selected tableID doesn't exist yet
        if(!tableID %in% names(x$featureTables$tables)){
            
            x$featureTables$tables[[tableID]] <- constructFeatureTable(value,
                                                                     mzcol= "mz", #column in df with mz values (columnname)
                                                                     rtcol= "rt", #column in df with mz values (columnname)
                                                                     commentcol = "comments",
                                                                     fragmentcol = "fragments",
                                                                     rtFormat = "sec", # "sec" or "min" 
                                                                     anagrouptable = NULL,
                                                                     tablename = paste0("modified_",FeatureTable(x)$tablename),
                                                                     editable = F)
            
            x$featureTables$index <- updateFTIndex(x$featureTables$tables)
            
        }else{
            
            if(replace){
                x$featureTables$tables[[tableID]]$df <- value
            }else{
                x$featureTables$tables[[tableID]] <- updateFeatureTable(x$featureTables$tables[[tableID]], value)
            }
        }
        
    }
    
    #if x is not returned, values becomes value in the local environment of the observer,
    #breaking any line of code expecting the MseekValues type values inside the observer 
    return(x)
    
    
    
}

#' @export
'FeatureTable<-.MseekFT' <- function(x, value, replace = T, tableID = NULL){
    
    if(!missing(value) && !is.null(value)){
        if(is.null(tableID) || missing(tableID)){
            tableID <- x$featureTables$active
        }
        
        x$featureTables$tables[[tableID]] <- value
        
        x$featureTables$index <- updateFTIndex(x$featureTables$tables)
        
        
    }
    #if x is not returned, values becomes value in the local environment of the observer,
    #breaking any line of code expecting the MseekValues type values inside the observer 
    return(x)
    
}

#' activeFT
#' 
#' returns the ID of the currently active Feature Table
#' 
#' @param x an MseekTree (reactivevalues) object
#'
#' @export
activeFT <- function(x){
    
    return(
    x$featureTables$active
)
    }

#' activeFT
#' 
#' changes the currently active Feature Table if supplied value is a valid table ID
#' 
#' @param x an MseekTree (reactivevalues) object
#' @param value a character() value corresponding to a table ID
#'
#' @export
"activeFT<-" <- function(x, value){
    
    isolate({
    updateFT(x)
    })
        
    if(is.character(value) && value %in% names(x$featureTables$tables)){
        x$featureTables$active <- value
    }else{
        warning("Tried to set the active table to a table ID that does not exist")
        }
    
    return(x)
    
}

#' updateFT
#' 
#' Update currently active Feature Table with any changes that may have
#'  been applied via the GUI
#'  
#' @param values an MseekTree (reactivevalues) object
#' 
#' @export
updateFT <- function(values){
    
    if(!is.null(values$featureTables) 
       && values$featureTables$Maintable$hasUpdates
    ){
        if((!is.null(values$featureTables$tables[[values$featureTables$active]]$editable) 
            && values$featureTables$tables[[values$featureTables$active]]$editable)
           || is.null(values$featureTables$Maintable$liveView$comments) ){
            values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$Maintable$liveView),colnames(values$featureTables$Maintable$liveView)] <- values$featureTables$Maintable$liveView
        }else{
            values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$Maintable$liveView),"comments"] <- values$featureTables$Maintable$liveView$comments
        }
    }   
    
}

#' FTselection
#'
#' get the rows that are currently selected in MainTable window.
#' Note: by default, will get its data from the underlying Feature Table,
#'  not using changes (such as comments) made immediately prior to selection, 
#'  but including all columns, including those not visible in the current table view.
#'
#' @param x a MseekValues (reactivevalues) object
#' @param liveView if TRUE, will get data from the liveView of the MainTable directly (default: FALSE)
#'
#' @export
FTselection <- function(x, ...){
    
    UseMethod('FTselection', x)
    
    
}

#' @export
FTselection.reactivevalues <- function(x, liveView = F){
    if(liveView){
        return(
            x$featureTables$Maintable$liveView[x$featureTables$Maintable$selected_rows,]
        )
    }else{
        return(
            FeatureTable(x)$df[row.names(x$featureTables$Maintable$liveView)[x$featureTables$Maintable$selected_rows],]
        )  
    }
    
}