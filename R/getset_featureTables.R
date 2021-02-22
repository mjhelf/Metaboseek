#' FeatureTable
#' 
#' @param x the \code{values} object
#' @param value an \code{MseekFT} or \code{data.frame} object
#' @param tableID table ID to retrieve. will use currently active Feature Table if not set.
#'  For setter, will create a new Feature Table under this ID if ID does not exist, 
#'  otherwise will override or update the selected Feature Table
#' @param update if TRUE, update table via \code{\link{updateFT}()} before 
#' getting its values
#' @param replace replace the current Feature table with the given tableID? 
#' If FALSE, will update instead.#' @return An \code{MseekFT} object
#' @param ... additional arguments passed to S3 methods
#' Getter and setter methods to retrieve or modify a Feature Table 
#' (\code{MseekFT} object) from \code{values} 
#'
#'
#' 
#' @rdname FeatureTable
#' 
#' @export
FeatureTable <- function(x, ...){
    
    UseMethod('FeatureTable', x)
    
}

#' @rdname FeatureTable
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

#' @rdname FeatureTable
#'
#' @export
'FeatureTable<-' <- function(x, value, ...){
    
    UseMethod('FeatureTable<-', value)
    
}

#' @rdname FeatureTable
#' @export
'FeatureTable<-.data.frame' <- function(x, value, replace = F,
                                        update = T, tableID = NULL){
    
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

#' @rdname FeatureTable
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
#' changes or returns the currently active Feature Table if 
#' supplied value is a valid table ID
#' 
#' @param x an MseekTree (reactivevalues) object
#' @param value a character() value corresponding to a table ID
#' 
#' @return returns the ID (character(1)) of the currently active Feature Table
#' 
#' @rdname activeFT
#'
#' @export
activeFT <- function(x){
    
    return(
    x$featureTables$active
)
    }


#' @rdname activeFT
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
#' @return nothing, but modifies \code{FeatureTable(values)$df}
#'  
#' @param values an MseekTree (reactivevalues) object
#' 
#' @export
updateFT <- function(values){
    
    if(!is.null(values$featureTables) 
       && !is.null(values$featureTables$Maintable$hasUpdates)
       && values$featureTables$Maintable$hasUpdates
       && values$featureTables$Maintable$updatedFrom == activeFT(values)
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
#' 
#' @details By default, will get its data from the underlying Feature Table,
#'  not using changes (such as comments) made immediately prior to selection, 
#'  but including all columns, including those not visible in the current table view.
#'
#' @return a data.frame that is a subset of \code{FeatureTable(x)$df}
#'
#' @param x a MseekValues (reactivevalues) object
#' @param liveView if TRUE, will get data from the liveView of the 
#' MainTable directly (default: FALSE)
#' @param ... additional arguments passed to S3 methods
#' @rdname FTselection
#'
#' @export
FTselection <- function(x, ...){
    
    UseMethod('FTselection', x)
    
    
}

#' @rdname FTselection
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


#' getFilters
#' 
#' get the currently applied filters in a Metaboseek session
#' 
#' @param x a MseekValues (reactivevalues) object
#' @param activeOnly if TRUE, gets only active Filters
#' 
#' @return a list of filters
#' 
getFilters <- function(x, activeOnly = TRUE){
    
    if(!length(x$featureTables)
       ||!length(x$featureTables$Filters)){
        res <- list()
        class(res) <- c("FilterList", class(res))
        return(res)
        }
    
    res <- x$featureTables$Filters$filterSet
    
    
    if(!length(res) 
       || !is.list(res)){
        res <- list()
        class(res) <- c("FilterList", class(res))
        return(res)
        }
    
    if(activeOnly){
    res <- res[sapply(res, function(n){n$active})]
    }
    
    class(res) <- c("FilterList", class(res))
    return(res)

    }


