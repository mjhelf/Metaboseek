#' TableUpdateChunk
#' 
#' 
#' Code chunk to update the active Feature Table before triggering a rerendering of the rhandsontable object.
#'  
#' Chunks are evaluated in their parent environment, and therefore require all objects they work on to be present under their correct names.
#' 
#' This chunk requires:
#' 
#' values$featureTables$Maintable - pointing to the MainTableObject
#' values$featureTables
#' 
#' 
TableUpdateChunk <- function(){
  eval.parent(quote({
    if(!is.null(values$featureTables) && values$featureTables$Maintable$hasUpdates){
      if((!is.null(values$featureTables$tables[[values$featureTables$active]]$editable) 
         && values$featureTables$tables[[values$featureTables$active]]$editable)
         || is.null(values$featureTables$Maintable$liveView$comments) ){
        values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$Maintable$liveView),colnames(values$featureTables$Maintable$liveView)] <- values$featureTables$Maintable$liveView
      }else{
        values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$featureTables$Maintable$liveView),"comments"] <- values$featureTables$Maintable$liveView$comments
      }
    }
    
    }))
}


