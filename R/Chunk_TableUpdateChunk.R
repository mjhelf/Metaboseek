#' TableUpdateChunk
#' 
#' 
#' Code chunk to update the active Feature Table before triggering a rerendering of the rhandsontable object.
#'  
#' Chunks are evaluated in their parent environment, and therefore require all objects they work on to be present under their correct names.
#' 
#' This chunk requires:
#' 
#' values$MainTable - pointing to the MainTableObject
#' values$featureTables
#' 
#' 
TableUpdateChunk <- function(){
  eval.parent(quote({
    if(!is.null(values$featureTables) && values$MainTable$hasUpdates){
      if((!is.null(values$featureTables$tables[[values$featureTables$active]]$editable) 
         && values$featureTables$tables[[values$featureTables$active]]$editable)
         || is.null(values$MainTable$liveView$comments) ){
        values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$MainTable$liveView),colnames(values$MainTable$liveView)] <- values$MainTable$liveView
      }else{
        values$featureTables$tables[[values$featureTables$active]]$df[row.names(values$MainTable$liveView),"comments"] <- values$MainTable$liveView$comments
      }
    }
    
    }))
}


