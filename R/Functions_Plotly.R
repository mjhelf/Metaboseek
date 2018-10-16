#' PlotBrowserModule
#' 
#' 
#' prepare text to be shown in plotly through aes(text)
#' 
#' @param df data.frame
#' @param cols character() of column names to display
#' 
plotlyTextFormatter <- function(df, cols){
  
  collect <- list()
  
  for(i in cols){
    collect[[i]] <- paste(i, df[[i]], sep = ": ")
  }
  
  collect2 <- collect[[1]]
  
  if(length(collect) >1){
    for (i in 2:length(collect)){
      collect2 <- paste(collect2, collect[[i]], sep = "\n")
    }
  }
  return(collect2)
  
}