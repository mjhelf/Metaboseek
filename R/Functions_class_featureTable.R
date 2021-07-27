#' constructFeatureTable
#' 
#' 
#' Constructor function for "MseekFT" object from a feature table data.frame.
#' uses and retains the original data frame plus names of columns containing 
#' relevant data. 
#' 
#' NOTE: currently, only the default values for column names are supported.
#' 
#' @param df feature table as data.frame, with retention time (rt), mz and 
#' (optionally) intensity values.
#' @param mzcol column in df with mz values (columnname), defaults to "mz"
#' @param rtcol column in df with rt values (columnname), defaults to "rt"
#' @param commentcol column in df with comments (columnname), defaults to "comments"
#' @param fragmentcol column in df with fragmentation information (columnname), 
#' defaults to "fragments"
#' @param rtFormat Are retention times given in seconds ("sec") or minutes ("min")
#' @param anagrouptable Analysis grouping table: a data.frame with columns 
#' "Column" (containing column names from df with intensity values) and "Group" 
#' (defining a group for each entry in "Column") 
#' @param tablename Name of the table as displayed by Mseek
#' @param editable allow editing of this table in the Mseek app? if FALSE, only
#'  comments column can be edited. editable tables are also not paginated.
#' @param processHistory a list of \code{\link[xcms]{processHistory}} objects
#' 
#' @return an \code{MseekFT} object containing a feature table and metadata
#' 
#' @export
constructFeatureTable <- function(df= data.frame(mz=numeric(3), rt = numeric(3)),# data frame 
                          mzcol= "mz", #
                          rtcol= "rt", #column in df with mz values (columnname)
                          commentcol = "comments",
                          fragmentcol = "fragments",
                          rtFormat = "sec", # "sec" or "min" 
                          anagrouptable = NULL,
                          tablename = "Custom Table",
                          editable = T,
                          processHistory = list()){ #T: free editing (add rows), but always see all columns in viewer, F: only comments can be edited directly, no adding of columns
    
    #make columns if they don't exist:
    if (class(try(df[,mzcol], silent = T))=="try-error"){ df[,mzcol] <- numeric(nrow(df)) }
    if (class(try(df[,rtcol], silent = T))=="try-error"){ df[,rtcol] <- numeric(nrow(df)) }
    if (class(try(df[,commentcol], silent = T))=="try-error"){ df[,commentcol] <- character(nrow(df)) }
    if (class(try(df[,fragmentcol], silent = T))=="try-error"){ df[,fragmentcol] <- character(nrow(df)) }
    
    FT = list()
    #save the entire dataframe 
    FT$df = df 
    
    if(!"rt_minutes" %in% colnames(FT$df)){
    if(rtFormat == "sec"){
      FT$df$rt_minutes <- df[,rtcol]/60
    }else{
      FT$df$rt_minutes <- df[,rtcol]
      df[,rtcol] <- df[,rtcol]*60
    }
    }
    
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
                                              minSel = if(is.numeric(df[,colnames(df)[1]])){min(df[,colnames(df)[1]])}else{NULL},
                                              maxSel = if(is.numeric(df[,colnames(df)[1]])){max(df[,colnames(df)[1]])}else{NULL},
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
    
    FT$.processHistory <- c(processHistory)
    
    class(FT) <- "MseekFT"
    
    return(FT)

}

#' updateDF
#' 
#' Update dataframe b with data from dataframe a; must have equal number of rows
#' Existing data in b will be overridden if columns by the same name exist in a,
#'  and retained otherwise.
#' Additional columns in a will be transferred to b. 
#' 
#' @param a source data.frame
#' @param b target data.frame
#' 
#' @return Returns the updated data.frame b
#' 
#' @export
updateDF <- function(a, b){

        for (i in colnames(a)){
             b[,i] <- a[,i, drop = FALSE]} #drop = FALSE added to avoid issues with list columns
        
        return(b)
    }
    
#' .getFTFormat
#' 
#' Tries to identify the format of a feature table input data frame
#' 
#' @param df data.frame
#' 
#' @return Returns the format of the df object. Can be either of these:
#' \itemize{
#'  \item "xcms PeakTable with Metaboseek Intensities"
#'  \item "xcms Peak Table"
#'  \item "mzMINE Aligned Peak List"
#'  \item "unknown"
#' }
#' 
#' 
.getFTFormat <- function(df){
  
  try({
    if(all(c("rt", "mz") %in% colnames(df))
       & any(grepl("\\.[Cc][Dd][Ff]__XIC$|\\.[Nn][Cc]__XIC$|\\.([Mm][Zz])?[Xx][Mm][Ll]__XIC$|\\.[Mm][Zz][Dd][Aa][Tt][Aa]__XIC$|\\.[Mm][Zz][Mm][Ll]__XIC$",
                   colnames(df)))){
      
      return("xcms PeakTable with Metaboseek Intensities")
      
    }
  }, silent = TRUE)
  
  try({
    if(all(c("rt", "mz") %in% colnames(df))
       & any(grepl("\\.[Cc][Dd][Ff]$|\\.[Nn][Cc]$|\\.([Mm][Zz])?[Xx][Mm][Ll]$|\\.[Mm][Zz][Dd][Aa][Tt][Aa]$|\\.[Mm][Zz][Mm][Ll]$",
                   colnames(df)))){
      
      return("xcms Peak Table")
      
    }
  }, silent = TRUE)
  
  try({
    if(all(c("row m/z", "row retention time") %in% colnames(df))
       & any(grepl("Peak area$",
                   colnames(df)))){
      
      return("mzMINE Aligned Peak List")
      
    }
  }, silent = TRUE)
  
  try({
    if(all(c("Sample") %in% colnames(df))
       && all(is.character(unlist(df[1,])) | is.factor(unlist(df[1,])))
       && all(grepl("\\/",
                    df[-1,1]))
       && all(sapply(df[-1,which(!is.na(unlist(df[1,])))[-1]], function(x){is.numeric(type.convert(x))}))
    ){
      
      return("MetaboAnalyst Peak Intensity Table")
      
    }
  }, silent = TRUE)
  
  return("unknown")
  
}

#' .reformatFeatureTable
#' 
#' Tries to identify the format of a feature table input data frame and 
#' reformats it for compatibility with \code{constructFeatureTable}
#' 
#' @param df data.frame
#' @param from input format. If auto, will run \code{.getFTFormat} to determine
#' the format.
#' 
#' @return a list
#' \itemize{
#'  \item df: reformatted input data.frame
#'  \item grouping: anagrouptable format (if grouping information can be 
#'  extracted from input) or NULL
#'  \item from: which input format was used
#' }
#' 
#' 
.reformatFeatureTable <- function(df,
                                  from = c("auto",
                                           "unknown", 
                                           "mzMINE Aligned Peak List",
                                           "MetaboAnalyst Peak Intensity Table")){
  
  from <- from[1]
  
  if(from == "auto"){from <- .getFTFormat(df)}
  
  res <- list(df = df,
              grouping = NULL,
              from = from)
  rm(df) # to save memory
  
  switch(from,
         "unknown" = {},
         
         "mzMINE Aligned Peak List" = {
           
           res$grouping = data.frame(Column = grep("Peak area$",
                                                   colnames(res$df),
                                                   value = TRUE),
                                     Group = "G1",
                                     stringsAsFactors = FALSE)

           res$df$mz <- res$df[["row m/z"]]
           res$df$rt <- res$df[["row retention time"]]*60
           
          #moving all intensity columns to the end
           res$df <- res$df[,c(colnames(res$df)[!grepl("Peak area$",
                                                         colnames(res$df))],
                                 colnames(res$df)[grepl("Peak area$",
                                                         colnames(res$df))])]
                                          
           
           
          
         },
         
         "MetaboAnalyst Peak Intensity Table" = {
           
           res$grouping = data.frame(Column = colnames(res$df)[-1],
                                     Group = unlist(res$df[1,])[-1],
                                     stringsAsFactors = FALSE)
           
           res$df <- data.frame(lapply(res$df[-1,], type.convert, as.is = TRUE),
                                stringsAsFactors = FALSE,
                                check.names = FALSE)
           
           sp <- strsplit(as.character(res$df[,1]), split = '\\/')
           
           res$df$mz <- as.numeric(sapply(sp,'[',1))
           res$df$rt <- as.numeric(sapply(sp,'[',2))
           
         }
         )
  
  return(res)
  
}


#' updateFeatureTable
#' 
#' update an existing MseekFT with new data.frame columns (leaving intensitiy
#'  column names and constants in effect, but updating information on which 
#'  "other" columns are present)
#' 
#' @param FT MseekFT object
#' @param df source data.frame
#' 
#' @return An updated \code{MseekFT} object
#' 
#' @export
updateFeatureTable <- function(FT, df){
    
    FT$df = updateDF(df,FT$df)
    #if(!is.null(FT$anagrouptable)){
        FT <- updateFTgrouping(FT,FT$anagrouptable)
   # }
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


#' updateFTgrouping
#' 
#' update or construct grouping information of a MseekFT object
#' 
#' @param FT MseekFT object
#' @param anagrouptable Analysis grouping table: a data.frame with columns 
#' "Column" (containing column names from df with intensity values) and 
#' "Group" (defining a group for each entry in "Column") 
#' 
#' @return An updated \code{MseekFT} object
#' 
#' @export
updateFTgrouping <- function(FT,anagrouptable){
    
    FT$anagrouptable = anagrouptable
    
    if(!is.null(FT$anagrouptable) && nrow(FT$anagrouptable) >0 ){
    newgrouping <- tableGrouping(FT$df,FT$anagrouptable)
    
    FT$anagroupnames = newgrouping$anagroupnames
    FT$intensities = unname(unlist(FT$anagroupnames))
    FT$anagroupnums = newgrouping$anagroupnums

    if(length(grep("__norm",colnames(FT$df)))>0){
        FT$anagroupnames_norm = relist(paste0(unlist(FT$anagroupnames),
                                              "__norm"),
                                       FT$anagroupnames)
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
        selCols <- unlist(sapply(paste0(FT$sNames[[i]],"__"),
                                 grep,
                                 colnames(FT$df)[which(!colnames(FT$df) %in% c(FT$intensities,FT$intensities_norm))], value = T))
        if(length(selCols)>0){
            FT$sProps[[i]] <- sort(selCols)}
    }
    }
    else{
      FT$anagroupnames = NULL
      FT$intensities = NULL
      FT$anagroupnums = NULL
      FT$anagroupnames_norm = NULL
      FT$intensities_norm = NULL
      FT$gNames = NULL
      FT$sNames = NULL
      FT$gProps = NULL
      FT$sProps = NULL
      
    }
    
    
    return(FT)
}


#' updateFTgrouping
#' 
#' update the FT index of the Mseek App (needed because this index is a named 
#' list - extracts tablename entry from each MseekFT object in list)
#' 
#' @param tables named list of MseekFT objects 
#' 
#' @return named list of MseekFT tablenames
#' 
updateFTIndex <- function(tables){
    out <- names(tables)
    
    names(out) <- unname(sapply(tables, "[[", "tablename"))

    return(out)
}


#' tableGrouping
#' 
#' Groups a anagrouptable into a named list. Helper function to
#'  reformat Table analysis grouping tables
#' 
#' @param df feature table as data.frame, with intensity values.
#' @param anagrouptable Analysis grouping table: a data.frame with columns 
#' \code{Column} (containing column names from df with intensity values) and 
#' \code{Group} (defining a group for each entry in "Column") 
#' 
#' @return A named list, see \code{details}
#' 
#' @details
#'  Will return a named list with two list items
#'  \itemize{
#'  \item\code{anagroupnames} named list of intensity column names
#' \item\code{anagroupnums} named list of intensity column numbers
#' }
#' 
tableGrouping <- function(df=NULL, anagrouptable){
  ## Make list object of grouped column names
  if(is.null(anagrouptable$Column)){
    ColumnNames <- gsub("-",".",paste0(basename(anagrouptable$File),"__XIC"))
    ColumnNames[which(substring(ColumnNames,1,1) %in% as.character(0:9))] <- paste0("X",ColumnNames[which(substring(ColumnNames,1,1) %in% as.character(0:9))])
    anagrouptable$Column <- ColumnNames
  }
  
  colme <- list()
  for (l in unique(anagrouptable$Group)){
    colme[[l]] <- as.character(anagrouptable$Column[which(anagrouptable$Group==l)])
  }
  anagroupnames <- colme
  ### Get column numbers from column names
  if(!is.null(df)){
    
    colnu <- integer(0)
    for (i in anagrouptable$Column){
      colnu<- c(colnu,which(colnames(df) == i))
    }
    ## Make list object of grouped column numbers
    colme <- list()
    for (l in unique(anagrouptable$Group)){
      colme[[l]] <- as.integer(colnu[which(anagrouptable$Group==l)])
    }
    anagroupnums <- colme
    return(list(anagroupnames = anagroupnames,anagroupnums = anagroupnums))}
  else{
    return(list(anagroupnames = anagroupnames,anagroupnums = NULL))
  }
}


#' tableWriter
#' 
#' Writes a feature table to a file in a given format.
#' 
#' @param df feature table as data.frame, with mz and rt columns.
#' @param fname file name
#' @param format output format
#' @param moreArgs named list of additional arguments to be 
#' passed to \code{\link{asInclusionlist}} or \code{\link{asMetaboAnalyst}}
#' 
#' @importFrom data.table fwrite
#' 
#' @return Returns nothing, but writes a file to disk
#' 
#' @examples 
#' \dontrun{
#' MseekExamplePreload(data = FALSE, tables = TRUE)
#' tableWriter(tab1$df, "test_MA.csv", format = "MetaboAnalyst", moreArgs = list(groups = tab1$anagrouptable))
#' }
#' 
#' @export
tableWriter <-function(df, fname, format = c("csv", "tsv",
                                             "instrumentList",
                                             "MetaboAnalyst"),
                       moreArgs = list()){
  
  if(!length(format) || is.na(format[1])){
    warning("File format selected was empty. Could not export data to file")
   return(invisible(NULL)) 
  }
  
  tryCatch({
  
  if(format[1] %in% c("csv","tsv")){
  
  switch(format[1],
         
         csv = {sep <- ","},
           tsv = {sep <- "\t"})
  
  #don't save complex columns
  fwrite(df[,sapply(df,is.atomic)],
         fname,
         sep = sep,
         quote = T,
         row.names = F
  )
  return(invisible(NULL))
  }
  
  if(format[1] %in% c("instrumentList")){
    
      moreArgs <- moreArgs[names(moreArgs) %in% c("rtwin", "polarity","instrument", "listType", "restrictRT")]
    
    df <- do.call(asInclusionlist, c(list(parenttable = df[,sapply(df,is.atomic)]),
                                     moreArgs))
    
    fwrite(df,
           fname,
           sep =  "\t",
           quote = F,
           row.names = F
    )
  
    
    return(fname)
  }
    
    if(format[1] %in% c("MetaboAnalyst")){
    
      
      ma <- do.call(asMetaboAnalyst, c(list(df = df),
                                       moreArgs))
      
      fwrite(ma$header,
             fname,
             sep =  ",",
             quote = TRUE,
             row.names = FALSE
      )
      
      fwrite(ma$body,
             fname,
             append = TRUE, #this also means colnames/header will not be written again
             sep =  ",",
             quote = TRUE,
             row.names = FALSE
      )
      
      return(invisible(NULL))
      
      }
    
  
  warning("Invalid file format selected. Could not export data to file")
  },
  error = function(e){
    warning(paste0("An error occured while exporting a table: ", e))
    })
    
}

#' asInclusionlist
#' 
#' Reformats a feature table into a format that can be imported 
#' into MS instruments as inclusion or exclusion lists.
#' 
#' @param parenttable feature table as data.frame, with mz and rt columns.
#' @param rtwin retention time window (+/- df$rt) in seconds
#' @param polarity "Positive" or "Negative" 
#' @param instrument which instrument is this table for?
#' @param listType "inclusion" or "exclusion"
#' @param restrictRT use retention time column from parenttable and rtwin to 
#' restrict retention time in inclusion/exclusion list
#' 
#' @return A data.frame that, when written to a file, has the expected format for a given instrument
#' 
#'
#' 
#' @export
asInclusionlist <- function(parenttable,
                            rtwin=2.5,
                            polarity = "Positive",
                            instrument = c("QExactive", "QExactive-HF"),
                            listType = "inclusion",
                            restrictRT = F){
  if(listType == "exclusion"){
    out <- data.frame("Mass [m/z]" = parenttable$mz,
                      "Formula [M]" = character(length(parenttable$mz)),
                      "Formula type" = character(length(parenttable$mz)),
                      Species = character(length(parenttable$mz)),
                      "CS [z]" = character(length(parenttable$mz)),
                      Polarity = as.character(rep(polarity,
                                                  length(parenttable$mz))),
                      "Start [min]" = if(!restrictRT){
                          character(length(parenttable$mz))
                          }else{as.numeric(round((parenttable$rt-rtwin)/60,3))},
                      "End [min]" = if(!restrictRT){
                          character(length(parenttable$mz))
                          }else{as.numeric(round((parenttable$rt+rtwin)/60,3))},
                      Comment = as.character(parenttable$comments),
                      stringsAsFactors = F)
  colnames(out) <- c("Mass [m/z]","Formula [M]", "Formula type", "Species",
                     "CS [z]", "Polarity", "Start [min]",
                     "End [min]", "Comment")
  }else{
  out <- data.frame("Mass [m/z]" = parenttable$mz,
                    "Formula [M]" = character(length(parenttable$mz)),
                    "Formula type" = character(length(parenttable$mz)),
                    Species = character(length(parenttable$mz)),
                    "CS [z]" = character(length(parenttable$mz)),
                    Polarity = as.character(rep(polarity,
                                                length(parenttable$mz))),
                    "Start [min]" = if(!restrictRT){
                        character(length(parenttable$mz))
                        }else{as.numeric(round((parenttable$rt-rtwin)/60,3))},
                    "End [min]" = if(!restrictRT){
                        character(length(parenttable$mz))
                        }else{as.numeric(round((parenttable$rt+rtwin)/60,3))},
                    "(N)CE" = character(length(parenttable$mz)),
                    "(N)CE type" = character(length(parenttable$mz)),
                    "MSX ID" = character(length(parenttable$mz)),
                    Comment = as.character(parenttable$comments),
                    stringsAsFactors = F)
  colnames(out) <- c("Mass [m/z]","Formula [M]", "Formula type", 
                     "Species", "CS [z]", "Polarity",
                     "Start [min]", "End [min]", "(N)CE", "(N)CE type",
                     "MSX ID", "Comment")
  }
  
  return(out) 
}

#' asMetaboAnalyst
#' 
#' Reformat a data.frame to MetaboAnalyst csv format
#'
#' @examples 
#' \dontrun{
#' MseekExamplePreload(data = FALSE, tables = TRUE)
#' asMetaboAnalyst(tab1$df, groups = tab1$anagrouptable)
#' }
#'
#' @param df data.frame with intensity values and rt and mz columns
#' @param groups a data.frame with grouping information (Sample names in first column, groups in additional columns)
#' 
#' @return A list with header and body of the MetaboAnalyst formatted table
#'
asMetaboAnalyst <- function(df, groups){
  
  body <- data.frame(Sample = paste0(df$mz,"/",df$rt),
             df[,groups[,1]],
             stringsAsFactors = FALSE)
  
  header <- as.data.frame(t(groups), stringsAsFactors = FALSE)
  colnames(header) <- groups[,1]
  header <- cbind(data.frame(Sample = colnames(groups[-1])),
        header[-1,])
  
  list(header = header,
       body = body)
}
