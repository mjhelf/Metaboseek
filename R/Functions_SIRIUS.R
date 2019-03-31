#' writeMS
#' 
#' write an .ms file containing a spectrum to be exported to SIRIUS.
#' will append additional spectrum to file if filename is that of an existing file.
#' 
#' @param filename path of file to write to
#' @param ms2 a matrix with columns mz and intensity
#' @param parentmz numeric: parent ion m/z value
#' @param comments character: comments
#' @param rt numeric: retention time in seconds
#' @param ion ion type (e.g. \eqn{[M+H]+})
#' @param charge charge (positive or negative integer)
#' @param splashtag splash tag of the ms2 spectrum
#' @param scanindices which scans were averaged into this ms2 spectrum
#' 
#' @importFrom splashR getSplash
#' 
#' @importFrom data.table fwrite
#' 
writeMS <- function(filename,
                    ms2,
                    parentmz,
                    comments = "",
                    rt = "",
                    ion = NULL,
                    charge = 1,
                    splashtag = NULL,
                    scanindices = NULL){
  
  if(is.null(splashtag)){splashtag <- getSplash(ms2)}
  
  appending <- file.exists(filename)
  
  write(paste(
    if(appending){"\n"}else{paste0('# Generated with MOSAiC version ',
                                   packageVersion("METABOseek"),', timestamp: ', strftime(Sys.time(),"%Y%m%d_%H%M%S"))},
    paste0(">compound ", splashtag),
    paste0(">parentmass ", parentmz),
    if(!is.null(ion)){paste0(">ion ", ion)}else{paste0(">charge ", charge)},
    if(!is.null(scanindices)){paste0(">scans ", scanindices)}else{""},
    paste0(">splash ", splashtag),
    paste0("# Comments: ", comments),
    paste0(">rt ", rt),
    paste0(">ms2"),
    sep =  "\n"),
    file = filename,
    append = appending
  )
  fwrite(data.table(ms2), filename, append = T, sep = "\t", row.names = F, col.names = F, quote = T, eol = "\n")
  
  
  
  
}

#' runSirius
#' 
#' run SIRIUS externally for a list of ms2 spectra
#' 
#' @param outfolder output folder for the SIRIUS results
#' @param ms2 a list of ms2 spectra (matrices with columns mz and intensity)
#' @param parentmz numeric: parent ion m/z value
#' @param comments character: comments
#' @param rt numeric: retention time in seconds
#' @param ion ion type (e.g. \eqn{[M+H]+})
#' @param charge charge (positive or negative integer)
#' @param scanindices which scans were averaged into this ms2 spectrum
#' @param sirpath path to SIRIUS executable
#' @param moreOpts character with additional options to be passed to SIRIUS
#' 
#' @importFrom splashR getSplash
#' @importFrom data.table fread fwrite
#' 
runSirius <- function(outfolder,
                       ms2,
                       instrument,
                       parentmz,
                       comments = "",
                       rt = "",
                       ion,
                       charge= 1,
                       fingerid = T,
                       scanindices = NULL,
                       sirpath,
                       moreOpts = ""){

  
  #write the MS2 data so sirius can read it
  dir.create(outfolder, showWarnings = F)
  splashtag <- lapply(ms2, getSplash)
  
  ts <- strftime(Sys.time(),"%y%m%d_%H%M%S")
  
  #make a data.frame with information on what will be analyzed here
   newjobs <- data.frame(mz = parentmz,
                         stringsAsFactors = F)
   
   instrument <- paste0(" -p ",
                        instrument)
   
   newjobs$rt = rt
   newjobs$splash = unlist(splashtag)
   newjobs$timestamp = ts
   newjobs$ion = ion
   newjobs$charge = charge
   newjobs$fingerid = fingerid
   newjobs$moreOpts = paste0(moreOpts, instrument)
   newjobs$METABOseek_version = as.character(packageVersion("METABOseek"))
   newjobs$METABOseek_sirius_revision = 1
   
   indexfile <- file.path(outfolder, "index.csv")
  if(file.exists(indexfile)){
    
    jobindex <- as.data.frame(data.table::fread(indexfile, na.strings = NULL))
    # for all columns that are of type logical and only contain NAs, assume they are mutilated empty character strings
    # and a victim of type.convert - make them character vectors again
    charCols <- sapply(jobindex,typeof) == "logical" & sapply(lapply(jobindex,is.na),all)
    if(any(charCols)){
      jobindex[,charCols] <- character(nrow(jobindex))
    }
    
    checkme <- as.data.frame(rbindlist(list(old = jobindex, new = newjobs), fill = T))[,c("mz", "splash", "ion", "charge", "fingerid", "moreOpts", "METABOseek_sirius_revision")]
    
    #To avoid problems with m/z digits possibly not retained when exporting and reimporting of siriusIndex to/from index.csv
    checkme$mz <- round(checkme$mz, 4)
    
    dups <- duplicated.data.frame(checkme)[seq(nrow(jobindex)+1,nrow(checkme))]
    
   # print(newjobs)
    newjobs <- newjobs[!dups,]
   # print(newjobs)
    
  }
  
   if(nrow(newjobs) > 0){
   
  filename <- file.path(outfolder,paste0(ts,".ms"))
  

  np <- mapply(writeMS,
               filename = filename,
               ms2 = ms2,
               parentmz = parentmz,
               scanindices = scanindices,
               comments = comments,
               rt = rt,
               splashtag = splashtag,
               ion = ion,
               charge = charge)
  
  
  outfolder <- file.path(outfolder,strftime(Sys.time(),"%y%m%d_%H%M%S"))
  
  
  instrument <- paste0(" -p ",
                       instrument)
  
  finger <- if(fingerid){" --fingerid "}else{" "}
  
  system(paste0(
    sirpath,
    " -o ",
    '"',
    outfolder,
    '"',
    instrument,
    finger,
    moreOpts,
    ' "',
    filename,
    '"'),
    intern = F, wait = F)
  
  if(file.exists(indexfile)){
    
  fwrite(newjobs,
         indexfile,
         append = T, sep = ",", row.names = F, col.names = F, quote = T, eol = "\n")
  
  }else{
    
    fwrite(newjobs,
           indexfile,
           append = F, sep = ",", row.names = F, col.names = T, quote = T, eol = "\n")
    
  }
   }
}



#' getSirius
#' 
#' get SIRIUS results for a given spectrum
#' 
#' @param outfolder folder with SIRIUS results
#' @param splash splash tag to look for
#' @param ts timestamp to look for
#' 
#' @importFrom data.table fread
#' 
getSirius <- function(outfolder, splash, ts){
  
  res <- list()
  
  res[["splash"]] <- splash
  res[["timestamp"]] <- ts
  
  targetfolder <- grep(paste0(splash),
                       list.dirs(grep(paste0(ts),
                                      list.dirs(file.path(outfolder),
                                                full.names = T, 
                                                recursive = F),
                                      value = T), 
                                 full.names = T,
                                 recursive = F),
                       value = T)
  
  res[["allfiles"]] <- list.files(targetfolder, full.names = T, recursive = T)
  
  res[["summary"]] <-  grep(paste0("summary_sirius.csv"),res[["allfiles"]],value = T)
  
  res[["summary_expanded"]] <-  as.data.frame(fread(res[["summary"]],
                                           stringsAsFactors = F), stringsAsFactors = F)
  
  res[["annotations"]] <-  list.files(file.path(dirname(res[["summary"]]), "spectra"), full.names = T, recursive = T)
  
  res[["trees_dot"]] <-  grep(".dot$",
                              list.files(file.path(dirname(res[["summary"]]), "trees"),
                                         full.names = T, recursive = T),
                              value = T)
  
  # res[["trees_json"]] <-  grep(".json$",
  #                              list.files(file.path(dirname(res[["summary"]]), "trees"),
  #                                         full.names = T, recursive = T),
  #                              value = T)
  
  res[["summary_fingerid"]] <-  grep(paste0("summary_csi_fingerid.csv"),res[["allfiles"]],value = T)
  
  res[["summary_fingerid_expanded"]] <-  as.data.frame(fread(res[["summary_fingerid"]],
                                                    stringsAsFactors = F), stringsAsFactors = F)
  
  
  res[["fingerids"]] <-  list.files(file.path(dirname(res[["summary"]]), "csi_fingerid"), full.names = T, recursive = T)
  res[["fingerprints"]] <-  list.files(file.path(dirname(res[["summary"]]), "fingerprints"), full.names = T, recursive = T)
  
  
  
  return(res)
  
}

#' getSiriusTree
#' 
#' get additional SIRIUS results for a spectrum
#' 
#' @param paths list generated by getSirius()
#' @param formula molecular formula prediction to get details for
#' 
#' @importFrom data.table fread
#' 
getSiriusTree <- function(paths, formula){
  
  res <- list()
  
  # res[["summary"]] <-  paths[["summary_expanded"]][which(paths[["summary_expanded"]]$formula == formula),]
  # 
  res[["annotations"]] <-  as.data.frame(fread(grep(paste0("_",formula,"_"),
                                           paths[["annotations"]],
                                           value = T), stringsAsFactors = F), stringsAsFactors = F)
  
  res[["trees_dot"]] <- DiagrammeR::grViz(grep(paste0("_",formula,"_"),
                                               paths[["trees_dot"]],
                                               value = T))
  
  # res[["trees_json"]] <-   read_json(grep(paste0("_",formula,"_"),
  #                                         paths[["trees_json"]],
  #                                         value = T))
  # 
  # res[["fingerids"]] <-  read.delim(grep(paste0("_",formula,"_"),
  #                                        paths[["fingerids"]],
  #                                        value = T), stringsAsFactors = F)
  # 
  # res[["fingerprints"]] <-  read.delim(grep(paste0("_",formula,"_"),
  #                                           paths[["fingerprints"]],
  #                                           value = T), stringsAsFactors = F)
  return(res)
  
}

