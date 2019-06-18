#' writeMS
#' 
#' write an .ms file containing a spectrum to be exported to SIRIUS.
#' will append additional spectrum to file if filename is that of an existing file.
#' 
#' @return Returns nothing, but writes a SIRIUS \code{.ms} file.
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
#' @importFrom digest digest
#' @importFrom data.table fwrite
#' 
writeMS <- function(filename,
                    ms2,
                    ms1 = NULL,
                    parentmz,
                    comments = "",
                    rt = "",
                    ion = NULL,
                    charge = 1,
                    splashtag = NULL,
                    scanindices = NULL){
  
  if(is.null(splashtag)){splashtag <-  digest(ms2, algo = "xxhash64")}
  
  appending <- file.exists(filename)
  
  write(paste(
    if(appending){"\n"}else{paste0('# Generated with MOSAiC version ',
                                   packageVersion("METABOseek"),', timestamp: ',
                                   strftime(Sys.time(),"%Y%m%d_%H%M%S"))},
    paste0(">compound ", splashtag),
    paste0(">parentmass ", parentmz),
    if(!is.null(ion)){paste0(">ion ", ion)}else{paste0(">charge ", charge)},
    if(!is.null(scanindices)){paste0(">scans ", scanindices)}else{""},
    paste0(">splash ", splashtag),
    paste0("# Comments: ", comments),
    paste0(">rt ", rt),
    sep =  "\n"),
    file = filename,
    append = appending
  )
  
  if(!is.null(ms1)){
    write(
      paste0(">ms1"),
      file = filename,
      append = T
    )
  fwrite(data.table(ms1), filename, append = T, sep = "\t",
         row.names = F, col.names = F, quote = T, eol = "\n")
  }
  
  if(!is.null(ms2)){
    
    if(!is.list(ms2)){ms2 <- list(ms2)}
    
    lapply(seq(length(ms2)), function(n){
    
      
      #expect collision energy to be in names, resulting in ">collision20", etc.
      write(
      if(is.null(names(ms2))){">ms2"}else{paste0(">",names(ms2)[n])},
      file = filename,
      append = T
    )
  
  fwrite(data.table(ms2[[n]]), filename, append = T, sep = "\t", 
         row.names = F, col.names = F, quote = T, eol = "\n")
    })
  }
  
  
  
}

#' runSirius
#' 
#' run SIRIUS externally for a list of ms2 spectra
#' 
#' @param outfolder output folder for the SIRIUS results
#' @param ms2 a list (or list of lists) of ms2 spectra 
#' (matrices with columns mz and intensity), see \code{Details}
#' @param parentmz numeric: parent ion m/z value
#' @param comments character: comments
#' @param rt numeric: retention time in seconds
#' @param ion ion type (e.g. \eqn{[M+H]+})
#' @param charge charge (positive or negative integer)
#' @param scanindices which scans were averaged into this ms2 spectrum
#' @param sirpath path to SIRIUS executable
#' @param moreOpts character with additional options to be passed to SIRIUS
#' @param force force calculation, even if same results should exist according to indexfile
#' 
#' @return Will cause SIRIUS to run a job and save its results in
#'  a timestamped subfolder of  \code{outfolder}, and will register the job in
#'  the \code{index.csv} file in \code{outfolder}.
#' 
#' @details
#' \describe{
#'    \item{ms2}{can be a list of matrices, or a list of list of matrices. 
#'    In the latter case, the (optional) names of the nested list items are 
#'    expected to be in the format "collisionXX", where XX denotes an integer 
#'    collision energy.}
#' }
#' 
#' @importFrom data.table fread fwrite
#' @importFrom digest digest
#' 
#' @export
runSirius <- function(outfolder,
                      ms1 = NULL,
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
                       moreOpts = "",
                      force = T){

  
  #write the MS2 data so sirius can read it
  dir.create(outfolder, showWarnings = F)
  #splashtag <- lapply(ms2, getSplash)
  splashtag <- lapply(ms2, digest, algo = "xxhash64")
  
  
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
   newjobs$ms1splash = sapply(ms1, function(x){if(is.null(x)){""}else{digest(x,algo = "xxhash64")}})
   newjobs$charge = charge
   newjobs$fingerid = fingerid
   newjobs$moreOpts = paste0(moreOpts, instrument)
   newjobs$METABOseek_version = as.character(packageVersion("METABOseek"))
   newjobs$METABOseek_sirius_revision = 2
   newjobs$settingsHash <- apply(newjobs[,c("ion", "ms1splash", "charge",
                                            "fingerid", "moreOpts",
                                            "METABOseek_sirius_revision"),
                                         drop = F], 1, digest,
                                 algo = "xxhash64")
   
  
   indexfile <- file.path(outfolder, "index.csv")
  if(file.exists(indexfile) && !force){
    
    jobindex <- as.data.frame(data.table::fread(indexfile, na.strings = NULL))
    # for all columns that are of type logical and only contain NAs, assume they are mutilated empty character strings
    # and a victim of type.convert - make them character vectors again
    charCols <- sapply(jobindex,typeof) == "logical" & sapply(lapply(jobindex,is.na),all)
    if(any(charCols)){
      jobindex[,charCols] <- character(nrow(jobindex))
    }
    
    checkme <- as.data.frame(rbindlist(list(old = jobindex, 
                                            new = newjobs),
                                       fill = T))[,c("mz", "splash",
                                                     "ion", "charge", 
                                                     "fingerid", "moreOpts",
                                                     "METABOseek_sirius_revision")]
    
    #To avoid problems with m/z digits possibly not retained when 
    #exporting and reimporting of siriusIndex to/from index.csv
    checkme$mz <- round(checkme$mz, 4)
    
    dups <- duplicated.data.frame(checkme)[seq(nrow(jobindex)+1,nrow(checkme))]
    
    newjobs <- newjobs[!dups,]

  }
  
   if(nrow(newjobs) > 0){
   
  filename <- file.path(outfolder,paste0(ts,".ms"))
  

  np <- mapply(writeMS,
               filename = filename,
               ms1 = ms1,
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
         append = T, sep = ",", row.names = F,
         col.names = F, quote = T, eol = "\n")
  
  }else{
    
    fwrite(newjobs,
           indexfile,
           append = F, sep = ",", row.names = F,
           col.names = T, quote = T, eol = "\n")
    
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
#' @return Returns a list with information for a completed SIRIUS job
#' that matches \code{splash} and \code{ts} in \code{outfolder}
#' 
#' @details Finds a result folder matching the search terms and reads 
#' its contents.
#' \itemize{
#' \item \code{splash} copy of \code{splash}
#' \item \code{timestamp} copy of \code{ts}
#' \item \code{allfiles} character vector with all files in \code{outfolder}
#' \item \code{summary} path of the \code{summary_sirius.csv} file
#' \item \code{summary_expanded} data.frame read in from the 
#' \code{summary_sirius.csv} file
#' \item \code{annotations} list of all files in the \code{spectra} 
#' subfolder of the result folder
#' \item \code{trees_dot} list of all fragmentation tree \code{.dot} files
#' \item \code{trees_json} list of all fragmentation tree \code{.json} files
#' \item \code{summary_fingerid} path of the \code{summary_csi_fingerid.csv} 
#' file
#' \item \code{summary_fingerid_expanded} data.frame read in from the 
#' \code{summary_csi_fingerid.csv} file
#' \item \code{fingerids} list of all files in the \code{csi_fingerid} 
#' subfolder of the result folder
#' \item \code{fingerprints} list of all files in the \code{fingerprints} 
#' subfolder of the result folder
#' }
#' 
#' @export
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
  
  res[["summary"]] <-  grep(paste0("summary_sirius.csv"),
                            res[["allfiles"]],
                            value = T)
  
  res[["summary_expanded"]] <-  as.data.frame(fread(res[["summary"]],
                                           stringsAsFactors = F),
                                           stringsAsFactors = F)
  
  res[["annotations"]] <-  list.files(file.path(dirname(res[["summary"]]),
                                                "spectra"),
                                      full.names = T, recursive = T)
  
  res[["trees_dot"]] <-  grep(".dot$",
                              list.files(file.path(dirname(res[["summary"]]),
                                                   "trees"),
                                         full.names = T, recursive = T),
                              value = T)
  
  res[["trees_json"]] <-  grep(".json$",
                               list.files(file.path(dirname(res[["summary"]]),
                                                    "trees"),
                                          full.names = T, recursive = T),
                               value = T)
  
  res[["summary_fingerid"]] <-  grep(paste0("summary_csi_fingerid.csv"),
                                     res[["allfiles"]],value = T)
  
  res[["summary_fingerid_expanded"]] <-  as.data.frame(fread(res[["summary_fingerid"]],
                                                    stringsAsFactors = F), 
                                                    stringsAsFactors = F)
  
  
  res[["fingerids"]] <-  list.files(file.path(dirname(res[["summary"]]),
                                              "csi_fingerid"),
                                    full.names = T, recursive = T)
  res[["fingerprints"]] <-  list.files(file.path(dirname(res[["summary"]]),
                                                 "fingerprints"),
                                       full.names = T, recursive = T)
  
  
  
  return(res)
  
}

#' getSiriusTree
#' 
#' get additional SIRIUS results (fragmentation trees) for a spectrum
#' 
#' @param paths list generated by \code{\link{getSirius}()}
#' @param formula molecular formula prediction to get details for
#' 
#' @return Returns a named list with fragmentation tree objects for plotting,
#'  see |code{Details}
#' 
#' @details
#' \itemize{
#' \item \code{annotations} the matching file from \code{spectra} subfolder 
#' as data.frame
#' \item \code{trees_dot} the matching \code{.dot} tree file, read in by 
#' \code{DiagrammeR::grViz}
#' \item \code{trees_json} the matching \code{.json} tree file, read in by 
#' \code{jsonlite::read_json}
#' }
#' 
#' @importFrom data.table fread
#' @importFrom jsonlite read_json
#' 
getSiriusTree <- function(paths, formula){
  
  res <- list()
  
  res[["annotations"]] <-  as.data.frame(fread(grep(paste0("_",formula,"_"),
                                           paths[["annotations"]],
                                           value = T), stringsAsFactors = F),
                                         stringsAsFactors = F)
  
  res[["trees_dot"]] <- DiagrammeR::grViz(grep(paste0("_",formula,"_"),
                                               paths[["trees_dot"]],
                                               value = T))
  
  res[["trees_json"]] <-   jsonlite::read_json(grep(paste0("_",formula,"_"),
                                          paths[["trees_json"]],
                                          value = T))
  
  return(res)
  
}

