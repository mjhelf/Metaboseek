#' writeStatus
#' 
#' Write or update a status dataframe with custom messages, time stamp and 
#' time elapsed since last status. Useful to document time consumed by analysis 
#' steps.
#' 
#' @param previous a \code{writeStatus} object to be updated (must have same 
#' column names):  new object generated if NULL.
#' @param message named list of columns with elements \code{Status} 
#' and \code{Details}, specifying the progress message for thsi step
#' @param wd directory where the status.csv file should be written and 
#' updated. If NULL, no status.csv is written.
#' 
#' @return returns a \code{writeStatus} object, see \code{Details} and writes to 
#' a \code{status.csv} file in \code{wd}
#' 
#' @details A \code{writeStatus} object is a named list with two elements:
#' \describe{
#' \item{pt}{current \code{\link[base]{proc.time}()}},
#' \item{status}{A data.frame with progress messages in rows, and these columns:
#' \itemize{
#' \item \code{Time} time stamp of analysis step
#' \item \code{Status} status message/ analysis step
#' \item \code{Details} details on analysis step
#' \item \code{elapsed_time} total elapsed time
#' }
#' 
#' }
#' }
#' 
#' @examples 
#' 
#' status <- writeStatus(previous = NULL,
#'             message = list(Status = "Starting analysis",
#'                            Details = "loading files"
#'                           ),
#'            wd = getwd())
#'            
#'            
#' status <- writeStatus(previous = status,
#'             message = list(Status = "Running analysis",
#'                            Details = "detecting features"
#'                           ),
#'            wd = getwd())
#' 
#' @export
writeStatus <- function(previous = NULL,
                        message = list(Status = "Starting analysis",
                                       Details = "loading files"
                                       ),
                        wd = getwd()
                        ){
  if(is.null(previous)){
    history <- data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                          Status = message$Status,
                          Details = message$Details,
                          elapsed_time = 0,
                          stringsAsFactors = F
    )
    
    for(i in names(message)){
      history[[i]] <- as.character(message[[i]])
    }
    
    pt1 <- proc.time()
    
    if(!is.null(wd)){
    write.csv(history,file = file.path(wd,"status.csv"))}
    
    res <- list(pt = pt1,
              status = history)
    
    class(res) <- "writeStatus"
    
  return(res)
    }
  
  #If there is a previous input, update the status
  pt1 <- proc.time() - previous$pt
  previous$status[1,"elapsed_time"] <- pt1[3]
  pt1 <- proc.time()
  
  history <- rbind(data.frame(Time = strftime(Sys.time(),"%Y%m%d_%H%M%S"),
                             Status = message$Status,
                             Details = message$Details,
                             elapsed_time = 0,
                             stringsAsFactors = F
                             ),
                              previous$status)
  
  write.csv(history,file = file.path(wd,"status.csv"))
  
  res <- list(pt = pt1,
              status = history)
  
  class(res) <- "writeStatus"
  
  return(res)
}


#' savetable
#' 
#' Save a peakTable from an xcms::XCMSnExp, xcms::xcmsSet or xcms::xsAnnotate
#'  object, and also keep track of analysis status via internal calls to 
#'  \code{\link{writeStatus}()}.
#'  
#' @return Returns a \code{writeStatus} object, writes to \code{status.csv} 
#'  file as well as other .csv files (with processed or unprocessed peakTables) 
#'  in current working directory.
#' 
#' @param xset an xcms::XCMSnExp, xcms::xcmsSet or xcms::xsAnnotate object from 
#' which to extract the peaklist
#' @param status a \code{\link{writeStatus}} object to be modified and returned
#'  by the function, or NULL to start a new status.csv file
#' @param fill xcms::FillChromPeaksParam object to specify how to fill peaks
#'  (only works if xset is an XCMSnExp object). No peak filling if NULL.
#' @param nonfill also write the non-filled peaktable data? TRUE or FALSE
#' @param filename filename to use when writing the csv file (including file 
#' extension). "_filled" will be added for filled data.
#' @param bparams BBPARAMs (BiocParallel::BiocParallelParam object with 
#' parameters for parallel computing used in fill peaks)
#' @param intensities list of parameters for Mseek EIC intensity extraction 
#' (for each feature in the peaklist). ppm: mz window, rtw: rt window 
#' (+/- seconds), rtrange: if TRUE, rtw extends from rtmin and rtmax, 
#' if FALSE rtw centers around value in column rt. Not performed if 
#' intensities = NULL.
#' @param rawdata named list of xcmsRaw objects (as returned by
#'  Mseek::loadRawM), required for Mseek EIC intensity extraction which 
#'  is not performed if NULL.
#' @param saveR save the xset object as .Rds file
#' @param postProc list of SOME arguments passed to \code{\link{analyzeTable}()}
#'  for post processing, or NULL to skip processing. 
#'  TODO: transisiton to do.call solution for postProc and document
#'  
#' @export
savetable <- function(xset,
                      status = NULL,
                      fill = xcms::FillChromPeaksParam(expandMz = 0.005,
                                                 expandRt = 5, ppm = 3),
                      nonfill = F,
                      filename = "tableoutxx",
                      bparams = SnowParam(workers = 1),
                      intensities = list(ppm = 5,
                                         rtw = 5,
                                         rtrange = T),
                      rawdata = NULL,
                      saveR = T,
                      postProc = NULL){
  
  if(is.null(fill) & !nonfill){return(status)}
  
    grouptable <- NULL
    
    if(!is.null(postProc)){
  if(is.null(intensities)){
      #remove the __XIC in column names if no MseekIntensiy requested
    postProc$fileGrouping <- lapply(postProc$fileGrouping,grep, pattern = "__XIC",replacement = "")
  }
  
        if(length(postProc$fileGrouping)){      
  grouptable <- data.frame(Column = unlist(postProc$fileGrouping),
                           Group = unlist(lapply(seq_len(length(postProc$fileGrouping)),
                                                 function(i){rep(names(postProc$fileGrouping)[i],
                                                                 lengths(postProc$fileGrouping)[i])})),
                           stringsAsFactors = FALSE)
        }
  
    }
  
    
  
  if(!is.null(status)){
   status <- writeStatus (previous = status,
                           message = list(Status = paste0("Saving table ", filename),
                                         Details = "Extracting peaktable"))
  }
  
 
    
 tb_mskFT <- buildMseekFT(object = xset,
                 anagrouptable = grouptable,
                 tablename = paste0(filename,"_unprocessed"),
                 editable = F,
                 processHistory = NULL)
    
 #set NAs to 0 (mostly important if !fill)
 tb_mskFT <- removeNAs(tb_mskFT)

    
    if(!is.null(intensities) & !is.null(rawdata)){
         
       if(!is.null(status)){
      status <- writeStatus (previous = status,
                              message = list(Status = paste0("Saving table ", filename),
                                             Details = "Getting Mseek intensities"))
       }
      
  rtx <-  rtexport(xset)    
  
 rta <- rtadjust(rtx, tb_mskFT$df[,c("rt","rtmin","rtmax")])
 
 ###Get Mseek Intensities
 intens <- data.frame(pholder = numeric(nrow(tb_mskFT$df)))
 
      for(i in 1:length(rawdata)){
        if(intensities$rtrange){
        rtwin <- data.frame(rtmin = rta[[i]]$rtmin-intensities$rtw,
                            rtmax = rta[[i]]$rtmax+intensities$rtw)
        rtwin[rtwin < 0]<-0
        }else{
          rtwin <- data.frame(rtmin = rta[[i]]$rt-intensities$rtw,
                              rtmax = rta[[i]]$rt+intensities$rtw)
          rtwin[rtwin < 0]<-0
        }
        
        intens[[paste0(basename(names(rawdata)[i]),"__XIC")]] <- exIntensities(rawfile= rawdata[[i]],
                                                               mz = tb_mskFT$df$mz,
                                                               ppm=intensities$ppm,
                                                               rtw= rtwin
                                                               )
      }
 intens <- intens[,which(colnames(intens) != "pholder")]
 
 
    }else{
        intens <- NULL
        }
 
 analysisIterations <- character()
 
 if(nonfill){ analysisIterations <- c(analysisIterations, "unfilled")}
 if(!is.null(fill) & class(xset) == "XCMSnExp"){ analysisIterations <- c(analysisIterations, "filled")}
 
 for(step in analysisIterations){
 
     #important that step "unfilled" runs first, if requested!
  if(step == "filled"){
      
      if(!is.null(status)){
          status <- writeStatus (previous = status,
                                 message = list(Status = paste0("Saving table ", filename),
                                                Details = "Filling peaks (xcms fillChromPeaks)"))
      }
      
      xset <- xcms::fillChromPeaks(xset, fill,
                                   BPPARAM = bparams)
      
      tb_mskFT <- buildMseekFT(object = xset,
                               anagrouptable = grouptable,
                               tablename = paste0(filename,"_unprocessed"),
                               editable = F,
                               processHistory = NULL)
      
      #set NAs to 0 (mostly important if !fill)
      beforeHash <- digest::digest(tb_mskFT$df,
                                   algo = "xxhash64")
      tb_mskFT$df[is.na(tb_mskFT$df)]<-0
      afterHash <- digest::digest(tb_mskFT$df,
                                  algo = "xxhash64")
      tb_mskFT <- addProcessHistory(tb_mskFT, FTProcessHistory(changes = afterHash != beforeHash,
                                                               inputDFhash = beforeHash,
                                                               outputDFhash = afterHash,
                                                               sessionInfo = NULL,
                                                               info = "Replaced NA values in df by 0!"))
      
      filename <- paste0(filename,"_filled")
      
  }
      
    if(!is.null(intens)){
        beforeHash <- digest::digest(tb_mskFT$df,
                                     algo = "xxhash64")
        tb_mskFT$df <- cbind(tb_mskFT$df,intens) 
        afterHash <- digest::digest(tb_mskFT$df,
                                     algo = "xxhash64")
        tb_mskFT <- addProcessHistory(tb_mskFT, FTProcessHistory(info = "Added Mseek intensities!",
                                                                 inputDFhash = beforeHash,
                                                                 outputDFhash = afterHash,
                                                                    param = FunParam(fun = "Metaboseek::savetable",
                                                                                       args = list(intensities = intensities),
                                                                                       longArgs = list(rawdata = summary(rawdata)))))
        
        }
      

      
    
    if(!is.null(status)){
      status <- writeStatus (previous = status,
                             message = list(Status = paste0("Saving table ", paste0(tb_mskFT$tablename,
                                                                                    "_unprocessed.csv")),
                                            Details = "Writing file"))
    }
    
      saveMseekFT(tb_mskFT, file = tb_mskFT$tablename, 
                  writeCSV = TRUE, writeRDS = TRUE)
        
   # write.csv(tb, file = gsub("\\.csv$","_unprocessed.csv",filename))
   
    
    if(saveR){saveRDS(xset,file = paste0(filename,"_xset.Rds"))}
    
    
    if(!is.null(postProc)){
      
      
      
  if(!is.null(status)){
      status <- writeStatus (previous = status,
                             message = list(Status = paste0("Post-Processing ", filename),
                                            Details = paste(c("selected analyses:", postProc$analysesSelected, postProc$analysesSelected2), collapse = " ")))
  }
      
       tb_mskFT <- analyzeFT(object = tb_mskFT,
                       MSData = rawdata,
                       param = FTAnalysisParam(
                          intensities = if(!is.null(intens)){
                            colnames(intens) }else{unname(unlist(postProc$fileGrouping))},
                          groups = postProc$fileGrouping,
                          analyze = c(postProc$analysesSelected,postProc$analysesSelected2), 
                          normalize = postProc$normalize,
                          useNormalized = postProc$useNormalized,
                          logNormalized = postProc$logNormalized,
                          ppm = if(!is.null(postProc$ppm)){postProc$ppm}else{5},
                          controlGroup = postProc$controlGroups,
                          numClusters = postProc$numClusters,
                          workers = bparams$workers))
      

    
    if(!is.null(status)){
        status <- writeStatus (previous = status,
                           message = list(Status = paste0("Post-Processing finished", filename),
                                          Details = paste(if(length(res$errMsg)==0){"No errors"}else{p( paste0(names(res$errmsg), ": ", unlist(res$errmsg), collapse = "/ " ))})
                                          ))
    }
       
       tb_mskFT <- rename(tb_mskFT, filename)
       
       if(!is.null(status)){
         status <- writeStatus (previous = status,
                                message = list(Status = paste0("Saving table ", tb_mskFT$tablename),
                                               Details = "Writing file"))
       }
       
      # write.csv(tb, file = filename)
       saveMseekFT(tb_mskFT, file = paste0(filename), 
                   writeCSV = TRUE, writeRDS = TRUE)
  }
  }
      
  return(status)
}


#' cameraWrapper
#' 
#' converts a \code{XCMSnExp} object to \code{xcmsSet}, then consecutively calls
#' \code{xsAnnotate}, \code{groupFWHM}, \code{groupCorr}, \code{findIsotopes} and 
#' \code{findAdducts} from the \code{CAMERA} package, and finally adds an entry to
#' its \code{.processHistory} slot.
#' 
#' @param workers number of workers, passed on to \code{\link[CAMERA]{xsAnnotate}()}
#' (as \code{nSlaves})
#' @param polarity passed on to \code{\link[CAMERA]{xsAnnotate}()} and 
#' \code{\link[CAMERA]{findAdducts}()}
#' @param sigma passed on to \code{\link[CAMERA]{groupFWHM}()}
#' @param perfwhm passed on to \code{\link[CAMERA]{groupFWHM}()}
#' @param cor_eic_th passed on to \code{\link[CAMERA]{groupCorr}()}
#' @param pval passed on to \code{\link[CAMERA]{groupCorr}()}
#' @param maxcharge passed on to \code{\link[CAMERA]{findIsotopes}()}
#' @param maxiso passed on to \code{\link[CAMERA]{findIsotopes}()}
#' @param ppm passed on to \code{\link[CAMERA]{findIsotopes}()} and 
#' \code{\link[CAMERA]{findAdducts}()}
#' @param mzabs passed on to \code{\link[CAMERA]{findIsotopes}()} and 
#' \code{\link[CAMERA]{findAdducts}()}
#' @param minfrac passed on to \code{\link[CAMERA]{findIsotopes}()}
#' @param filter passed on to \code{\link[CAMERA]{findIsotopes}()}
#' 
#' @return an \code{xsAnnotate} object
#' 
#' @export
cameraWrapper <- function(xset,
                          polarity = NULL,
                          sigma = 6, perfwhm = 0.6,
                          cor_eic_th = 0.75, pval = 0.05,
                          maxcharge = 3, maxiso = 4,
                          ppm = 5, mzabs = 0.01,
                          minfrac = 0.5, filter = TRUE,
                          workers = 1){
    
    allargs <- as.list(environment())
    params <- xcms::GenericParam(fun = "cameraWrapper",
                                 args = allargs[names(allargs) != "xset"])

an   <- CAMERA::xsAnnotate(as(xset, "xcmsSet"),
                   nSlaves = workers,
                   polarity = polarity)###CHANGE POLARITY

an <- CAMERA::groupFWHM(an,
                sigma = sigma,
                perfwhm = perfwhm ) # peakwidth at FWHM is about 2.335*sigma, sigma factor should correspond to what max rt difference can be for features to be grouped.
#verify grouping
an <- CAMERA::groupCorr(an,
                cor_eic_th = cor_eic_th,
                pval = pval)

an <- CAMERA::findIsotopes(an,
                   maxcharge = maxcharge,
                   maxiso = maxiso,
                   ppm = ppm,
                   mzabs = mzabs,
                   minfrac = max(0.001,minfrac), #minFrac of 0 throws error otherwise
                   filter = filter)
an <- CAMERA::findAdducts(an,
                  ppm = ppm,
                  mzabs = mzabs,
                  polarity= polarity)

CAMERA::cleanParallel(an)

an <- addProcessHistory(an, xcms:::XProcessHistory(info = "Converted to CAMERA::xsAnnotate, analyzed with Metaboseek::cameraWrapper",
                                            param = params))

return(an)

}
