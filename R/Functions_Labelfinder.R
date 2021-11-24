#' featlistCompare
#' 
#' compare feature tables and find molecular features that are shifted from the 
#' mz values in \code{reflist} by \code{mzdiff} and are within \code{rtdiff}.
#' 
#' Will first find matches in \code{complist} and then go back and find the shifted
#' mz in \code{reflist} as well as the unshifted mass in \code{complist}.
#' 
#' @param reflist data.frame with molecular features and columns \code{mz, rt, rtmin, rtmax}
#' @param complist data.frame with molecular features and columns \code{mz, rt, rtmin, rtmax}
#' @param mzdiff expected mz difference between features of interest in \code{reflist} and \code{complist}
#' @param pktol maximum fold difference in retention time peak width between matched peaks
#' @param rtdiff maximum retention time difference (same unit as \code{rt} columns in \code{reflist} and \code{complist}, typically seconds)
#' @param ppm maximum relative \code{mz} difference between matched peaks in parts per million (ppm)
#'
#' @return a list of data.frames
#'
#' @examples
#' \dontrun{
#'  MseekExamplePreload(data = TRUE, tables = TRUE)
#' demo_reflist = data.frame(mz = 101:200,
#'                      rt = 201:300,
#'                      rtmin = 198:297,
#'                      rtmax = 201:300)
#' 
#' demo_complist = demo_reflist
#' 
#' demo_complist$mz <- demo_complist$mz + rnorm(100, 2*1.00335, sd = 2*1.00335/600)
#' 
#' demo_complist[101:150,] <- demo_complist[1:50,]
#' 
#' demo_complist$mz[101:150] <- demo_complist$mz[101:150] + 1e-10
#' 
#' 
#' compareResults <- featlistCompare(reflist = demo_reflist, complist = demo_complist)
#' 
#'   MseekExamplePreload(data = FALSE, tables = TRUE)
#' compareResultsBig <- featlistCompare(reflist = tab2$df, complist = tab2$df)
#'
#' }
#' 
#' @export
featlistCompare <- function(reflist=reflist,
                            complist=complist,
                            mzdiff=2*1.00335,
                            pktol = 2,#fold difference in peak width allowed
                            rtdiff=10,
                            ppm=5
){
    
    cat(paste0("Reference list with ",length(reflist[,1])," features, iterating through list, feature #" ))
    
    collector <- vector()
    collectlist <- list()
    for (i in c(1:length(reflist[,1]))){
        
        # find labeled counterparts of feature in labeled sample
        selection1 <- complist[which(abs(reflist$mz[i]+mzdiff-complist$mz)<reflist$mz[i]*ppm*0.000001 
                                     & abs(reflist$rt[i]-complist$rt)<rtdiff
                                     & complist$rtmax-complist$rtmin < (reflist$rtmax[i]-reflist$rtmin[i]) * pktol
                                     & complist$rtmax-complist$rtmin > (reflist$rtmax[i]-reflist$rtmin[i]) / pktol
        ),]
        
        
        #find "labeled" compounds in unlabeled sample
        selection2 <- reflist[which(abs(reflist$mz[i]+mzdiff-reflist$mz)<reflist$mz[i]*ppm*0.000001 
                                    & abs(reflist$rt[i]-reflist$rt)<rtdiff
                                    # & reflist$rtmax-reflist$rtmin < peakwi*pktol
                                    #  & reflist$rtmax-reflist$rtmin > peakwi/pktol
        ),]
        
        #find "unlabeled" compounds in labeled sample
        selection3 <- complist[which(abs(reflist$mz[i]-complist$mz)<reflist$mz[i]*ppm*0.000001 
                                     & abs(reflist$rt[i]-complist$rt)<rtdiff
                                     #  & complist$rtmax-complist$rtmin < peakwi*pktol
                                     #  & complist$rtmax-complist$rtmin > peakwi/pktol
        ),]
        
        #selection1e <-selection1
        
        
        if(length(selection1[,1])>0){
            
            collectlist$I1S1[[length(collectlist$I1S1)+1]] <- reflist[i,]
            collectlist$I2S2[[length(collectlist$I1S1)]] <- selection1[order(abs(selection1$rt-reflist$rt[i])),]
            collectlist$I2S1[[length(collectlist$I1S1)]] <- selection2[order(abs(selection1$rt-reflist$rt[i])),]
            collectlist$I1S2[[length(collectlist$I1S1)]] <- selection3[order(abs(selection1$rt-reflist$rt[i])),]
            
        }
        if(!i %% 500){message(i, " ")}}
    return(collectlist)}


#' mergecollectlist
#' 
#' helper function to reorganize featlistcompare() outputs for findLabeledPeaks()
#' 
#' @param collectlist a list of list of data.frames from featlistcompare()
#' 
mergecollectlist <- function(collectlist){
    #combining the lists into one file, using only the features as filtered with the smallest RT difference (1 to 1 to 1 to 1 feature); smallest rt because dfs are ordered by featlistcompare()
    
    combinat <- lapply(c(1:length(collectlist$I1S1)), function(n){
        collectlist$I1S1[[n]]$feat <- n
        collectlist$I2S2[[n]]$feat <- n
        collectlist$I2S1[[n]]$feat <- n
        collectlist$I1S2[[n]]$feat <- n
        
        #keep column names for I1S1
        #colnames(collectlist$I1S1[[n]]) <- paste0("I1S1.",colnames(collectlist$I1S1[[n]]))
        
        
        colnames(collectlist$I2S2[[n]]) <- paste0("I2S2.",colnames(collectlist$I2S2[[n]]))
        colnames(collectlist$I2S1[[n]]) <- paste0("I2S1.",colnames(collectlist$I2S1[[n]]))
        colnames(collectlist$I1S2[[n]]) <- paste0("I1S2.",colnames(collectlist$I1S2[[n]]))
        
        
        combinat <-cbind(collectlist$I1S1[[n]][1,],collectlist$I2S2[[n]][1,],
                         collectlist$I2S1[[n]][1,],collectlist$I1S2[[n]][1,])
        
    })
    
    
    
    return(do.call(rbind,combinat))}

mergecollectlist2 <- function(collectlist){
    #combining the lists into one file, using only the features as filtered with the smallest RT difference (1 to 1 to 1 to 1 feature)
    
    #lapply(names(collectlist), function(n){colnames(collectlist[[n]]) <- paste0(n,".",colnames(collectlist[[n]]))})
    
    
    combinat <- do.call(rbind, collectlist[[1]])
    
    for(n in names(collectlist)){
        combinat[[n]] <- collectlist[[n]]
    }
    
    return(combinat)}


#' findLabeledPeaks
#' 
#' find labeled peaks in a data.frame that was prepared by mergecollectlist()
#' 
#' @param df data.frame as returned by mergecollectlist(); importantly with IXSX prefixes, including for the intensity columns
#' @param ref_intensityCols intensity column names from original feature table (without IXS1. prefix) e.g. for unlabeled samples
#' @param comp_intensityCols intensity column names from original feature table (without IXS2. prefix) e.g. for labeled samples
#' @param ifoldS1 min. fold Iso1/Iso2 in unlabeled
#' @param ifoldS2 max. fold Iso1/Iso2 in labeled
#' 
findLabeledPeaks <- function (df,
                              ref_intensityCols,
                              comp_intensityCols,
                              ifoldS1 = 10,
                              ifoldS2 = 5){
    
    
    if(length(ref_intensityCols) > 1){
        df$I1S1.meanInt = rowMeans(df[,paste0(ref_intensityCols)])
        df$I2S1.meanInt = rowMeans(df[,paste0("I2S1.",ref_intensityCols)])
    }else{
        df$I1S1.meanInt = df[,paste0(ref_intensityCols)]
        df$I2S1.meanInt = df[,paste0("I2S1.",ref_intensityCols)]
    }
    
    if(length(comp_intensityCols) > 1){
        df$I1S2.meanInt = rowMeans(df[,paste0("I1S2.",comp_intensityCols)])
        df$I2S2.meanInt = rowMeans(df[,paste0("I2S2.",comp_intensityCols)])
    }else{
        df$I1S2.meanInt = df[,paste0("I1S2.",comp_intensityCols)]
        df$I2S2.meanInt = df[,paste0("I2S2.",comp_intensityCols)]
    }
    
    
    df$ratio.I1S1.I2S1 <- as.numeric(df$I1S1.meanInt)/as.numeric(df$I2S1.meanInt)
    df$ratio.I1S2.I2S2 <- as.numeric(df$I1S2.meanInt)/as.numeric(df$I2S2.meanInt)
    
    
    filtrate <- df[which(df$ratio.I1S1.I2S1> ifoldS1
                         & df$ratio.I1S2.I2S2< ifoldS2),]
    filtrate$rt <- (filtrate$rt+filtrate$I2S2.rt)/2
    #filtrate$mz <- filtrate$mz
    filtrate$mziso <- filtrate$I2S2.mz
    #filtrate$Comments <- paste("Unlab: Abovemean/mini", filtrate$Abovemean_I1S1, "/", filtrate$Abovemini_I1S1, "Spikes/Peaks", filtrate$Spikes_I1S1,"/", filtrate$Peaks_I1S1,
    #                          "Lab: Abovemean/mini", filtrate$Abovemean_I2S2, "/", filtrate$Abovemini_I2S2, "Spikes/Peaks", filtrate$Spikes_I2S2,"/", filtrate$Peaks_I2S2)
    
    filtrate <- filtrate[order(filtrate$mz),]
    filtrate
}


#' findLabels
#'
#' find labeled compounds in \code{complist} which correspond to unlabeled compounds in \code{reflist}
#'
#' @inheritParams featlistCompare
#' @param ref_intensityCols intensity column names in reflist
#' @param comp_intensityCols intensity column names in complist
#' @param labelmz expected m/z difference between reference and labeled compounds
#' @param pktolerance maximum fold difference in retention time peak width between matched peaks
#' @param ppm_compare ppm m/z tolerance for feature list comparison
#' @param ifoldS1 min. fold Iso1/Iso2 in unlabeled
#' @param ifoldS2 max. fold Iso1/Iso2 in labeled
#' @param rawdata list of xcmsRaw objects. Item names must be contained in column names
#' @param ppm_extract ppm m/z tolerance for EIC extraction
#' @param rtw_extract retention time window for EIC extraction (seconds, will be applied +/- the expected rt)
#' 
#' @examples
#' \dontrun{
#' MseekExamplePreload(data = TRUE, tables = TRUE)
#' findLabelsResults <- findLabels(reflist = tab2$df[,!colnames(tab2$df) %in% c(tab2$intensities, paste0(tab2$intensities,"__norm"))], #remove intensity columns to have them replaced with new ones from rawdata
#'                                 complist = tab2$df[,!colnames(tab2$df) %in% c(tab2$intensities, paste0(tab2$intensities,"__norm"))],
#'                                 ref_intensityCols = tab2$intensities[1:3],
#'                                 comp_intensityCols = tab2$intensities[4:7],
#'                                 labelmz = 2*1.00335,
#'                                 ifoldS1 = 10,
#'                                 ifoldS2 = 10000,
#'                                 rawdata = MSD$data)
#'  }
#'  
#' @return a data.frame based on reflist, filtered for features with matches in complist
#' and information on a single match (best rt match).
#'
#' @export
findLabels <- function(reflist,
                       complist,
                       ref_intensityCols, 
                       comp_intensityCols,
                       
                       #Label m/z
                       labelmz = 2*1.00335,
                       
                       #fold difference in peak width allowed
                       pktolerance = 20, 
                       
                       #max rt difference (sec)
                       rtdiff = 10,
                       
                       #accuracy values
                       ppm_compare = 5,
                       ifoldS1 = 10,
                       ifoldS2 = 5,
                       
                       rawdata = NULL,
                       ppm_extract = 5,
                       rtw_extract = 5
                       
){
    
    collectlist <- featlistCompare(reflist=reflist,
                                   complist=complist,
                                   mzdiff=labelmz,
                                   pktol = pktolerance,#fold difference in peak width allowed
                                   rtdiff=rtdiff,#max rt difference (sec)
                                   ppm=ppm_compare)
    
    combinat <- mergecollectlist(collectlist)
    
    # Optionally, re-extract EICs here and put in new columns [with appropriate IXSX prefix]
    
    if(length(rawdata)){
        
        for(s in ref_intensityCols){
            
            ## TODO add checks to handle ambiguity
            thisfile <- rawdata[[which(sapply(names(rawdata), function(x){grepl(basename(x), s)}))]]
            
            combinat[[paste0(s)]] <- Metaboseek::exIntensities(thisfile,
                                                                       mz = combinat$mz,
                                                                       rtw= data.frame(combinat$rtmin-rtw_extract,combinat$rtmax+rtw_extract),
                                                                       ppm = ppm_extract,
                                                                       areaMode = TRUE)
            message(paste("Unlabeled intensities extracted for unlabeled sample", s))
            
            combinat[[paste0("I2S1.",s)]] <- Metaboseek::exIntensities(thisfile,
                                                                       mz = combinat$I2S2.mz,
                                                                       rtw= data.frame(combinat$rtmin-rtw_extract,combinat$rtmax+rtw_extract), #changed these to use I1S1 rts instead of I2S2.rt to guess I2S1 rt 
                                                                       ppm = ppm_extract,
                                                                       areaMode = TRUE)
            message(paste("Labeled intensities extracted for unlabeled sample", s))
        }
        
        for(s in comp_intensityCols){
            
            ## TODO add checks to handle ambiguity
            thisfile <- rawdata[[which(sapply(names(rawdata), function(x){grepl(basename(x), s)}))]]
            
            combinat[[paste0("I1S2.",s)]] <- Metaboseek::exIntensities(thisfile,
                                                                       mz = combinat$mz, #used this before, but that would contain NAs: combinat$I1S2.mz,
                                                                       rtw= data.frame(combinat$I2S2.rtmin-rtw_extract,combinat$I2S2.rtmax+rtw_extract), #changed these to use I2S2 rts instead of I1S1.rt to guess I1S2 rt 
                                                                       ppm = ppm_extract,
                                                                       areaMode = TRUE)
            
            message(paste("Unlabeled intensities extracted for labeled sample", s))
            
            
            combinat[[paste0("I2S2.",s)]] <- Metaboseek::exIntensities(thisfile,
                                                                       mz = combinat$I2S2.mz,
                                                                       rtw= data.frame(combinat$I2S2.rtmin-rtw_extract,combinat$I2S2.rtmax+rtw_extract), 
                                                                       ppm = ppm_extract,
                                                                       areaMode = TRUE)
            
            message(paste("Labeled intensities extracted for labeled sample", s))
            
        }
        
    }
    
    
    
   findLabeledPeaks(df = combinat,
                     ifoldS1 = ifoldS1,
                     ifoldS2 = ifoldS2,
                     ref_intensityCols = ref_intensityCols,
                     comp_intensityCols = comp_intensityCols)
}


