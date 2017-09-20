
mzxml_pos <- list.files("C:/Workspace/mzxml/MOSAIC Experimental/ppac vs cele new/full files/", pattern="mzXML", recursive = TRUE, include.dirs = T, full.names = T)
rwdata <- loadRaw(mzxml_pos)

rawgroups <- data.frame(File = mzxml_pos, Group = c("G1","G2","G2","G2"), stringsAsFactors = F)
MSL <- constructRawLayout(rawgroups)

ftable <- read.csv("C:/Workspace/mzxml/MOSAIC Experimental/SMID-DB_Cele_v_Ppac_plist_own.csv", stringsAsFactors = F)
FT <- constructFeatureTable(ftable, anagrouptable = data.frame(Column = colnames(ftable)[5:8], Group = c("G1","G2","G2","G2"), stringsAsFactors = F))


EICs <- multiEIC(rawdata= rdata,
                 mz = data.frame(mzmin = FT$df[,"mz"]-0.01, mxmax = FT$df[,"mz"] + 0.01),
                 rt = data.frame(mzmin = FT$df[,"rt"]-10, mxmax = FT$df[,"rt"] + 10),
                 rnames = row.names(FT$df), #major item names
                 byFile = F #if true, table will be sorted by rawfile, otherwise by feature
)

max(unlist(EICs[[1]][1:2,]$intensity))

xx <- t(as.matrix(EICs[[1]][1,])['intensity',])

row.names(xx) <- "bvoo"

EICgeneral(rtmid = FT$df[,"rt"],
                       mzmid = FT$df[,"mz"],
                       glist = MSL$grouping,
                       cols = MSL$settings$cols,
                       colrange = MSL$settings$colr,
                       transparency = MSL$settings$alpha,
                       RTall = F,
                       TICall = F,
                       rtw = MSL$settings$rtw,
                       ppm = MSL$settings$ppm,
                       rdata = rwdata,
                       pdfFile = NULL,
                       leadingTIC = F)

as.matrix(FT$df[,FT$intensities],
          ctrl = c(1,2,3)
)
rme <- function(cols,mx){return(if(length(cols)==1){mx[,cols]}else{rowMeans(mx[,cols])})}
rmeans <- sapply(FT$anagroupnames,rme, as.matrix(FT$df[,FT$intensities]))
