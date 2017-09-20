setwd('C:/Users/mjh43/OneDrive - Cornell University/R scripts new/Mosaic/Beta r2/')

uie <- source("ui.R")$value

ui2<-unlist(uie())
allids <- unname(ui2[grep(".id",names(ui2),value=F)])
cat(paste0('"',allids,'"', collapse = ", "))
write(paste0('"',allids,'"', collapse = ", "), file = "ids.txt")

rnorm(100000)