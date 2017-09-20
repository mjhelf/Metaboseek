library(shiny)


# Define UI for dataset viewer app ----
ui <- fluidPage(
  textInput('xcms-folder', "mzXML file folder"),
  actionButton('xcms-loadfolder', "load files")
  
  textInput('xcms-name', "Title of this analysis", "xcms_run"),
  
  
      numericInput("tester","test",0),
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary")
      
 
    )
  


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  xcmsSettings <- reactiveValues(params = list(filegroups = data.frame(),
                                               centWave = read.csv("defaults/xcmsAnalysis/centWave.csv",
                                                                   row.names = 1,
                                                                   stringsAsFactors = F),
                                               group = read.csv("defaults/xcmsAnalysis/group.csv",
                                                                row.names = 1,
                                                                stringsAsFactors = F),
                                               retcor = read.csv("defaults/xcmsAnalysis/retcor.csv",
                                                                         row.names = 1,
                                                                         stringsAsFactors = F),
                                               outputs = read.csv("defaults/xcmsAnalysis/outputs.csv",
                                                                  row.names = 1,
                                                                  stringsAsFactors = F)
                                               ))
  
  cparam <- CentWaveParam(ppm = as.numeric(centWave["ppm",1]),
                          peakwidth = c(min(as.numeric(unlist(strsplit(centWave["peakwidth",1], split = " ")))), max(as.numeric(unlist(strsplit(centWave["peakwidth",1], split = " "))))),
                          snthresh = as.numeric(centWave["snthresh",1]),
                          mzCenterFun = as.character(centWave["mzCenterFun",1]),
                          firstBaselineCheck = as.logical(centWave["firstBaselineCheck",1]),
                          prefilter = c(min(as.numeric(unlist(strsplit(centWave["prefilter",1], split = " ")))), max(as.numeric(unlist(strsplit(centWave["prefilter",1], split = " "))))),
                          mzdiff=as.numeric(centWave["mzdiff",1]),
                          noise = as.numeric(centWave["noise",1]), 
                          fitgauss = as.logical(centWave["fitgauss",1]), 
                          verboseColumns = TRUE ) 
  
  library(jsonlite)
  fromJSON(toJSON(mzxml_pos))
  write_json(mzxml_pos, paste0(wd,"files"))
  mzxml_pos2 <- read_json(paste0(wd,"files"), simplifyVector = T)
  
  
  for(i in 1:length(params)){
    write.csv(params[[i]], file = paste0(wd,names(params)[i]), row.names = T)
  }
  
  wd <- "C:/Users/mjh43/OneDrive - Cornell University/R scripts new/Mosaic/Beta r2/external/scripts/"

  
    
  system(paste0("Rscript ",
                '"C:/Users/mjh43/OneDrive - Cornell University/R scripts new/Mosaic/Beta r2/external/scripts/centWaveRun.R" "',
                wd,
                '"'),
         wait = T)
  
  
  print(paste0("Rscript ", '"C:/Users/mjh43/OneDrive - Cornell University/R scripts new/Mosaic/Beta r2/external/scripts/centWaveRun.R" ', wd, '"'))
  
  # Generate a text output ----
  output$summary <- renderPrint({
    print(input$tester)
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)