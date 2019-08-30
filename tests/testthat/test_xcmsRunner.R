context("xcms integration and object histories")

test_that("xcmsRunner functions work",{
    
    dir.create('test_xcms')
    
    configfol <- system.file("config", package = "Metaboseek")
    datafol <- system.file("extdata", "examples", "ms2", package = "Metaboseek")
    
    file.copy(list.files(file.path(configfol,"xcms_defaults"), full.names = TRUE),
              "./test_xcms", overwrite = TRUE)
    
    file.copy(system.file("scripts","xcms_runner_i.R", package = "Metaboseek"), 
              "./test_xcms", overwrite = TRUE)
    
    write.csv(
        data.frame(File = list.files(datafol, full.names = TRUE, recursive = T),
                   Group = c("G1","G2")),
        "./test_xcms/filegroups.csv")
    
    runner <- system.file("scripts", "xcms_runner_i.R",
                          package = "Metaboseek")
    rpath <- file.path(R.home(component = "bin"), "Rscript")
    
    
    
    system(paste0( '"',
                   rpath,
                   '"  --verbose ',
                   '"',
                   runner,
                   '" "',
                   "test_xcms",
                   '"'),
           wait = TRUE)
    
    status <- read.csv("test_xcms/status.csv", stringsAsFactors = FALSE)
    
    expect_false("ERROR" %in% status$Status)

})
