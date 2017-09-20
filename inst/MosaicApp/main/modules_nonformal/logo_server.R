output$logo <- renderImage({
    
    list(src = "img/Mosaic_logo.png",
         contentType = 'image/png',
         width = 250,
        # height = 300,
         alt = "MOSAiC")
    
}, deleteFile = FALSE)