callModule(updaterModule, 'update', tag = 'update', set =list(package = "Mosaic",
                                                                    refs = c("master", "devel", "devel_raw"),
                                                                    active = T))
 

#output$projectName <- renderUI({
 #   textInput('projectName', 'Project Name:', value = paste0("MOSAiC_session_",timeStamp))
#})