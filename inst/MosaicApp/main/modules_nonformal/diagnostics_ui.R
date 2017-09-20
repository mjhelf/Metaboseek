fluidPage(runcodeUI(code = "", type = c("text", "textarea", "ace"), width = NULL,
                     height = NULL, includeShinyjs = FALSE),
verbatimTextOutput('diag'))