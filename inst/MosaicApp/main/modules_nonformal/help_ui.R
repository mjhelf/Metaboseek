fluidPage(
  fluidRow(
    box(title = "Help", status = "danger", collapsible = T, width = 12,
  
        h2("MOSAiC help"),
        p("You can upload MS data files and/or tables with LC-MS features to analyze them with MOSAiC."),

        #h1("MS data files"),

        
        h3("Feature tables"),
        p("Filter your feature tables and run basic statistical analyses in the \"Data Processing\" section."),
                
        h3("MS data files"),
        p("You can browse through the MS data files (mzXML) in the \"Explore Data\" section and run xcms analyses.")
    )
  ),
  fluidRow(
    box(title = "Update mosaic!", status = "danger", collapsible = T, width = 12,
        updaterModuleUI('update')
    )
  )
        
)