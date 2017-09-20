fluidPage(
        h2("Welcome to MOSAiC!"),
        p("You can upload MS data files and/or tables with LC-MS features to analyze them with MOSAiC."),

        #h1("MS data files"),
        p("Give a name to your current analysis - this tag will be part of all file names generated in this session."),
        
        htmlOutput("projectName"),
        
        
        h3("Feature tables"),
        p("Filter your feature tables and run basic statistical analyses in the \"Data Processing\" section."),
                
        h3("MS data files"),
        p("You can browse through the MS data files (mzXML) in the \"Explore Data\" section and run xcms analyses.")
        
        
)