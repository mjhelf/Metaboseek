#load packages for code that is not functionalized yet
library(Metaboseek)

#load .MseekOptions in case they have been deleted from environment
MseekOptions(recentProjects = "",
             filePaths = c(test = "./"),
             testMode = T)


ui <- function(request){
    
    MseekContainerUI("Mseek")
    
}

server <- function(input, output, session) {
    callModule(MseekContainer, "Mseek")
}


# Create Shiny app ----
shinyApp(ui, server)