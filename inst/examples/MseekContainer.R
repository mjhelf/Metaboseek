#' \dontrun{
ui <- function(request){
    MseekContainerUI("Mseek")
}
server <- function(input, output, session) {
    callModule(MseekContainer, "Mseek")
}
# Create Shiny app ----
shinyApp(ui, server)
#' }