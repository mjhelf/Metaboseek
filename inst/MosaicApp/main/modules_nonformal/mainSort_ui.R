fluidPage(
    fluidRow(
    column(2,
          fluidRow(htmlOutput('mainSortDecreasing')),
           fluidRow(htmlOutput('mainSortToggle'))
          ),
    column(10,
           htmlOutput('mainSort'))),
#    htmlOutput("FilterUI"),
actionButton('addFilter',"Add Filter"),
actionButton('updateFilter',"Update Filter"),
#checkboxInput("multiFilterToggle","Toggle all Filters"),
FilterModuleUI("Filter1"),
FilterModuleUI("Filter2"),
FilterModuleUI("Filter3"),
FilterModuleUI("Filter4"),
FilterModuleUI("Filter5")


)