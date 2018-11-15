pageWithSidebar(
  headerPanel('Iris Data'),
  sidebarPanel(
    ## Input Widgets
  ),
  mainPanel(
    plotOutput('plot1'),
    
    plotOutput('plot2')
      )
)