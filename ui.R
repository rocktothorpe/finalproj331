pageWithSidebar(
  headerPanel('Wine Data'),
  sidebarPanel(
    ## Input Widgets
    selectInput("selectCountry", label = h3("Country"),
                choices = as.list(countries),
                selected = "US"), hr(), fluidRow(column(3, verbatimTextOutput("value")))
    # selectInput("selectReviewer", label = h3("Reviewer"),
    #             choices = as.list(reviewerList),
    #             selected = "Alexander Peartree"), hr(), fluidRow(column(3, verbatimTextOutput("value")))
  ),
  mainPanel(
      plotOutput('plot1'),
      plotOutput('plot2'),
      plotOutput('plot3')
      
    )
)