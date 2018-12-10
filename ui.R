library(tidyverse)
library(networkD3)

load("workspace.RData")

countries <- wine %>%
  select(country)
countries <- unique(countries)
countries <- sort(countries$country)

varieties <- wine %>%
  select(variety)
varieties <- unique(varieties)
varieties <- sort(varieties$variety)

reviewers  <- wine %>%
  select(taster_name)

reviewers <- unique(reviewers)
reviewers<- sort(reviewers$taster_name)

panel1 <- tabPanel("Score by Country",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("selectCountry", label = h3("Country"),
                                   choices = as.list(countries),
                                   selected = "US"),
                       selectInput("selectVariety", label = h3("Variety"),
                                   choices = c("All", as.list(varieties)),
                                   selected = "All"),
                       tableOutput("subSummary"),
                       hr()
                     ),
                     mainPanel(
                       plotOutput('countryPlot', height="700px")
                     )
                   )
          )

panel2 <- tabPanel("Reviewer Scores",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("selectReviewer", label = h3("Reviewer"),
                                   choices = as.list(reviewers),
                                   selected = "Alexander Peartree"),
                       sliderInput("num_to_display", label = h3("Number to display"),
                                   min = 1, max = 10, value = 5), hr()
                     ),
                     mainPanel(
                       simpleNetworkOutput("d3Plot", height="200px"),
                       plotOutput('reviewPlot', height = "600px")
                       
                     )
                   )
          )
panel3 <- tabPanel("Quantile Analysis",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("X Variable", label = h3("X Variable"),
                                   choices = as.list(names(wine[c(-1,-3:-6, -12, -14)])),
                                   selected = "taster_name"),
                       selectInput("Y Variable", label = h3("Y Variable"),
                                   choices = as.list(names(wine[c(-2:-4,-7:-14)])),
                                   selected = "points"),
                       sliderInput("Yrange", label = h3("Y Range"),
                                   min = 0, max = 3000, value = c(80,100)),
                       hr()
                     ),
                     mainPanel(
                       plotOutput('quantilePlot', height="700px"), br(), br()
                     )
                   )
)
panel4 <- tabPanel("Regression Analysis",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("Ymax", label = h3("Y Maximum"),
                                   min = 0, max = 3000, value = 1500),
                       textOutput("tTestSummary")
                     ),
                     mainPanel(
                       plotOutput('pricePointPlot'), br(), br(),
                       plotOutput('strHistPlot'), br(), br(),
                       plotOutput('fitStrPlot'), br(), br()
                       
                     )
                   )
)

navbarPage("Wine Stats!", panel1, panel2, panel3, panel4)
