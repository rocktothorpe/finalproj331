library(tidyverse)

load("workspace.Rdata")

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
                       hr()
                     ),
                     mainPanel(
                       plotOutput('countryPlot')
                     )
                   )
          )

panel2 <- tabPanel("Reviewer Scores",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("selectReviewer", label = h3("Reviewer"),
                                   choices = as.list(reviewers),
                                   selected = "Alexander Peartree"), hr()
                     ),
                     mainPanel(
                       plotOutput('reviewPlot')
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
                                   selected = "points"), hr()
                     ),
                     mainPanel(
                       plotOutput('quantilePlot')
                     )
                   )
)
panel4 <- tabPanel("Regression Analysis",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("Ymax", label = h3("Y Maximum"),
                                   min = 0, max = 100, value = 100)
                     ),
                     mainPanel(
                       plotOutput('regressionPlot')
                     )
                   )
)

navbarPage("Wine Stats!", panel1, panel2, panel3, panel4)
