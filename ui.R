library(tidyverse)

wine <- read.csv("winemag-data-130k-v2.csv")

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

navbarPage("Wine Stats!", panel1, panel2)
