library(tidyverse)
library(rlang)


wine <- read.csv("../CourseDataSets/wine-reviews/winemag-data-130k-v2.csv")

USwine <- subset(wine, wine$country == "US")
stateavg <- USwine %>%
  group_by(province) %>%
  summarize(avgPoints = mean(points))

countries <- wine %>%
              select(country)
countries <- unique(countries)
countries <- sort(countries$country)

function(input, output, session) {
  
  reactive({
   USwine <- subset(wine, wine$country == input$selectCountry)
    
    stateavg <- USwine %>%
      group_by(province) %>%
      summarize(avgPoints = mean(points))
  })
  
  
  
  output$plot1 <- renderPlot({
    ggplot(stateavg, aes(x=province, y = avgPoints, fill = province)) + 
      geom_bar(stat = "identity") + coord_cartesian(ylim = c(min(stateavg$avgPoints) - 2, max(stateavg$avgPoints) + 2)) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), legend.position="none") + 
      labs(title = "Average Points of All Wines reviewed by US State", x = "State", y ="Average Points Earned")
  })
  output$plot2 <- renderPlot({
    iris %>%
      ggplot(aes(x = Sepal.Length, y = Petal.Length)) +
      geom_point()
  })
}