library(tidyverse)
library(rlang)


# Shiny App Draft: Wine Data
# by Connor Graves and Raquel Bonilla

wine <- read.csv("winemag-data-130k-v2.csv")
wine <- wine %>%
          slice(1:40000)

# For wine points / country graph
USwine <- subset(wine, wine$country == "US")
stateavg <- USwine %>%
  group_by(province) %>%
  summarize(avgPoints = mean(points))

countries <- wine %>%
              select(country)
countries <- unique(countries)
countries <- sort(countries$country)

# For review ratings on wine graph
reviewers <- wine %>%
              select(taster_name)

function(input, output, session) {
  
  output$plot1 <- renderPlot({
   USwine <- subset(wine, wine$country == input$selectCountry)
    
    stateavg <- USwine %>%
      group_by(province) %>%
      summarize(avgPoints = mean(points))
  
    ggplot(stateavg, aes(x=province, y = avgPoints, fill = province)) + 
      geom_bar(stat = "identity") + coord_cartesian(ylim = c(min(stateavg$avgPoints) - 2, max(stateavg$avgPoints) + 2)) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), legend.position="none") + 
      labs(title = "Average Points of All Wines reviewed by Region of a Country", x = "Region", y ="Average Points Earned")
  }),
  output$plot2 <- renderPlot({
    
    reviewer <- wine %>%
      filter(taster_name == input$selectReviewer)  %>%# the user can switch the name; maybe a toggle dropdown
      arrange(desc(points)) %>%
      select(taster_name, points, title, region_1)
    
    reviewer <- unique(reviewer) # get rid of duplicates
    
    num_to_display <- 5 # user can enter how many they want to see on the graph
    
    reviewer <- reviewer %>%
      slice(1:num_to_display)
    
    ggplot(reviewer, aes(x=title, y=points, fill=region_1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), legend.position="none") +
      geom_bar(stat="identity") + coord_cartesian(ylim = c(80, 100))
      
  })
}