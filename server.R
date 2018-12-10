# Shiny App Draft: Wine Data
# by Connor Graves and Raquel Bonilla

# Dependencies
library(tidyverse)
library(rlang)
library(ggplot2)
library(stringr)
library(pastecs)
library(networkD3)
library(RColorBrewer)

load("workspace.RData")

# For wine points / country graph
USwine <- subset(wine, wine$country == "US")
stateavg <- USwine %>%
  group_by(province) %>%
  summarize(avgPoints = mean(points))

# generate Color Pallette that resembles wine colors
wineColors <- brewer.pal(9, "PuRd")
wineColors <- wineColors[4:9]
wineColors <- rep(wineColors, 10)

# subset countries from wine data
countries <- wine %>%
  select(country)

# get unique countries and sort them alphabetically
countries <- unique(countries)
countries <- sort(countries$country)

# For review ratings on wine graph
# get each unique reviewer to develop a list to display to 
# the user 
reviewerList <- wine %>%
  select(taster_name)
reviewerList <- unique(reviewerList$taster_name)
reviewers  <- wine %>%
  filter(taster_name == "Alexander Peartree")  %>%# the user can switch the name; maybe a toggle dropdown
  arrange(desc(points)) %>%
  select(taster_name, points, title, region_1)

reviewers <- unique(reviewers)
reviewers<- reviewers %>%
  arrange(taster_name)
num_to_display <- 5

# for regression modeling
mod <- lm(wine$price ~ wine$points)
rst <- rstandard(mod)
fit <- fitted(mod)
regTest <- as.data.frame(rst)
regTest$fit <- fit

function(input, output, session) {
  
  # this plot will graph point value vs wine variety of a country 
  output$countryPlot <- renderPlot({
    if(input$selectVariety == "All")
      USwine <- subset(wine, wine$country == input$selectCountry)
    if(input$selectVariety != "All")
      USwine <- subset(wine, wine$country == input$selectCountry & wine$variety == input$selectVariety)
    
    # subset region that the user selected
    stateavg <- USwine %>%
      group_by(province) %>%
      summarize(avgPoints = mean(points))
    
    ggplot(stateavg, aes(x=province, y = avgPoints, fill = province)) +
      geom_bar(stat = "identity", fill=wineColors[1:nrow(stateavg)]) + coord_cartesian(ylim = c(min(stateavg$avgPoints) - 2, max(stateavg$avgPoints) + 2)) +
      theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, size = 20, hjust = 1, vjust = 0), legend.position="none") +
      labs(title = paste("Mean Points of", input$selectVariety, "Wines reviewed by Region of", input$selectCountry), x = "Region", y ="Mean Points Earned")
    
  })
  
  # this graph will plot points based on a selected reviewer
  output$reviewPlot <- renderPlot({
    
    # subset reviewers based off of user selection
    reviewers  <- wine %>%
      filter(taster_name == input$selectReviewer)  %>%# the user can switch the name; maybe a toggle dropdown
      arrange(desc(points)) %>%
      select(taster_name, points, title, region_1)
    
    reviewers <- reviewers %>% 
                  distinct
    
    # get only the number of reviews the user wants to view
    reviewers <- reviewers %>%
      slice(1:input$num_to_display)
    
    
    ggplot(reviewers, aes(x=title, y=points)) + 
      theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, size = 14, hjust = 1, vjust = 0), legend.position="none") +
      geom_bar(stat="identity", fill=wineColors[1:input$num_to_display]) + coord_cartesian(ylim = c(80, 100)) +
      labs(title = "Points by Reviewer", x = "Wine", y ="Points") + scale_x_discrete(labels = function(title) str_wrap(title, width = 10))
    
  })
  
  # plots quantiles, the user has a lot of freedom to select which data to view
  # on the graph
  output$quantilePlot <- renderPlot({
    ggplot(wine, aes(x = eval(parse(text = input$`X Variable`)), y = eval(parse(text = input$`Y Variable`)),
                     fill = eval(parse(text = input$`X Variable`)))) + geom_boxplot() + 
      theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, size = 20, hjust = 1, vjust = 0), legend.position="none") + labs(x = input$`X Variable`, y= input$`Y Variable`) +
      coord_cartesian(ylim = c(input$Yrange[1], input$Yrange[2]))
  })
  
  # Start of regression testing
  output$strHistPlot <- renderPlot({
    
    ggplot(regTest, aes(x = fit, y = rst)) + geom_point(colour=wineColors[6]) + coord_cartesian(ylim = c(0, 100)) + ylab("Standardized Residuals") + 
      xlab("Fitted Values") + ggtitle("Standardized Residuals vs Fitted Value") + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, size = 20, hjust = 1, vjust = 0))
    
  })
  output$fitStrPlot <- renderPlot({
    
    ggplot(regTest, aes(x = rst, y=..density..)) + geom_histogram(bins = 1000, fill=wineColors[5]) + ggtitle("Distribution of Standardized residuals") + 
      ylab("Density") + xlab("Standardized Residuals") + coord_cartesian(xlim = c(-5, 5)) + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, size = 20, hjust = 1, vjust = 0))
    
  })
  
  output$pricePointPlot <- renderPlot({
    ggplot(wine, aes(x = points, y = price)) + geom_point(colour=wineColors[6]) + coord_cartesian(ylim = c(0, input$Ymax)) + 
      ylab("Price (in USD)") + xlab("Points") + ggtitle("Wine Price vs Point Value") + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, size = 20, hjust = 1, vjust = 0)) + 
      geom_smooth(method = "lm")
  })
  
  # output p-value for 
  output$tTestSummary <- renderText({
    t <- t.test(wine$points, wine$price)
    return('P-Value for Price vs Points Plot via T-Test is < 2.2e-16')
    
  })
  
  # networkd3 plot so user can see where a reviewer is reviewing the wines
  output$d3Plot <- renderSimpleNetwork({
    
    target  <- wine %>%
      filter(taster_name == input$selectReviewer)  %>%# the user can switch the name; maybe a toggle dropdown
      arrange(desc(points)) 
    target <- target %>%
                distinct
    
      target <- target %>%
                  select(region_1) %>%
                  slice(1:input$num_to_display)
    
    src <- rep(c(input$selectReviewer), input$num_to_display)
    target <- as.vector(target)
    networkData <- data.frame(src, target)
    simpleNetwork(networkData, fontSize = 14, height = 5, width = 10, linkDistance = 80, opacity = 1.0, nodeColour = wineColors)
    
  })
  
  # output summary statistics onto app
  output$subSummary <- renderTable({
    if(input$selectVariety == "All")
      USwine <- subset(wine, wine$country == input$selectCountry)
    if(input$selectVariety != "All")
      USwine <- subset(wine, wine$country == input$selectCountry & wine$variety == input$selectVariety)
    table <- stat.desc(USwine[c("price", "points")])
    table$Statistic = c("Number of Values", "Number of Nulls", "Number of NAs", "Min", "Max", "Range", "Sum", "Median", "Mean", "SE.mean", "Conf. Interval (95%)", "Variane", "Std Deviation", "Coeff Variance")
    table <- table[c(3,1,2)]
    table
  })
  
}

