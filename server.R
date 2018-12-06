library(tidyverse)
library(rlang)
library(ggplot2)
library(stringr)
library(pastecs)


# Shiny App Draft: Wine Data
# by Connor Graves and Raquel Bonilla
load("workspace.Rdata")
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
  
  output$countryPlot <- renderPlot({
    if(input$selectVariety == "All")
      USwine <- subset(wine, wine$country == input$selectCountry)
    if(input$selectVariety != "All")
      USwine <- subset(wine, wine$country == input$selectCountry & wine$variety == input$selectVariety)
    
    stateavg <- USwine %>%
      group_by(province) %>%
      summarize(avgPoints = mean(points))
    
    ggplot(stateavg, aes(x=province, y = avgPoints, fill = province)) +
      geom_bar(stat = "identity") + coord_cartesian(ylim = c(min(stateavg$avgPoints) - 2, max(stateavg$avgPoints) + 2)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), legend.position="none") +
      labs(title = paste("Mean Points of", input$selectVariety, "Wines reviewed by Region of", input$selectCountry), x = "Region", y ="Mean Points Earned")
    
  })
  output$reviewPlot <- renderPlot({
    
    reviewers  <- wine %>%
      filter(taster_name == input$selectReviewer)  %>%# the user can switch the name; maybe a toggle dropdown
      arrange(desc(points)) %>%
      select(taster_name, points, title, region_1)
    #
    # # make this a widget as well
    
    #
    reviewers <- reviewers %>%
      slice(1:input$num_to_display)
    
    ggplot(reviewers, aes(x=title, y=points, fill=region_1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), legend.position="none") +
      geom_bar(stat="identity") + coord_cartesian(ylim = c(80, 100)) +
      labs(title = "Points by Reviewer", x = "Wine", y ="Points") + scale_x_discrete(labels = function(title) str_wrap(title, width = 10))
    
    
  })
  
  output$quantilePlot <- renderPlot({
    ggplot(wine, aes(x = eval(parse(text = input$`X Variable`)), y = eval(parse(text = input$`Y Variable`)),
                     fill = eval(parse(text = input$`X Variable`)))) + geom_boxplot() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), legend.position="none") + labs(x = input$`X Variable`, y= input$`Y Variable`) +
      coord_cartesian(ylim = c(input$Yrange[1], input$Yrange[2]))
  })
  
  output$strHistPlot <- renderPlot({
    
    # plot(wine$price ~ wine$points)
    # abline(lm( wine$price ~ wine$points, data=wine))
    mod <- lm(wine$price ~ wine$points)
    rst <- rstandard(mod)
    fit <- fitted(mod)
    
    # plot(rst~fit, main="Standardized Residuals vs Fitted Value", ylab ="Standardized Residuals", xlab="Fitted Value")
    
    ggplot(regTest, aes(x = fit, y = rst)) + geom_point() + coord_cartesian(ylim = c(0, input$Ymax)) + ylab("Standardized Residuals") + xlab("Fitted Values") + ggtitle("Standardized Residuals vs Fitted Value")
    
    
    
    
  })
  output$fitStrPlot <- renderPlot({
    
    # plot lmreg line for price vs points
    mod <- lm(wine$price ~ wine$points)
    rst <- rstandard(mod)
    fit <- fitted(mod)
    
    ggplot(regTest, aes(x = rst, y=..density..)) + geom_histogram(bins = 1000) + ggtitle("Distribution of Standardized residuals") + 
      ylab("Density") + xlab("Standardized Residuals") + coord_cartesian(xlim = c(-10, 10))
    
    
    
    
  })
  
  output$subSummary <- renderTable({
    if(input$selectVariety == "All")
      USwine <- subset(wine, wine$country == input$selectCountry)
    if(input$selectVariety != "All")
      USwine <- subset(wine, wine$country == input$selectCountry & wine$variety == input$selectVariety)
    table <- stat.desc(USwine[c("price", "points")])
    table$Statistic = c("Number of Values", "Number of Nulls", "Number of NAs", "min", "max", "range", "sum", "median", "mean", "SE.mean", "CI.mean.0.95", "var", "std.dev", "coef.var")
    table <- table[c(3,1,2)]
    table
  })
  
}

