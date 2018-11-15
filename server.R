library(tidyverse)
function(input, output, session) {
  output$plot1 <- renderPlot({
    iris %>%
      ggplot(aes(x = Sepal.Length, y = Petal.Length)) +
      geom_point()
  })
  output$plot2 <- renderPlot({
    iris %>%
      ggplot(aes(x = Sepal.Length, y = Petal.Length)) +
      geom_point()
  })
}