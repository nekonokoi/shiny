library(shiny)
iris <- iris

shinyServer(function(input,output){
  output$distPlot <- renderPlot(
    {
      hist(iris$Sepal.Length)
    }
  )

})
