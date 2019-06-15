library(shiny)
iris <- iris
mdl=lm(Sepal.Length~Sepal.Width,data=iris)
res=summary(mdl)

shinyServer(function(input,output){

output$view <- reactiveTable(function() {
head(iris[c(input$x,input$y)])
})
  output$scatterPlot <- renderPlot(
    {
      plot(iris$Sepal.Length,iris$Sepal.Width)
    }
  )
  #代入なので,はいらない
  output$string1=renderText(input$string1)
  output$distPlot<-renderPlot(
    {
      hist(res$residuals)
    }
  )

  output$regOutput=renderTable(
    {

    res$coefficients
    }
  )

  output$qqPlot<-renderPlot(
    {
      qqplot(iris$Sepal.Length, iris$Sepal.Width)
    }
  )

})
