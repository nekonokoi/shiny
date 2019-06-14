library(shiny)
iris <- iris

shinyServer(function(input,output){
  output$distPlot <- renderPlot(
    {
      hist(iris$Sepal.Length)
    }
  )
  #代入なので,はいらない
  output$string1=renderText(input$string1)

})
