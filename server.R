library(shiny)
library(dplyr)
iris <- iris
mdl=lm(Sepal.Length~Sepal.Width,data=iris)
res=summary(mdl)

shinyServer(function(input,output){

output$multi.chart<-renderPlot({
  par(mfrow=c(3,2))
  name <- c(
    #
    "Sepal.Length",
    "Sepal.Width",
    "Petal.Length",
    "Petal.Width",
    "Species"

    )
  for(n in 1:length(name)){
    if(
      class(iris[,name[n]]) == "numeric"
    ){
    hist(iris[,name[n]])
    }else if(class(iris[,name[n]]) == "factor"){
      d<-iris %>% select(name[n])%>% group_by_(name[n]) %>% summarize(n = n())
      barplot(d$n,horiz=T)
    }
  }
})

# 事前に使うデータをつくる
data <- reactive({
  iris[,c(input$x,input$y)]
})

output$view <- renderTable(
head(data())
)

# 事前に使うモデルを作る
# reactiveとisolate
res <- reactive({
i1 <- iris
x<-iris[,input$x]
y<-iris[,input$y]

mdl=lm(y~x)
res=summary(mdl)
return(res)
})
  output$scatterPlot <- renderPlot(
    {
      plot(data())
    }
  )
  #代入なので,はいらない
  output$string1=renderText(input$string1)
  output$distPlot<-renderPlot(
    {
      hist(res()$residuals)
    }
  )

  output$regOutput=renderTable(
    {
    a <- res()
    a$coefficients
    }
  )

  output$qqPlot<-renderPlot(
    {
      x<-data()[,1]
      y<-data()[,2]
      qqplot(x,y)
    }
  )

})
