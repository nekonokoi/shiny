library(shiny)
library(dplyr)
library(ggplot2)

iris <- iris
mdl=lm(Sepal.Length~Sepal.Width,data=iris)
res=summary(mdl)
library("RSQLite")
con = dbConnect(SQLite(), "~/Desktop/test", synchronous="off")


shinyServer(function(input,output,session){
table.lists<- dbListTables(con)
observe({
  updateSelectInput(session, "select_table", choices = table.lists)
})

q_res <- reactive({
if(input$select_table == ""){
  res<-iris
}else{
q <- paste("select * from ",input$select_table)

res <- dbGetQuery(con, q)
}
  res
})

file.list <- list.files('./data/')
observe({
  updateSelectInput(session, "select_file", choices = file.list)
})

f_res <-reactive({
  path<-paste0('./data/',input$select_file)
  print(path)
  dat<-read.csv(path)
  dat
})

output$choice_file<-renderTable({
head(f_res())
})



output$choice_table<-renderTable({
q_res()
})

output$multi.chart<-renderPlot({
  par(mfrow=c(3,2))
  dat <- f_res()
  name <- names(dat)
  for(n in 1:length(name)){
    print(class(dat[,name[n]]))
    if(
      class(dat[,name[n]]) == "numeric"
      | class(dat[,name[n]]) == "integer"
    ){
    hist(dat[,name[n]])
    }else if(class(dat[,name[n]]) == "factor"){
      d<-dat %>% select(name[n])%>% group_by_(name[n]) %>% summarize(n = n())
      tryCatch(
        {
          d2<-d %>% mutate_(ch=as.character(name[n])) %>% mutate(date=as.Date(ch))

plot(x=d2$date,y=d2$n)
        },
        error=function(e){

          d<-dat %>% select(name[n])%>% group_by_(name[n]) %>% summarize(n = n())
          barplot(d$n,horiz=T)
        }
      )
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
