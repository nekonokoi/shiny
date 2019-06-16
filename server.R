library(shiny)
library(dplyr)
library(ggplot2)
library(rpart)

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
# 回帰
reg_y_choices <-  ""
observeEvent(input$useButton,{
  reg_y_choices<-{names(f_res())}
  print(reg_y_choices)
  updateSelectInput(session, "y", choices = reg_y_choices)
})

res<-reactiveValues()
observeEvent(input$regButton,{
  dat <- data.frame(f_res())
  y<-dat[,input$y]
  mdl<-lm(y~.,data=dat)
  res$res<-summary(mdl)
})

rpart_res<-reactiveValues()
observeEvent(input$rpartButton,{

  dat <- data.frame(f_res())
  y<-dat[,input$y]

  res<-rpart(
    formula = y ~ .,
    data = dat,
    method = 'class',
    parms = list(split='information')
  )
  rpart_res$res<-res
}
)

output$rparttext<-renderPlot(
  {
  plot(rpart_res$res)
  }
)

# 事前に使うデータをつくる
data <- reactive({
  iris[,c(input$x)]
})

output$view <- renderTable(
head(f_res(),100)
)

# 事前に使うモデルを作る

  #代入なので,はいらない
  output$string1=renderText(input$string1)
  output$distPlot<-renderPlot(
    {
      hist(res$res$residuals)
    }
  )

  output$regOutput=renderTable(
    {
    a <- res$res$coefficients
    }
  )

  output$qqPlot<-renderPlot(
    {
      x<-data()[,1]
      y<-data()[,1]
      qqplot(x,y)
    }
  )

})
