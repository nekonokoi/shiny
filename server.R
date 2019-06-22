library(shiny)
library(dplyr)
library(ggplot2)
library(rpart)
library(tidyverse)
library(arules)
library(arulesViz)
library(tseries)
library(forecast)
library(survival)


iris <- iris
mdl=lm(Sepal.Length~Sepal.Width,data=iris)
res=summary(mdl)
library("RSQLite")
con = dbConnect(SQLite(), "./db/myr.db", synchronous="off")


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
  delim = input$file_delim
  #dat<-read.csv(path,sep=delim)
  dat<-data.frame(read_delim(path,delim=delim))
  dat
})

output$choice_file<-renderTable({
head(f_res())
})



output$choice_table<-renderTable({
q_res()
})

output$multi.chart<-renderPlot({

  dat <- f_res()
  name <- names(dat)
  rows <- ceiling(length(name)/2)
  par(mfrow=c(rows,2))
  for(n in 1:length(name)){
    print(class(dat[,name[n]]))
    if(
      class(dat[,name[n]]) == "numeric"
      | class(dat[,name[n]]) == "integer"
    ){
    hist(dat[,name[n]],main=name[n])
    }else if(class(dat[,name[n]]) == "factor"
      |class(dat[,name[n]]) == "character"
      ){
      d<-dat %>% select(name[n])%>% group_by_(name[n]) %>% summarize(n = n())
      tryCatch(
        {
          d2<-d %>% mutate_(ch=as.character(name[n])) %>% mutate(date=as.Date(ch))

plot(x=d2$date,y=d2$n,main=name[n])
        },
        error=function(e){

          d<-dat %>% select(name[n])%>% group_by_(name[n]) %>% summarize(n = n())
          barplot(d$n,horiz=T,main=name[n])
        }
      )
      }
      else if(class(dat[,name[n]]) == "Date"){
        d<-dat %>% select(name[n])%>% group_by_(name[n]) %>% summarize(n = n())
        plot(x=d[,name[n]],y=d$n,main=name[n])

      }
    }
})
# 回帰
reg_y_choices <-  ""
observeEvent(input$useButton,{
  reg_y_choices<-{names(f_res())}
  updateSelectInput(session, "y", choices = reg_y_choices)

  updateCheckboxGroupInput(
    session,
    inputId="xs",
    label = "説明変数",
    choices = reg_y_choices,
    selected = NULL,
    inline = FALSE
    #choiceNames = NULL,
    #choiceValues = NULL
    )
})


res<-reactiveValues()
observeEvent(input$regButton,{
  dat <- data.frame(f_res())
  xs<-paste(input$xs,collapse="+")
  mdl<-lm(
    formula=as.formula(paste0(input$y,"~",xs)),
    data=dat
  )
  res$res<-summary(mdl)
})

logit_res<-reactiveValues()
observeEvent(input$logitButton,{
  dat <- data.frame(f_res())
  xs<-paste(input$xs,collapse="+")
  res<-glm(
    formula=as.formula(paste0(input$y , '~',xs)),
    data=dat,
    family=binomial
  )
  logit_res$res<-res
}
)

output$logitText<-renderPrint({
  summary(logit_res$res)

}
)
test_res<-reactiveValues()
observeEvent(input$testButton,{
  a<-f_res()[1]
  b<-f_res()[2]
  res<-t.test(a,b,var.equal=T)
  test_res$res<-res

})
output$testText<-renderPrint({
  test_res$res
})
clust_res<-reactiveValues()
observeEvent(input$clustButton,{
  cls <- kmeans(f_res(),as.numeric(input$n_cls))
  clust_res$res<-cls


})
output$clsText<-renderPrint({
  clust_res$res

})

rpart_res<-reactiveValues()
observeEvent(input$rpartButton,{

  dat <- data.frame(f_res())
    xs<-paste(input$xs,collapse="+")

  res<-rpart(
    formula = as.formula(paste0(input$y , '~',xs)),
    data = dat,
    method = 'class',
    parms = list(split='information')
  )
  rpart_res$res<-res
}
)
output$rpartText<-renderPrint({
  summary(rpart_res$res)

}
)

output$rparttext<-renderPlot(
  {
  plot(rpart_res$res)
  text(rpart_res$res)
  }
)


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
  output$regText<-renderPrint({
    res$res

  })
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
  apriori_res <- reactiveValues()
  observeEvent(input$aprioriButton,{
    #データの作成
    apriori_res$dat <- as(as.matrix(f_res()),"transactions")
    apriori_res$rule <- apriori(apriori_res$dat)



  })

  output$aprioriText1 <- renderPrint({
    summary(apriori_res$dat)
  })

  output$aprioriPlot <- renderPlot({
    itemFrequencyPlot(apriori_res$dat)
  })

  output$apiroriText2 <- renderPrint({
    summary(apriori_res$rule)

  })
  output$aprioriRule <- renderTable({
    inspect(head(apriori_res$rule,30))
  })

  output$aprioriNetwork<-renderPlot({
    plot(apriori_res$rule,
      method="graph",
      control=list(type="items",cex=2)
    )
  })


  ts_res <- reactiveValues()

  observeEvent(input$tsButton,{
    dat<-f_res()
    ts_res$dat <-ts(
      dat[,input$y],
      start=as.numeric(input$tsStart),
      frequency=as.numeric(input$tsTime)
    )
    print(ts_res)

  })

  output$tsDecompose <- renderPlot({
    plot(decompose(ts_res$dat))
  })

  output$adfTest<- renderPrint({
    adf.test(ts_res$dat)
  })

  output$pacf <- renderPlot({
    pacf(ts_res$dat)
  })

  output$arimaFit <- renderPrint({
    ts_res$arimaFit <- auto.arima(ts_res$dat,ic="aic",trace=F,stepwise=F,approximation=F,allowmean=F,allowdrift=F)
    ts_res$arimaFit


  })

  output$pairPlot <- renderPlot({
    f <- function(x) {
      if(is.numeric(x)){
        return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
    select_if(f_res(), f) %>% pairs()

  })

  suv_res <- reactiveValues()

  observeEvent(input$useButton,{
      suv_dat<-{names(f_res())}
      updateSelectInput(session, "suv_y", choices = suv_dat)
      updateSelectInput(session, "suv_censor", choices = suv_dat)

      updateCheckboxGroupInput(
        session,
        inputId="suv_xs",
        label = "説明変数",
        choices = suv_dat,
        selected = NULL,
        inline = FALSE
        #choiceNames = NULL,
        #choiceValues = NULL
        )
    })

  observeEvent(input$suvButton,{
    dat<-f_res()
    ys<-c(input$suv_y,input$suv_censor)
    xs<-paste(input$suv_xs,collapse="+")
    suv_y <- paste(ys,collapse=",")

    f<-paste0("Surv(",suv_y,")","~",xs)


    mdl<-coxph(
      formula=as.formula(f),
      method="breslow",
      data=dat
    )
    suv_res$res<-mdl

  })

  output$suvText<-renderPrint({
      summary(suv_res$res)
  })

  output$suvPlot<-renderPlot({
    kidney.fit <- survfit(suv_res$res)
    plot(kidney.fit)
  })

    cross_res <- reactiveValues()
    observeEvent(input$useButton,{
        cross_dat<-{names(f_res())}
        updateSelectInput(session, "cross_y", choices = cross_dat)
        updateSelectInput(session, "cross_x", choices = cross_dat)

        })

    observeEvent(input$crossButton,{
      x <- input$cross_x
      y <- input$cross_y

      cross_res$res <- xtabs(
        as.formula(paste0("~",y,"+",x)),
        data=f_res()
      )
    })

    output$crossTable <- renderPrint({
      print(cross_res$res)
    })

    output$crossTest <- renderPrint({
      summary(cross_res$res)
    })


})
