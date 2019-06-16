library(shiny)

shinyUI(
  navbarPage("MyR",
    tabPanel("アプリ1",
      mainPanel(
        tabsetPanel(
          tabPanel(
            "データ選択",
            sidebarLayout(
              sidebarPanel = sidebarPanel(
                selectInput("select_file", label = "fileリスト", choices = "")
              ),
              mainPanel=mainPanel(
                h1('aaa'),
                tableOutput("choice_file")
              )
            )
          ),
          tabPanel("カラム情報",
            plotOutput("multi.chart")
          ),

          tabPanel("page3",
            tabsetPanel(
              tabPanel("回帰",
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    selectInput(
                      inputId="x",
                      label="説明変数",
                      choices=c("Petal.Length","Petal.Width","Sepal.Length","Sepal.Width")
                    ),
                    selectInput(
                      inputId="y",
                      label="目的変数",
                      choices=c("Petal.Length","Petal.Width","Sepal.Length","Sepal.Width")
                    )
                  ),
                  mainPanel=mainPanel(
                    plotOutput("scatterPlot"),
                    tableOutput(outputId = "regOutput"),
                    plotOutput("distPlot"),
                    plotOutput("qqPlot")
                  )
                )
              ),
              tabPanel("残り",
                sidebarLayout(
                  mainPanel = mainPanel(
                    tableOutput("choice_table"),
                    tableOutput(outputId = "view")
                  ),
                  sidebarPanel = sidebarPanel(
                    selectInput("select_table", label = "テーブルリスト", choices = ""),
                    textInput(
                      inputId = "string1",
                      label = "テキスト1を入力してください。",
                      value="",
                      placeholder="place",
                      width=NULL
                    ),
                    textAreaInput(
                      inputId = "string2",
                      label = "sqlとか",
                      value="",
                      placeholder="place",
                      width="300px",
                      height="300px",
                      cols=NULL,
                      rows=NULL,
                      resize=NULL
                    )
                  )
                )
              )
            )
          )
        )#tabsetPanel
      )#mainPanel
    )
  )
)
