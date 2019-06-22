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
                selectInput("select_file", label = "fileリスト", choices = ""),
                selectInput("file_delim",label="ファイル形式",choices=c(',',' ','\t')),
                actionButton(inputId="useButton", label="このデータを使う", icon = NULL, width = NULL)
              ),
              mainPanel=mainPanel(
                h1('aaa'),
                tableOutput("choice_file")
              )
            )
          ),
          tabPanel("カラム情報",
            plotOutput("pairPlot"),
            plotOutput("multi.chart",height="1000px")
          ),

          tabPanel("アナリティクス",
            tabsetPanel(
              tabPanel("検定",
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    selectInput(
                      inputId="test_name",
                      label="テスト種類",
                      choices=c("対応ありt検定")
                    ),
                    actionButton(inputId="testButton", label="検定を行う", icon = NULL, width = NULL)
                  ),
                  mainPanel=mainPanel(
                    verbatimTextOutput("testText")
                  )
                )
              ),
              tabPanel("回帰",
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    checkboxGroupInput(
                      inputId="xs",
                      label="説明変数",
                      choices=""
                    ),
                    selectInput(
                      inputId="y",
                      label="目的変数",
                      choices=""
                    ),
                    actionButton(inputId="regButton", label="回帰を行う", icon = NULL, width = NULL)
                  ),
                  mainPanel=mainPanel(
                    verbatimTextOutput("regText"),
                    tableOutput(outputId = "regOutput")
                    #plotOutput("distPlot"),
                    #plotOutput("qqPlot")
                  )
                )
              ),
              tabPanel("決定木",
                sidebarLayout(
                  sidebarPanel=sidebarPanel(actionButton("rpartButton",label="rpart")),
                  mainPanel=mainPanel(
                    verbatimTextOutput("rpartText"),
                    plotOutput("rparttext")
                  )
                )
              ),
              tabPanel("ロジット",
                sidebarLayout(
                  sidebarPanel=sidebarPanel(actionButton("logitButton",label="logit")),
                  mainPanel=mainPanel(
                    verbatimTextOutput("logitText")
                  )
                )
              ),
              tabPanel("クラスタリング",
                sidebarLayout(

                  sidebarPanel=sidebarPanel(
                    actionButton("clustButton",label="クラスタリング"),
                    selectInput(
                      inputId="n_cls",
                      label="クラスター数",
                      choices=c(2,3,4,5,6)
                    )
                  ),
                  mainPanel=mainPanel(
                    verbatimTextOutput("clsText")
                  )
                )
              ),
              tabPanel("アプリオリ",
                sidebarLayout(

                  sidebarPanel=sidebarPanel(
                    actionButton("aprioriButton",label="アプリオリ")
                  ),
                  mainPanel=mainPanel(
                    verbatimTextOutput("aprioriText1"),
                    plotOutput("aprioriPlot"),
                    verbatimTextOutput("aprioriText2"),
                    tableOutput("aprioriRule"),
                    plotOutput("aprioriNetwork")


                  )
                )
              ),
              tabPanel("時系列",
                sidebarLayout(

                  sidebarPanel=sidebarPanel(
                    selectInput(
                      "tsTime",
                      "時系列カラム",
                      choices = c("7","30","12","365")
                    ),
                    textInput(
                      "tsStart",
                      label="開始時期",
                      value=""
                    ),
                    actionButton("tsButton",label="ARIMA")

                  ),
                  mainPanel=mainPanel(
                    plotOutput("tsDecompose"),
                    verbatimTextOutput("adfTest"),
                    plotOutput("pacf"),
                    verbatimTextOutput("arimaFit")



                  )
                )
              ),
              tabPanel("生存時間",
                sidebarLayout(

                  sidebarPanel=sidebarPanel(
                    actionButton("suvButton",label="生存時間")

                  ),
                  mainPanel=mainPanel(
                    plotOutput("suvPlot"),
                    verbatimTextOutput("suvText")
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
