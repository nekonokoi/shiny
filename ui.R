library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel = headerPanel(title="iris hist"),
    mainPanel = mainPanel(
    tableOutput("choice_table"),
    plotOutput("multi.chart"),
    tableOutput(
      outputId = "view"
    ),
      plotOutput("scatterPlot"),
      tableOutput(
        outputId = "regOutput"
      ),
            plotOutput("distPlot"),
      plotOutput("qqPlot")
    ),

    sidebarPanel = sidebarPanel(
    selectInput("select_file", label = "fileリスト", choices = ""),

    selectInput("select_table", label = "テーブルリスト", choices = ""),
      #引数としてつないでいく
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
          ),

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
    )
  )
)
