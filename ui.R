library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel = headerPanel(title="iris hist"),
    mainPanel = mainPanel(
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
      #引数としてつないでいく
      textInput(
        inputId = "string1",
        label = "テキスト1を入力してください。",
        value="",
        placeholder="place",
        width=NULL
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
