library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel = headerPanel(title="iris hist"),
    mainPanel = mainPanel(
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
    textOutput(
      outputId = "string1"
      ,inline=FALSE
    )
    )
  )
)
