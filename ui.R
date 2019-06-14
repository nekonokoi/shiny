library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel = headerPanel(title="iris hist"),
    mainPanel = mainPanel(
      plotOutput("distPlot")
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
