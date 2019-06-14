library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel = headerPanel(title="iris hist"),
    mainPanel = mainPanel(
      plotOutput("distPlot")
    ),
    sidebarPanel = sidebarPanel()
  )
)
