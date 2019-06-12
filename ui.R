library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel = headerPanel(title="title"),
    mainPanel = mainPanel(),
    sidebarPanel = sidebarPanel()
  )
)
