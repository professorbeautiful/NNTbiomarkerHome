shinyUI(basicPage(
  h1("Achievable sensitivity and specificity"),
  numericInput("prevalence", label = "prevalence",
               value=0.5, min = 0, max=1, step = 0.05),
  plotOutput("myplot", hoverId="myplot_hover"),
  tableOutput("hoverinfo")
))
