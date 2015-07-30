shinyUI(ui = fluidPage(
  plotOutput("SkiSlopePlotID", hover = "skiSlopeHover", click = "skiSlopeClick"),
  plotOutput("AEplotID")
)
