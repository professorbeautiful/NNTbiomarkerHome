library(shiny)

shinyUI(fluidPage(
  titlePanel("hhhhh"), 
  
  mainPanel( 
    plotOutput(outputId = "skislope"), 
    plotOutput(outputId = "AEstack")
    )
  
))
  
  