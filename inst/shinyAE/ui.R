library(shiny)

shinyUI(fluidPage(
  titlePanel("Chemotherapy Adverse Events"), 
  
  fluidRow(
    numericInput(inputId = "RSchosen", label = "yayaya", value = 30, min = 1, max = 50),
    actionButton("ODXlow", "ODXlow RS = 17"),
    actionButton("ODXhigh", "ODXhigh RS = 30"),
    actionButton("TailorX_low", "TailorX_low = 11"),
    actionButton("TailorX_high", "TailorX_high = 25")
  ),
  fluidRow(
      
    column(6,  
           plotOutput(outputId = "skislope", 
                      click="plot_click",
                      hover="skislope_hover")), 
    column(6, 
           plotOutput(outputId = "AEstack")
    )
  )
))
  
  