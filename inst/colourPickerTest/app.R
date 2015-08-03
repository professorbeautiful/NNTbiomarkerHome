source("RSdump.R")
library("shinyjs")
server = function(input, output, session) {
  output$barPlot = renderPlot({
    Opacity  = as.hexmode(input$opacity) / 100 * (16^2 - 1)
    smartBarPlot(split(RSsample, RSsampleBtoT),
                 boxfill=paste0(input$pickerId, Opacity), # For decreasing opacity
                 pointcol=input$dotColorID,
                 groupNames=c("Did not recur", "Recurred"), ylab= "RS", xlab="Outcome")

  })
  output$pickerstring = renderText({input$pickerId})
}

ui = fluidPage(
  inputPanel(
    colourInput("pickerId", "color of boxes", value = "#FF0000",
                showColour = c("both", "text", "background"),
                allowTransparent = FALSE, "transparentText")
    ,
    sliderInput(inputId = "opacity", label = "Opacity", min = 10, max = 100, step = 10, value = 50),
    colourInput("dotColorID", "color of dots", value = "#FF0000",
                showColour = c("both", "text", "background"),
                allowTransparent = FALSE, "transparentText")
  )
  ,
  hr(),
  textOutput("pickerstring", container = div),
  hr(),
  plotOutput("barPlot")

)

shinyApp(ui = ui, server = server)
