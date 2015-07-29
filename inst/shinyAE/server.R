library(shiny)

shinyServer(function(input, output) {
  rV = reactiveValues(RSclicked = 20)
  output$skislope = renderPlot({
    skisloplot(rV$RSclicked)
    })
  output$AEstack = renderPlot({
    AEplot(rV$RSclicked)
  })
  observe({
    rV$RSclicked = try(input$skislope_hover$x)
    if(class(rV$RSclicked) == "try-error")
      rV$RSclicked = 0
    cat("The observe function is run, and the value is ", rV$RSclicked)
  })
})
