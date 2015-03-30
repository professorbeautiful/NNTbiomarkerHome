####  shinyElicit server

require("shiny")
require("xtable")
shinyServer(function(input, output, session) {

  #source("conveniences.R", local=TRUE)
  source("../debugTools.R", local=TRUE)
  source("plotDiscomfort.R", local=TRUE)
  observe( {
    cat("updating numerical input\n")
    updateNumericInput(session, inputId="NNTpos",
                       max = input$NNTlower)
    updateNumericInput(session, inputId="NNTneg",
                       min = input$NNTupper)
  })
  output$plotDiscomfort = renderPlot({
  plotDiscomfort(  NNTlower = input$NNTlower,
                   NNTupper = input$NNTupper)
}
  )
})


