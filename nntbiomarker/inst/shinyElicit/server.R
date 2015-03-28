require("shiny")
require("xtable")
shinyServer(function(input, output, session) {
  observe( {
    cat("updating numerical input\n")
    updateNumericInput(session, inputId="NNTpos",
                       max = input$NNTlower)
    updateNumericInput(session, inputId="NNTneg",
                       min = input$NNTupper)
  }       )
})


