####  shinyElicit server

require("shiny")
require("xtable")
shinyServer(function(input, output, session) {

  #source("conveniences.R", local=TRUE)
  source("../debugTools.R", local=TRUE)
  source("plotDiscomfort.R", local=TRUE)
  NNTgap = 1
  observe( {
    updateNumericInput(session, inputId="NNTpos",
                       value=min(input$NNTpos,
                                 input$NNTlower-NNTgap),
                       max = input$NNTlower-NNTgap)
  })
  observe( {
    updateNumericInput(session, inputId="NNTlower",
                       value=max(input$NNTlower,
                                 input$NNTpos+NNTgap),
                       min = input$NNTpos+NNTgap)
  })
  observe( {
    updateNumericInput(session, inputId="NNTneg",
                       value=max(input$NNTneg,
                                 input$NNTupper+NNTgap),
                       min = input$NNTupper+NNTgap)
  })
  observe( {
    updateNumericInput(session, inputId="NNTupper",
                       value=min(input$NNTupper,
                                 input$NNTneg-NNTgap),
                       max = input$NNTneg-NNTgap)
  })
  output$plotDiscomfort = renderPlot({
  plotDiscomfort(  NNTlower = input$NNTlower,
                   NNTupper = input$NNTupper,
                   NNTpos = input$NNTpos,
                   NNTneg = input$NNTneg)
  })
})


