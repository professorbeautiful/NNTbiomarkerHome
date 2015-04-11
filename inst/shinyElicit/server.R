####  shinyElicit server

require("shiny")
require("xtable")
shinyServer(
  #   shinyServerFunction)
  # shinyServerFunction = (
  function(input, output, session) {
    thisSession <<- session

  #source("conveniences.R", local=TRUE)
  source("../debugTools.R", local=TRUE)

  rValues = reactiveValues(
    stepsTable = stepsTableInitial )

  output$steps = renderTable({
    #catn("Calling renderTable on stepsTable");
    rValues$stepsTable
  })

  #  for(number in 1:nrow(stepsTableInitial)) {
  #     output[["completedText" %&% number]] =
  #       renderText({ rValues$stepsTable[number, "Done?"]
  #                    })
  obs = function(number)
    assign("obs" %&% number,
           observe({
             newValue <- input[["stepStatus" %&% number]]
             catn("Toggling stepStatus" %&% number %&% " = " %&% newValue)
             isolate({ # necessary, or else crash!
               rValues$stepsTable[number, "Done?"] = newValue
               catn("New value in stepsTable: ",
                    rValues$stepsTable[number, "Done?"] )
             })
           }
           )
    )
  lapply(1:7, obs)  ### for loop won't work.
      ### ### must assign, or else only the last will take.
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
#debug(shinyServerFunction)
#shinyServer(shinyServerFunction)


