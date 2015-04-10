####  shinyElicit server

require("shiny")
require("xtable")
shinyServer(
  #   shinyServerFunction)
  # shinyServerFunction = (
  function(input, output, session) {

  #source("conveniences.R", local=TRUE)
  source("../debugTools.R", local=TRUE)

  rValues = reactiveValues(
    stepsTable = stepsTableInitial,
    doneLabels = rep(buttonLabelValues[1], nrow(stepsTableInitial))
  )

  output$steps = renderTable({(rValues$stepsTable)})

  for(number in 1:nrow(stepTable)) {
    output[["Question" %&% number]] = textOutput("Question" %&% number)
  }

  sectionHeader = function(number) {
    list(h2(paste0('(', number, ') ',
                   textOutput(stepsTableInitial[number, "Stepping stone"]))),
         h3(textOutput("Question" %&% number, stepsTableInitial[iStep, "Question"])),
#         completedToggle(number)
    )
  }
#   completedToggle = function(number) {
#     span(
#       radioButtons("stepStatus" %&% number,
#                    label="Is this step done?", choices=c("Not yet", "Done"))
# #       textOutput(outputId = "completedText" %&% number,
# #                  "Not yet done."),
# #       actionButton("completed" %&% number,
# #                    label = "")
#     )
#   }
#   for(iStep in 1:nrow(stepsTableInitial)) {
# #     output[["completedText" %&% number]] =
# #       renderText({ rValues$stepsTable[number, `Done?`]
# #                    })
#     observeEvent(input[["stepStatus" %&% number]], ## radio buttons
#                  function() {
#                    rValues$stepTable[number, `Done?`] =
#                      input[["stepStatus" %&% number]]
#                  }
#     )
#   }
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


