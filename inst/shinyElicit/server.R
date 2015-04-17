####  shinyElicit server

require("shiny")
require("xtable")
require("NNTbiomarker")

shinyServer(
  #   shinyServerFunction)
  # shinyServerFunction = (
  function(input, output, session) {
    thisSession <<- session

  #source("conveniences.R", local=TRUE)
  source("debugTools.R", local=TRUE)
  source("contraBayesPlot.R", local=TRUE)

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
  obs = function(number) {
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
#     cat("obs" %&% number %&% " is where? ",
#         find("obs" %&% number))
#     print(getAnywhere("obs" %&% number))
  }
  obsList = lapply(1:7, obs)
  # find(obs1) # not found!
  #print(identical(obs1, obs2))
  #print(identical(obs1, obsList[[1]]))
  #print(identical(obsList[[1]], obsList[[7]]))
  ###
      ### ### must assign, or else only the last will take.

  observe({
    if(input$who == "" | input$options == "")
      disableActionButton("stepStatus1", session)
  })
  observe({
    if(!all(sapply(1:7,
                   function(n) "Done"==
                     input[["stepStatus" %&% n]])))
      disableActionButton("reportButton", session)
    else
      enableActionButton("reportButton", session)
  })

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
    plotDiscomfort(drawPosNeg=FALSE,
                   NNTlower = input$NNTlower,
                   NNTupper = input$NNTupper)
  },
  height=280
  )
  output$plotNNTgoals = renderPlot({
    plotDiscomfort(drawPosNeg=TRUE,
                   NNTlower = input$NNTlower,
                   NNTupper = input$NNTupper,
                   NNTpos = input$NNTpos,
                   NNTneg = input$NNTneg)
    },
    height=280
  )
  })
#debug(shinyServerFunction)
#shinyServer(shinyServerFunction)


