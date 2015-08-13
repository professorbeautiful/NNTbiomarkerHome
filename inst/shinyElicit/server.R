####  shinyElicit server
appName = "shinyElicit"
cat("Launching ", appName, "\n")

require("shiny")
require("xtable")
require("NNTbiomarker")
require("knitr")

shinyServerFunction =
  function(input, output, session) {
    thisSession <<- session

    #source("conveniences.R", local=TRUE)
    source("debugTools.R", local=TRUE)
    source("contraBayesPlot.R", local=TRUE)


    observe({
      if(!is.null(input$contraBayesPlot_click)) {
        ppv = input$contraBayesPlot_click$x
        npv = input$contraBayesPlot_click$y
        nnts = pv.to.NNT(ppv = ppv, npv = npv)
        catn("nnts observed: ", nnts[[1]], nnts[[2]])
        if(all(!is.nan(nnts))) {
          updateNumericInput(session, inputId="NNTneg",
                             value=round(nnts[[2]]))
          updateNumericInput(session, inputId="NNTpos",
                             value=round(nnts[[1]]))
        }
       }
    })
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
               if( ! is.null(newValue)) {
                 catn("Toggling stepStatus" %&% number %&% " = " %&% newValue)
                 isolate({ # necessary, or else crash!
                   rValues$stepsTable[number, "Done?"] = newValue
                   catn("New value in stepsTable: ",
                        rValues$stepsTable[number, "Done?"] )
                 })
               }
             }
             )
      )
      #     cat("obs" %&% number %&% " is where? ",
      #         find("obs" %&% number))
      #     print(getAnywhere("obs" %&% number))
    }
    obsList = lapply(1:nrow(stepsTableInitial), obs)
    # find(obs1) # not found!
    #print(identical(obs1, obs2))
    #print(identical(obs1, obsList[[1]]))
    #print(identical(obsList[[1]], obsList[[7]]))
    ###
    ### ### must assign, or else only the last will take.

    observe({
      if(input$who == "" | input$options == "")
        try(disableActionButton("stepStatus1", session))
    })
    observe({
      try(
        if(!all(sapply(1:nrow(stepsTableInitial),
                       function(n) "Done"==
                       input[["stepStatus" %&% n]])))
          disableActionButton("reportButton", session)
        else
          enableActionButton("reportButton", session)
      )
    })

    source("plotDiscomfort.R", local=TRUE)

    NNTgap = 1
#     observe( {
#       updateNumericInput(session, inputId="NNTpos",
#                          value=min(input$NNTpos,
#                                    isolate(input$NNTlower-NNTgap)),
#                          max = isolate(input$NNTlower-NNTgap))
#     })
#     observe( {
#       updateNumericInput(session, inputId="NNTlower",
#                          value=max(input$NNTlower,
#                                    isolate(input$NNTpos+NNTgap)),
#                          min = isolate(input$NNTpos+NNTgap))
#     })
#     observe( {
#       updateNumericInput(session, inputId="NNTneg",
#                          value=max(input$NNTneg,
#                                    isolate(input$NNTupper+NNTgap)),
#                          min = isolate(input$NNTupper+NNTgap))
#     })
#     observe( {
#       updateNumericInput(session, inputId="NNTupper",
#                          value=min(input$NNTupper,
#                                    isolate(input$NNTneg-NNTgap)),
#                          max = isolate(input$NNTneg-NNTgap))
#     })
    output$plotDiscomfort = renderPlot({
      plotDiscomfort(drawPosNeg=FALSE,
                     NNTlower = input$NNTlower,
                     NNTupper = input$NNTupper)
    }
    #, height=280
    )

    PPVderived = reactive({1/input$NNTpos})
    output$PPVderived = renderText({PPVderived()})
    NPVderived = reactive({1 - 1/input$NNTneg})
    output$NPVderived = renderText({NPVderived() })

    output$plotNNTgoals = renderPlot({
      plotDiscomfort(drawPosNeg=TRUE,
                     NNTlower = input$NNTlower,
                     NNTupper = input$NNTupper,
                     NNTpos = input$NNTpos,
                     NNTneg = input$NNTneg)
    }
    #, height=280
    )
    wasClicked =  function(button) {
      if(exists("input"))
        if(!is.null(button) ) {
          if(button > 0) {
            return(TRUE)
          }
        }
      return(FALSE)
    }
    autoFillObserver = observe({
      cat("==> autoFillObserver\n")
      if(wasClicked(input$autoFill) ) {
        updateTextInput(session, "biomarkerReportTitle",
                        value="DEMO report")
        updateTextInput(session, "objective",
                        value="Prognosis of cutaneous T cell
                        lymphoma (CTCL). In early stages of CTCL, patients (Stages IA-IIA)
                        usually do well and have slowly progressive disease, which does not
                        require aggressive therapy associated with substantial side effects.
                        However, about 15% of these patients have unexpected progressive course
                        and rapid demise.")
        updateTextInput(session, "who",
                        value="CTCL patients in Stages IA-IIA")
        updateTextInput(session, "options",
                        value="Watchful waiting\nAggressive treatment")
        updateTextInput(session, "benefit",
                        value="A biomarker progression risk model
                      that is able to classify patients into high and low risk groups will
                      enable personalized and more aggressive therapy for the patients at
                      highest risk for progression.")
        for(stepNum in 1:env_sectionHeader$number)
          updateRadioButtons(session, "stepStatus" %&% stepNum, selected="Done")
      }
    })
    assembleReportObserver = observe({
      cat("==> assembleReportObserver\n")

      ### Only react when the reportButton is clicked.
      if(wasClicked(input$reportButton)) {
        Objective = input$objective
        cat("input$options = ", capture.output(input$options), '\n')
        Option_1 <<- strsplit(input$options, "\n")[[1]] [1]
        Option_2 <<- strsplit(input$options, "\n")[[1]] [2]
        NNTlower <<- input$NNTlower
        NNTupper <<- input$NNTupper
        NNTpos <<- input$NNTpos
        NNTneg <<- input$NNTneg
        Benefit <<- input$benefit

        cat('getOption("markdown.HTML.options")',
            capture.output(getOption("markdown.HTML.options")), '\n')
        cat('getOption("markdown.extensions")',
            capture.output(getOption("markdown.extensions")), '\n')
        knit2html("www/Steps.Rmd", output = "www/Steps.html")
        # browseURL("Steps.html") ### Fails at shinyapp.io.
        #window.open("Steps.html"); # JS works if file is in www folder.
        print(getwd())
        print(dir())
      }
      # Ideally, simulate click on "markdownAnchor". or as a form?

    })
  }
#debug(shinyServerFunction)
shinyServer(func=shinyServerFunction)


