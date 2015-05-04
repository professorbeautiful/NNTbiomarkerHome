#####  shinyElicit ui

shinyUI(fluidPage(
  tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
        function(message) {
          console.log(message)
          eval(message.code);
        }
      );
    '))),
  uiOutput("debugTools"),
  h1("Biomarker validation study design support"),
  a(href="../doc/Using_the_NNTbiomarker_package.html",
    fluidRow(column(1, offset=2, strong(em("click for information:",
                                           style="color:lightgreen")))
             ,
             column(3, actionButton(inputId = "Info", label="",
                                    style="background:lightgreen",
                                    icon=icon("info-sign", lib="glyphicon")))
    )
  ),
  hr(),
  actionButton(inputId = "reportButton",
               label = "When all steps are Done, you can click here for a report. (In Progress)"),
  div(style="background:darkGrey",
      checkboxInput(inputId='stepTableCheckbox', value=FALSE,
                    label=em(strong("NNT design table of stepping stones"))),
      conditionalPanel('input.stepTableCheckbox',
                       tableOutput("steps")
      )
  ),
  hr(),
  div(style="position:relative;overflow:scroll;height:1200px;background:lightgrey",
      sectionHeader(1,
                    div(style="vertical-align:middle;font-size:150%",
                        HTML(stringr::str_dup("&nbsp;", 15)),
                        "Intended beneficiaries", tags$textarea(id = "who"),
                        HTML(stringr::str_dup("&nbsp;", 15)),
                        " Decision choices (two)", tags$textarea(id = "options" )
                    )),
      sectionHeader(2, div(
        fluidRow(
          #column(0, HTML("&nbsp;")),
#           column(4, sliderInput("NNTlower", label = "NNTlower",
#                                  value=7, min = 1, max=100, step=1)),
#           column(4, sliderInput("prevalence", label = "prevalence = Pr(BestToAct) = 1/NNT",
#                                  value=0.1, min = 0, max=1, step=0.05)),
#           column(4, sliderInput("NNTupper", label = "NNTupper",
#                                  value=17, min = 10, max=100, step=1))
          column(4, numericInput("NNTlower", label = "NNTlower",
                                 value=7, min = 1,  step=1)),
          column(4, numericInput("prevalence", label = "prevalence = Pr(BestToAct) = 1/NNT",
                                 value=0.1, min = 0, max=1, step=0.01)),
          column(4, numericInput("NNTupper", label = "NNTupper",
                                 value=17, min = 2, step=1))
        ),
        plotOutput(outputId = "plotDiscomfort",
                   height='200px')
      )),
      sectionHeader(3, div(
        fluidRow(
          column(2, HTML("&nbsp;")),
          column(5, numericInput("NNTpos", label = "NNTpos, must be smaller than NNTlower",
                                 value=1, min = 1, step=1)),
          column(5, numericInput("NNTneg", value=30, label = "NNTneg, must be larger than NNTupper", min = 1, step=1))
        ),
        plotOutput(outputId = "plotNNTgoals",
                   height='250px')
      )),
      sectionHeader(4, div(
        "DELETE ME"
      )),
      sectionHeader(5,  div(
        "Positive predictive value = 1/NNTpos = ",
        textOutput("PPVderived"),
        br(),
        "Negative predictive value = 1 - 1/NNTneg = ",
        textOutput("NPVderived")
      )),
      sectionHeader(6,
                    numericInput("samplesize", label = "Prospective study sample size",
                                 value=30, min = 10, max=1000, step = 1)
      ),
      sectionHeader(7, div(
        h3("Required sensitivity and specificity for a retrospective study."),
        h2("Select the prevalence; hover mouse on plot for calculations."),
        numericInput("prevalence", label = "prevalence",
                     value=0.5, min = 0, max=1, step = 0.05),
        fluidRow(column(6,
                        plotOutput("contraBayesPlot",
                                   clickId="contraBayesPlot_click",
                                   hoverId="contraBayesPlot_hover",
                                   width='100%')),
                 column(6, tableOutput("selectedNNTPosNeg"),
                        numericInput("samplesizeCases",
                                     label = "Retrospective study #cases",
                                     value=30),
                        numericInput("samplesizeControls",
                                     label = "Retrospective study #controls",
                                     value=30),
                        h3("Anticipated results (TODO)")
                 )
        )
      ))  ### end 7
  ) # end scroll pane
))
