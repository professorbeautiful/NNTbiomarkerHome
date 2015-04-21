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
  fixedRow(column(1, offset=2, em("click for information:")),
           column(3, submitButton("Info",
                        icon=icon("glyphicon-info-sign", lib="glyphicon")
           ))),
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
          column(2, HTML("&nbsp;")),
          column(5, numericInput("NNTlower", label = "NNTlower",
                                 value=7, min = 1, max=10, step=1)),
          column(5, numericInput("NNTupper", label = "NNTupper",
                                 value=17, min = 10, max=100, step=1))
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
                    numericInput("samplesize", label = "sample size",
                                 value=30, min = 10, max=1000, step = 1)
      ),
      sectionHeader(7, div(
        h3("Required sensitivity and specificity"),
        ## TODO: input$prevalence is not changing.
        numericInput("prevalence", label = "prevalence",
                     value=0.5, min = 0, max=1, step = 0.05),
        fluidRow(column(6,
                        plotOutput("contraBayesPlot",
                                   hoverId="contraBayesPlot_hover",
                                   width='100%')),
                 column(6, tableOutput("selectedNNTPosNeg"))
        )
      ))  ### end 7
  ) # end scroll pane
))
