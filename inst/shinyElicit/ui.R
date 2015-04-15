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
  hr(),
  actionButton(inputId = "reportButton",
               label = "When all steps are Done, \nclick here for report."),
  tableOutput("steps"),
  hr(),
  div(style="overflow:scroll;height:400px;background:lightgrey",
    sectionHeader(1),
    span("Intended beneficiaries", tags$textarea(id = "who"),
         "         Decision choices (two)", tags$textarea(id = "options" )
         ),
    sectionHeader(2),
    fluidRow(
      column(2, HTML("&nbsp;")),
      column(5, numericInput("NNTlower", label = "NNTlower",
                             value=7, min = 1, max=10, step=1)),
      column(5, numericInput("NNTupper", label = "NNTupper",
                             value=17, min = 10, max=100, step=1))
    ),
    fluidRow(
      column(2, HTML("&nbsp;")),
      column(5, numericInput("NNTpos", label = "NNTpos, must be smaller than NNTlower",
                             value=6, min = 1, step=1)),
      column(5, numericInput("NNTneg", value=18, label = "NNTneg, must be larger than NNTupper", min = 1, step=1))
    ),
    plotOutput(outputId = "plotDiscomfort"),
    numericInput("samplesize", label = "sample size",
                 value=30, min = 10, max=1000, step = 1),
    numericInput("prevalence", label = "prevalence",
                 value=0.5, min = 0.01, max=1, step = 0.01),
    hr(),
    sectionHeader(3),
    sectionHeader(4),

    sectionHeader(5),
    sectionHeader(6),
    sectionHeader(7)
  )
  ))
