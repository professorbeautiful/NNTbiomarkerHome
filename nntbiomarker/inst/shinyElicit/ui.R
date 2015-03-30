#####  shinyElicit ui

stepsTable <- read.csv("SteppingStoneTable.csv")  ## TODO
#stepsTable[2,2] <- "Who are the patients you want to help? What is the clinical decision? What are the clinical options?"

shinyUI(basicPage(
  uiOutput("debugTools"),
  h1("Biomarker validation study design support"),
  hr(),
  h2("Defining the clinical scenario"),
  h3("Who are the patients you want to help? What is the clinical decision? What are the clinical options?"),
  tags$textarea(id = "who", label = "Intended beneficiaries"),
  hr(),
  h2("Principal goal"),
  h3("What NNT s for the BestToAct and BestToWait groups would make the decision clear-cut?"),
  numericInput("NNTlower", label = "NNTlower",
               value=2, min = 1, max=10, step=1),
  numericInput("NNTupper", label = "NNTupper",
               value=20, min = 10, max=100, step=1),
  hr(),
  plotOutput(outputId = "plotDiscomfort"),
  numericInput("NNTpos", label = "NNTpos, must be smaller than NNTlower",
               value=2, min = 1, step=1),
  numericInput("NNTneg", value=20, label = "NNTneg, must be larger than NNTupper", min = 1, step=1),
  numericInput("samplesize", label = "sample size",
               value=30, min = 10, max=1000, step = 1),
  numericInput("prevalence", label = "prevalence",
               value=0.5, min = 0.01, max=1, step = 0.01),
  hr()
))
