#require("shiny")
#require("NNTbiomarker")

appName = "shinyContraBayes"

shinyServer(function(input, output, session) {
  rValues = reactiveValues()
  source("contraBayesPlot.R", local=T)
})
