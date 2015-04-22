require("shiny")
require("NNTbiomarker")

appName = "shinyContraBayes"

shinyServer(function(input, output, session) {
  rValues = reactiveValues()
  require("xtable")
  source("contraBayesPlot.R", local=T)
})
