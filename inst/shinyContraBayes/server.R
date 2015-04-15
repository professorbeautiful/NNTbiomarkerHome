require("shiny")
require("NNTbiomarker")

shinyServer(function(input, output, session) {
  rValues = reactiveValues()
  require("xtable")
  source("contraBayesPlot.R", local=T)
})
