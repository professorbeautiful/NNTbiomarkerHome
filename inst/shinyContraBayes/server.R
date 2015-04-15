require("shiny")
require("NNTbiomarker")

shinyServer(function(input, output) {
  require("xtable")
  source("contraBayesPlot.R", local=T)
})
