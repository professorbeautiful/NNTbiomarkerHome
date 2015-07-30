####  shinyElicit server
appName = "shinyElicit"

require("shiny")
require("xtable")
require("NNTbiomarker")

shinyServerFunction =
  function(input, output, session) {
    thisSession <<- session

    #source("conveniences.R", local=TRUE)
    source("debugTools.R", local=TRUE)
    source("contraBayesPlot.R", local=TRUE)
