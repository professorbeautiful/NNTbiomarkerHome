#require("shiny")
#require("NNTbiomarker")

appName = "shinyContraBayes"
cat("Launching ", appName, "\n")

shinyServer(function(input, output, session) {
  rValues = reactiveValues()
  PPVderived = reactive({1/input$NNTpos})
  NPVderived = reactive({1 - 1/input$NNTneg})

  source("contraBayesPlot.R", local=T)
})
