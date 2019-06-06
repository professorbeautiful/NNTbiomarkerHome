appName = "shinyAE"
cat("Launching ", appName, "\n")

library(shiny)

# options(shiny.error=recover)

shinyServer(function(input, output, session) {

  thisSession <<- session
  rValues = reactiveValues()
  
  source("skislope.R", local=TRUE)
  source("AEplot.R", local=TRUE)

  rV = reactiveValues(RSclicked = 30)

  observe({
    input$ODXlow
    updateNumericInput(inputId = "RSchosen",
                       session = thisSession,
                       value = OncotypeRScutoffs[1])
  })
  observe({
    input$ODXhigh
    updateNumericInput(inputId = "RSchosen",
                       session = thisSession,
                       value = OncotypeRScutoffs[2])
  })
  observe({
    input$TailorX_low
    updateNumericInput(inputId = "RSchosen",
                       session = thisSession,
                       value = TailorXRScutoffs[1])
  })
  observe({
    input$TailorX_high
    updateNumericInput(inputId = "RSchosen",
                       session = thisSession,
                       value = TailorXRScutoffs[2])
  })
  
  observeEvent(eventExpr = input$skislope_click, handlerExpr = {
    if(is.null(input$skislope_click))
      RSarg = 49
    else RSarg = input$skislope_click$x
    RSarg = round(max(RSarg, 5))
    cat("============  Setting rV$RSclicked: ", rValues$RSclicked,
        " ======\n")
     updateNumericInput(session = thisSession, inputId = 'RSchosen', value = RSarg)

  })
  observeEvent('input$RSchosen', {
    rValues$RSselected = input$RSchosen
  })
  output$skislope = renderPlot({
    skisloplot(rValues$RSselected, input$ytop)
  })

  output$AEstack = renderPlot({
    AEplot(rValues$RSselected, makeTitle=TRUE)
  })

#   observe({
#     RSclicked = try(input$skislope_click$x)
#     if(class(RSclicked) == "try-error")
#       rV$RSclicked = 50
#     else
#       rV$RSclicked = RSclicked
#   })
})
