output$contraBayesPlot <- renderPlot({
  achievable.se.sp(the.prev = input$prevalence)
})

# rValues$selectedNNTPosNeg =

output$selectedNNTPosNeg = renderTable({
  hD = hoverData()
  if(is.null(hD)) NULL
  else
    xtable(digits=3, hD)
})

hoverData = reactive({
  #catn("hoverData")
  # TODO: currently assumes  axes = "pv",
  # as opposed to NNTpos, NNTneg.
  ppv = input$contraBayesPlot_hover$x
  npv = input$contraBayesPlot_hover$y
  the.prev = input$prevalence
  nnt = NNT.from.pv(ppv = ppv, npv=npv)
  sesp = sesp.from.pv(ppv = ppv, npv=npv, prev=the.prev)
  result = try(silent = TRUE, (data.frame(
    sensitivity=sesp["se"], specificity=sesp["sp"],
    PPV=ppv,
    NPV=npv,
    NNTpos=nnt[1],
    NNTneg=nnt[2]
  )))
  if(class(result) == "try-error")
    return (NULL)
  rownames(result) = "+"
  #names(result) = "value"
  result
})
#catn("is.reactive(hoverData)= ", is.reactive(hoverData))

observe({
    catn("hoverinfo ", capture.output(input$contraBayesPlot_hover))
    if(!is.null(input$contraBayesPlot_hover))
       rValues$hoverinfo <- hoverData()
})
